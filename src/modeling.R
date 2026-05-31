# src/modeling.R

library(tidyverse)
library(tidymodels)
library(textrecipes)
library(themis)
library(jsonlite)

source(file.path("src", "features.R"), encoding = "UTF-8")

# -----------------------------
# 1. Grouped leave-one-text-out CV
# -----------------------------

make_group_loto_cv <- function(data, group_col) {
  groups <- unique(data[[group_col]])

  splits <- map(groups, function(g) {
    assessment_idx <- which(data[[group_col]] == g)
    analysis_idx <- which(data[[group_col]] != g)

    rsample::make_splits(
      list(
        analysis = analysis_idx,
        assessment = assessment_idx
      ),
      data = data
    )
  })

  ids <- str_c("text_", groups)

  rsample::manual_rset(splits, ids)
}

# -----------------------------
# 2. Recipe: token pipelines + feature selection
# -----------------------------

step_top_cor_features <- function(
    recipe,
    ...,
    outcome,
    max_features = 30,
    role = "predictor",
    trained = FALSE,
    selected = NULL,
    removals = NULL,
    skip = FALSE,
    id = recipes::rand_id("top_cor_features")) {
  recipes::add_step(
    recipe,
    step_top_cor_features_new(
      terms = rlang::enquos(...),
      outcome = outcome,
      max_features = max_features,
      role = role,
      trained = trained,
      selected = selected,
      removals = removals,
      skip = skip,
      id = id
    )
  )
}

step_top_cor_features_new <- function(
    terms,
    outcome,
    max_features,
    role,
    trained,
    selected,
    removals,
    skip,
    id) {
  recipes::step(
    subclass = "top_cor_features",
    terms = terms,
    outcome = outcome,
    max_features = max_features,
    role = role,
    trained = trained,
    selected = selected,
    removals = removals,
    skip = skip,
    id = id
  )
}

prep.step_top_cor_features <- function(x, training, info = NULL, ...) {
  col_names <- recipes:::recipes_eval_select(x$terms, training, info)
  max_features <- min(x$max_features, length(col_names))

  if (max_features == 0 || length(col_names) == 0) {
    selected <- character()
  } else {
    selected <- calc_one_vs_rest_feature_correlations(
      data = training,
      outcome_col = x$outcome,
      feature_cols = col_names
    ) |>
      arrange(desc(mean_abs_cor), feature) |>
      slice_head(n = max_features) |>
      pull(feature)
  }

  step_top_cor_features_new(
    terms = x$terms,
    outcome = x$outcome,
    max_features = x$max_features,
    role = x$role,
    trained = TRUE,
    selected = selected,
    removals = setdiff(col_names, selected),
    skip = x$skip,
    id = x$id
  )
}

bake.step_top_cor_features <- function(object, new_data, ...) {
  cols_to_remove <- intersect(object$removals, names(new_data))

  if (length(cols_to_remove) == 0) {
    return(new_data)
  }

  new_data |>
    select(-all_of(cols_to_remove))
}

print.step_top_cor_features <- function(x, width = max(20, options()$width - 38), ...) {
  title <- if (x$trained) {
    str_c("Top ", x$max_features, " outcome-correlated feature filter kept ")
  } else {
    str_c("Top ", x$max_features, " outcome-correlated feature filter on ")
  }

  recipes:::print_step(x$selected, x$terms, x$trained, title, width)
  invisible(x)
}

tidy.step_top_cor_features <- function(x, ...) {
  terms <- if (recipes:::is_trained(x)) {
    x$selected
  } else {
    recipes:::sel2char(x$terms)
  }

  tibble(terms = terms, id = x$id)
}

make_author_recipe <- function(data, fs_name, config) {
  fs <- config$ml$feature_selection |>
    filter(.data$fs_name == !!fs_name) |>
    slice(1)

  text_col <- config$ml$text_col
  outcome_col <- config$ml$outcome_col
  max_features <- fs$max_features

  rec <- recipe(
    as.formula(str_c(outcome_col, " ~ .")),
    data = data
  ) |>
    update_role(
      row_id,
      chunk_id,
      text_id,
      author,
      title,
      filename,
      chunk_order,
      chunk_type,
      chunk_size,
      n_texts,
      n_words_text,
      n_chars_text,
      new_role = "id"
    ) |>

    # Копии текста под разные token pipelines.
    step_mutate(
      text_word_ngrams = .data[[text_col]],
      text_char_ngrams = .data[[text_col]],
      text_stopwords = .data[[text_col]]
    ) |>

    # Убираем исходную текстовую колонку из predictors после генерации признаков.
    step_rm(!!rlang::sym(text_col)) |>

    # Word n-grams.
    step_tokenize(
      text_word_ngrams,
      token = "ngrams",
      options = list(
        n = config$ml$word_ngram_max,
        n_min = config$ml$word_ngram_min
      )
    ) |>
    step_tokenfilter(
      text_word_ngrams,
      max_tokens = config$ml$max_word_ngram_features
    ) |>
    step_tfidf(text_word_ngrams) |>

    # Character n-grams.
    step_tokenize(
      text_char_ngrams,
      token = "character_shingle",
      options = list(
        n = config$ml$char_ngram_max,
        n_min = config$ml$char_ngram_min,
        strip_non_alphanum = FALSE
      )
    ) |>
    step_tokenfilter(
      text_char_ngrams,
      max_tokens = config$ml$max_char_ngram_features
    ) |>
    step_tfidf(text_char_ngrams) |>

    # Stop/function words.
    step_tokenize(
      text_stopwords,
      token = "words"
    ) |>
    step_stopwords(
      text_stopwords,
      language = "en",
      keep = TRUE
    ) |>
    step_tokenfilter(
      text_stopwords,
      max_tokens = config$ml$max_stopword_features
    ) |>
    step_tf(text_stopwords) |>

    # Балансировка авторов внутри train каждого fold.
    step_upsample(
      all_outcomes(),
      over_ratio = config$ml$upsample_over_ratio
    ) |>

    # Базовая чистка numeric-признаков.
    step_zv(all_numeric_predictors()) |>
    step_nzv(all_numeric_predictors())

  if (fs$fs_type == "corr") {
    rec <- rec |>
      step_corr(
        all_numeric_predictors(),
        threshold = fs$threshold
      ) |>
      step_top_cor_features(
        all_numeric_predictors(),
        outcome = outcome_col,
        max_features = max_features
      ) |>
      step_normalize(all_numeric_predictors())
  }

  if (fs$fs_type == "pca") {
    rec <- rec |>
      step_normalize(all_numeric_predictors()) |>
      step_pca(
        all_numeric_predictors(),
        num_comp = max_features
      )
  }

  rec
}

# -----------------------------
# 4. Модели tidymodels
# -----------------------------

make_model_spec <- function(model_name) {
  if (model_name == "multinom_glmnet") {
    return(
      multinom_reg(
        penalty = tune(),
        mixture = tune()
      ) |>
        set_engine("glmnet") |>
        set_mode("classification")
    )
  }

  if (model_name == "svm_linear") {
    return(
      svm_linear(
        cost = tune()
      ) |>
        set_engine("kernlab") |>
        set_mode("classification")
    )
  }

  if (model_name == "rand_forest") {
    return(
      rand_forest(
        mtry = tune(),
        trees = tune(),
        min_n = tune()
      ) |>
        set_engine("ranger", importance = "impurity") |>
        set_mode("classification")
    )
  }

  if (model_name == "xgboost") {
    return(
      boost_tree(
        trees = tune(),
        tree_depth = tune(),
        learn_rate = tune(),
        loss_reduction = tune(),
        min_n = tune(),
        sample_size = tune(),
        stop_iter = tune()
      ) |>
        set_engine(
          "xgboost",
          validation = 0.1
        ) |>
        set_mode("classification")
    )
  }

  if (model_name == "mlp") {
    return(
      mlp(
        hidden_units = tune(),
        penalty = tune(),
        epochs = tune()
      ) |>
        set_engine(
          "nnet",
          trace = FALSE,
          MaxNWts = 100000
        ) |>
        set_mode("classification")
    )
  }

  stop("Unknown model: ", model_name)
}

# -----------------------------
# 5. Количество признаков после recipe / feature selection
# -----------------------------

extract_feature_info <- function(fitted_workflow) {
  mold <- workflows::extract_mold(fitted_workflow)

  tibble(
    n_features = ncol(mold$predictors)
  )
}

calc_feature_count <- function(tune_result, best_config) {
  feature_counts <- collect_extracts(tune_result) |>
    filter(.config == best_config) |>
    unnest(.extracts)

  tibble(
    n_features_min = min(feature_counts$n_features),
    n_features_max = max(feature_counts$n_features),
    n_features_mean = mean(feature_counts$n_features)
  )
}

calc_one_vs_rest_feature_correlations <- function(data, outcome_col, feature_cols) {
  outcome <- factor(data[[outcome_col]])
  outcome_levels <- levels(outcome)

  map_dfr(feature_cols, function(feature) {
    values <- data[[feature]]

    if (!is.numeric(values) || length(unique(values[!is.na(values)])) < 2) {
      return(tibble(feature = feature, mean_abs_cor = 0))
    }

    class_cors <- map_dbl(outcome_levels, function(level) {
      is_class <- as.integer(outcome == level)

      if (length(unique(is_class[!is.na(is_class)])) < 2) {
        return(NA_real_)
      }

      corr <- suppressWarnings(
        cor(values, is_class, use = "pairwise.complete.obs")
      )

      abs(corr)
    })

    mean_abs_cor <- mean(class_cors, na.rm = TRUE)

    tibble(
      feature = feature,
      mean_abs_cor = if_else(is.nan(mean_abs_cor), 0, mean_abs_cor)
    )
  })
}

calc_cv_feature_correlations <- function(data, folds, outcome_col) {
  feature_cols <- names(data) |>
    purrr::keep(~ str_starts(.x, "f_"))

  if (length(feature_cols) == 0) {
    return(tibble(
      feature = character(),
      mean_abs_cor = double(),
      sd_abs_cor = double(),
      n_folds = integer()
    ))
  }

  map2_dfr(folds$splits, folds$id, function(split, fold_id) {
    rsample::analysis(split) |>
      calc_one_vs_rest_feature_correlations(
        outcome_col = outcome_col,
        feature_cols = feature_cols
      ) |>
      mutate(fold_id = fold_id)
  }) |>
    group_by(feature) |>
    summarise(
      mean_abs_cor = mean(mean_abs_cor, na.rm = TRUE),
      sd_abs_cor = sd(mean_abs_cor, na.rm = TRUE),
      n_folds = n(),
      .groups = "drop"
    ) |>
    arrange(desc(mean_abs_cor))
}

# -----------------------------
# 6. Метрики: chunk-level и text-level
# -----------------------------

get_prob_cols <- function(preds) {
  names(preds) |>
    purrr::keep(~ str_starts(.x, "\\.pred_")) |>
    purrr::discard(~ .x == ".pred_class")
}

make_text_predictions <- function(preds) {
  prob_cols <- get_prob_cols(preds)

  preds |>
    group_by(text_id, author_id) |>
    summarise(
      across(all_of(prob_cols), mean),
      .groups = "drop"
    ) |>
    pivot_longer(
      cols = all_of(prob_cols),
      names_to = "pred_author_id",
      values_to = "mean_prob"
    ) |>
    mutate(
      pred_author_id = str_remove(pred_author_id, "^\\.pred_")
    ) |>
    group_by(text_id, author_id) |>
    slice_max(mean_prob, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      pred_author_id = factor(pred_author_id, levels = levels(author_id))
    )
}

calc_chunk_metrics <- function(preds) {
  tibble(
    chunk_macro_f1 = yardstick::f_meas_vec(
      truth = preds$author_id,
      estimate = preds$.pred_class,
      estimator = "macro"
    ),
    chunk_accuracy = yardstick::accuracy_vec(
      truth = preds$author_id,
      estimate = preds$.pred_class
    )
  )
}

calc_text_metrics <- function(preds) {
  text_preds <- make_text_predictions(preds)

  tibble(
    text_macro_f1 = yardstick::f_meas_vec(
      truth = text_preds$author_id,
      estimate = text_preds$pred_author_id,
      estimator = "macro"
    ),
    text_accuracy = yardstick::accuracy_vec(
      truth = text_preds$author_id,
      estimate = text_preds$pred_author_id
    )
  )
}

# -----------------------------
# 7. Сохранение графиков диагностики
# -----------------------------

save_tuning_plot <- function(tune_result, run_id, config) {
  plots_dir <- file.path(
    config$paths$metrics_dir,
    config$paths$plots_dir
  )

  dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

  metrics <- collect_metrics(tune_result)

  p <- metrics |>
    filter(.metric %in% c("f_meas", "accuracy", "mn_log_loss")) |>
    ggplot(aes(x = .config, y = mean, group = .metric)) +
    geom_point() +
    facet_wrap(~ .metric, scales = "free_y") +
    coord_flip() +
    labs(
      title = run_id,
      x = "Tuning configuration",
      y = "CV metric"
    ) +
    theme_minimal(base_size = 10)

  plot_path <- file.path(
    plots_dir,
    str_c(run_id, ".png")
  )

  ggsave(
    filename = plot_path,
    plot = p,
    width = 10,
    height = 6,
    dpi = 150
  )

  plot_path
}

save_confusion_matrix_plot <- function(preds, run_id, config) {
  confusion_dir <- file.path(
    config$paths$metrics_dir,
    config$paths$confusion_matrix_dir
  )

  dir.create(confusion_dir, recursive = TRUE, showWarnings = FALSE)

  text_preds <- make_text_predictions(preds)
  author_levels <- levels(text_preds$author_id)

  matrix_grid <- expand_grid(
    author_id = factor(author_levels, levels = author_levels),
    pred_author_id = factor(author_levels, levels = author_levels)
  )

  matrix_counts <- text_preds |>
    mutate(
      author_id = factor(as.character(author_id), levels = author_levels),
      pred_author_id = factor(as.character(pred_author_id), levels = author_levels)
    ) |>
    count(author_id, pred_author_id, name = "n")

  matrix_data <- matrix_grid |>
    left_join(matrix_counts, by = c("author_id", "pred_author_id")) |>
    mutate(n = replace_na(n, 0L)) |>
    group_by(author_id) |>
    mutate(
      row_total = sum(n),
      row_prop = safe_div(n, row_total)
    ) |>
    ungroup()

  p <- matrix_data |>
    ggplot(aes(x = pred_author_id, y = author_id, fill = row_prop)) +
    geom_tile(color = "white", linewidth = 0.35) +
    geom_text(aes(label = n), size = 3) +
    scale_fill_gradient(
      low = "#f7fbff",
      high = "#08519c",
      labels = scales::percent_format(accuracy = 1)
    ) +
    coord_equal() +
    labs(
      title = str_c(run_id, " | text-level confusion matrix"),
      x = "Predicted author",
      y = "True author",
      fill = "Row %"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )

  plot_path <- file.path(
    confusion_dir,
    str_c(run_id, ".png")
  )

  ggsave(
    filename = plot_path,
    plot = p,
    width = 8,
    height = 7,
    dpi = 150
  )

  plot_path
}

save_feature_correlation_plot <- function(data, folds, run_id, config, top_n = 30) {
  feature_dir <- file.path(
    config$paths$metrics_dir,
    config$paths$feature_correlation_dir
  )

  dir.create(feature_dir, recursive = TRUE, showWarnings = FALSE)

  feature_correlations <- calc_cv_feature_correlations(
    data = data,
    folds = folds,
    outcome_col = config$ml$outcome_col
  )

  plot_path <- file.path(
    feature_dir,
    str_c(run_id, ".png")
  )

  if (nrow(feature_correlations) == 0) {
    p <- ggplot() +
      annotate(
        "text",
        x = 0,
        y = 0,
        label = "No precomputed f_* features found.\nRebuild chunk tables to add them.",
        size = 4
      ) +
      labs(title = str_c(run_id, " | feature correlations")) +
      theme_void()
  } else {
    p <- feature_correlations |>
      slice_max(mean_abs_cor, n = top_n, with_ties = FALSE) |>
      mutate(feature = fct_reorder(feature, mean_abs_cor)) |>
      ggplot(aes(x = mean_abs_cor, y = feature)) +
      geom_col(fill = "#2b8cbe") +
      geom_segment(
        aes(
          x = pmax(mean_abs_cor - replace_na(sd_abs_cor, 0), 0),
          xend = mean_abs_cor + replace_na(sd_abs_cor, 0),
          y = feature,
          yend = feature
        ),
        color = "#4d4d4d",
        linewidth = 0.25
      ) +
      labs(
        title = str_c(run_id, " | precomputed feature correlations"),
        subtitle = "Mean absolute one-vs-rest correlation across LOTO analysis folds",
        x = "Mean absolute correlation",
        y = NULL
      ) +
      theme_minimal(base_size = 10)
  }

  ggsave(
    filename = plot_path,
    plot = p,
    width = 9,
    height = 7,
    dpi = 150
  )

  plot_path
}

# -----------------------------
# 8. Один прогон: chunk table × FS × model
# -----------------------------

run_one_model_config <- function(chunk_path, fs_name, model_name, config) {
  set.seed(config$ml$seed)

  chunking_name <- chunk_path |>
    basename() |>
    str_remove("^chunks_") |>
    str_remove("\\.csv$")

  run_id <- str_c(
    chunking_name,
    "__",
    fs_name,
    "__",
    model_name
  )

  message("\n[START] ", run_id)

  data <- read_csv(chunk_path, show_col_types = FALSE) |>
    mutate(
      row_id = row_number(),
      author_id = factor(author_id),
      text_id = as.character(text_id)
    )

  folds <- make_group_loto_cv(
    data = data,
    group_col = config$ml$group_col
  )

  rec <- make_author_recipe(
    data = data,
    fs_name = fs_name,
    config = config
  )

  model_spec <- make_model_spec(model_name)

  wf <- workflow() |>
    add_recipe(rec) |>
    add_model(model_spec)

  metrics <- metric_set(
    accuracy,
    f_meas,
    mn_log_loss
  )

  tune_result <- tune_grid(
    wf,
    resamples = folds,
    grid = config$ml$grid_size,
    metrics = metrics,
    control = control_grid(
      save_pred = TRUE,
      save_workflow = TRUE,
      verbose = TRUE,
      allow_par = TRUE,
      extract = extract_feature_info
    )
  )

  selection_metric <- config$ml$selection_metric

  if (is.null(selection_metric)) {
    selection_metric <- "mn_log_loss"
  }

  best_metric_row <- show_best(
    tune_result,
    metric = selection_metric,
    n = 1
  ) |>
    slice(1)

  best_config <- best_metric_row$.config

  best_params <- select_best(
    tune_result,
    metric = selection_metric
  )

  feature_count <- calc_feature_count(
    tune_result = tune_result,
    best_config = best_config
  )

  preds <- collect_predictions(
    tune_result,
    parameters = best_params
  ) |>
    left_join(
      data |>
        select(
          .row = row_id,
          text_id,
          chunk_id,
          title,
          author,
          filename
        ),
      by = ".row"
    )

  chunk_metrics <- calc_chunk_metrics(preds)
  text_metrics <- calc_text_metrics(preds)

  tune_metrics <- collect_metrics(tune_result)

  selected_tune_metrics <- tune_metrics |>
    filter(.config == best_config)

  best_cv <- selected_tune_metrics |>
    filter(.metric == "f_meas") |>
    transmute(
      cv_macro_f1_mean = mean,
      cv_macro_f1_std_err = std_err
    )

  best_log_loss <- selected_tune_metrics |>
    filter(.metric == "mn_log_loss") |>
    transmute(
      cv_log_loss_mean = mean,
      cv_log_loss_std_err = std_err
    )

  plot_path <- save_tuning_plot(
    tune_result = tune_result,
    run_id = run_id,
    config = config
  )

  confusion_matrix_path <- save_confusion_matrix_plot(
    preds = preds,
    run_id = run_id,
    config = config
  )

  feature_correlation_path <- save_feature_correlation_plot(
    data = data,
    folds = folds,
    run_id = run_id,
    config = config
  )

  result_row <- tibble(
    timestamp = as.character(Sys.time()),
    run_id = run_id,
    chunking = chunking_name,
    model = model_name,
    feature_selection = fs_name,
    selection_metric = selection_metric,
    best_config = best_config,
    best_hyperparameters = as.character(jsonlite::toJSON(best_params, auto_unbox = TRUE)),
    plot_path = plot_path,
    confusion_matrix_path = confusion_matrix_path,
    feature_correlation_path = feature_correlation_path,
    error = NA_character_
  ) |>
    bind_cols(feature_count) |>
    bind_cols(best_cv) |>
    bind_cols(best_log_loss) |>
    bind_cols(chunk_metrics) |>
    bind_cols(text_metrics)

  message(
    "[DONE] ",
    run_id,
    " | text_macro_f1 = ",
    round(result_row$text_macro_f1, 4),
    " | chunk_macro_f1 = ",
    round(result_row$chunk_macro_f1, 4),
    " | n_features_mean = ",
    round(result_row$n_features_mean, 1)
  )

  result_row
}

# -----------------------------
# 9. Все 120 прогонов
# -----------------------------

combine_run_results <- function(results) {
  combined <- bind_rows(results)

  if (!"run_id" %in% names(combined)) {
    return(combined)
  }

  combined |>
    group_by(run_id) |>
    slice_tail(n = 1) |>
    ungroup()
}

get_enabled_chunk_specs <- function(config) {
  chunk_specs <- config$chunk_specs
  enabled_chunk_names <- config$enabled_chunk_names

  if (is.null(enabled_chunk_names) || length(enabled_chunk_names) == 0) {
    return(chunk_specs)
  }

  unknown_chunk_names <- setdiff(enabled_chunk_names, chunk_specs$chunk_name)

  if (length(unknown_chunk_names) > 0) {
    stop(
      "Unknown enabled_chunk_names: ",
      str_c(unknown_chunk_names, collapse = ", ")
    )
  }

  chunk_specs |>
    filter(chunk_name %in% enabled_chunk_names) |>
    mutate(chunk_order = match(chunk_name, enabled_chunk_names)) |>
    arrange(chunk_order) |>
    select(-chunk_order)
}

run_all_modeling <- function(config) {
  set.seed(config$ml$seed)

  current_selection_metric <- config$ml$selection_metric

  if (is.null(current_selection_metric)) {
    current_selection_metric <- "mn_log_loss"
  }

  metrics_dir <- config$paths$metrics_dir
  dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

  metrics_path <- file.path(
    metrics_dir,
    config$paths$metrics_file
  )

  chunk_tables_dir <- file.path(
    config$paths$output_dir,
    config$paths$chunk_tables_dir
  )

  chunk_paths <- list.files(
    path = chunk_tables_dir,
    pattern = "\\.csv$",
    full.names = TRUE
  )

  chunk_specs <- get_enabled_chunk_specs(config)
  chunk_order <- chunk_specs$chunk_name

  chunk_files <- tibble(
    chunk_path = chunk_paths,
    chunk_name = basename(chunk_paths) |>
      str_remove(str_c("^", config$paths$chunk_file_prefix)) |>
      str_remove("\\.csv$")
  )

  missing_chunk_files <- setdiff(chunk_order, chunk_files$chunk_name)

  if (length(missing_chunk_files) > 0) {
    stop(
      "Missing chunk CSV files for enabled_chunk_names: ",
      str_c(missing_chunk_files, collapse = ", "),
      ". Rebuild chunk tables first."
    )
  }

  chunk_paths <- chunk_files |>
    mutate(chunk_order = match(chunk_name, chunk_order)) |>
    filter(!is.na(chunk_order)) |>
    arrange(is.na(chunk_order), chunk_order, chunk_name) |>
    pull(chunk_path)

  if (length(chunk_paths) == 0) {
    stop(
      "No chunk CSV files found for enabled_chunk_names: ",
      str_c(chunk_order, collapse = ", ")
    )
  }

  run_grid <- expand_grid(
    model_name = config$ml$models,
    chunk_path = chunk_paths,
    fs_name = config$ml$feature_selection$fs_name
  ) |>
    mutate(
      chunking_name = basename(chunk_path) |>
        str_remove(str_c("^", config$paths$chunk_file_prefix)) |>
        str_remove("\\.csv$"),
      run_id = str_c(chunking_name, "__", fs_name, "__", model_name)
    ) |>
    select(chunk_path, fs_name, model_name, chunking_name, run_id)

  current_run_ids <- run_grid$run_id

  existing_results <- tibble()

  if (file.exists(metrics_path) && file.info(metrics_path)$size > 0) {
    existing_results <- read_csv(metrics_path, show_col_types = FALSE)

    if (!"error" %in% names(existing_results)) {
      existing_results <- existing_results |>
        mutate(error = NA_character_)
    }

    if (!"selection_metric" %in% names(existing_results)) {
      existing_results <- existing_results |>
        mutate(selection_metric = NA_character_)
    } else {
      existing_results <- existing_results |>
        mutate(selection_metric = as.character(selection_metric))
    }

    if ("best_hyperparameters" %in% names(existing_results)) {
      existing_results <- existing_results |>
        mutate(best_hyperparameters = as.character(best_hyperparameters))
    }

    if ("run_id" %in% names(existing_results)) {
      existing_results <- existing_results |>
        filter(
          run_id %in% current_run_ids,
          selection_metric == current_selection_metric
        )
    }
  }

  completed_run_ids <- character()

  if (nrow(existing_results) > 0 && all(c("run_id", "text_macro_f1", "error") %in% names(existing_results))) {
    completed_run_ids <- existing_results |>
      filter(
        !is.na(run_id),
        !is.na(text_macro_f1),
        coalesce(error, "") == ""
      ) |>
      distinct(run_id) |>
      pull(run_id)
  }

  if (length(completed_run_ids) > 0) {
    message("[SKIP] already completed runs: ", length(completed_run_ids))

    run_grid <- run_grid |>
      filter(!run_id %in% completed_run_ids)
  }

  if (is.finite(config$ml$max_runs)) {
    run_grid <- run_grid |>
      slice_head(n = config$ml$max_runs)
  }

  all_results <- if (nrow(existing_results) > 0) {
    list(existing_results)
  } else {
    list()
  }

  for (i in seq_len(nrow(run_grid))) {
    one <- run_grid[i, ]

    result <- tryCatch(
      run_one_model_config(
        chunk_path = one$chunk_path,
        fs_name = one$fs_name,
        model_name = one$model_name,
        config = config
      ),
      error = function(e) {
        message("[ERROR] ", one$chunk_path, " | ", one$fs_name, " | ", one$model_name)
        message(e$message)

        tibble(
          timestamp = as.character(Sys.time()),
          run_id = one$run_id,
          chunking = one$chunking_name,
          model = one$model_name,
          feature_selection = one$fs_name,
          selection_metric = current_selection_metric,
          best_config = NA_character_,
          best_hyperparameters = NA_character_,
          plot_path = NA_character_,
          confusion_matrix_path = NA_character_,
          feature_correlation_path = NA_character_,
          n_features_min = NA_real_,
          n_features_max = NA_real_,
          n_features_mean = NA_real_,
          cv_macro_f1_mean = NA_real_,
          cv_macro_f1_std_err = NA_real_,
          cv_log_loss_mean = NA_real_,
          cv_log_loss_std_err = NA_real_,
          chunk_macro_f1 = NA_real_,
          chunk_accuracy = NA_real_,
          text_macro_f1 = NA_real_,
          text_accuracy = NA_real_,
          error = e$message
        )
      }
    )

    all_results[[length(all_results) + 1]] <- result

    current_results <- combine_run_results(all_results)
    write_csv(current_results, metrics_path)
  }

  final_results <- combine_run_results(all_results)

  if ("text_macro_f1" %in% names(final_results) && any(!is.na(final_results$text_macro_f1))) {
    best_model <- final_results |>
      filter(!is.na(text_macro_f1)) |>
      arrange(desc(text_macro_f1), desc(chunk_macro_f1)) |>
      slice(1)

    message("\n==============================")
    message("BEST MODEL")
    message("==============================")
    print(best_model)
  } else {
    message("\nNo successful model runs yet.")
  }

  invisible(final_results)
}

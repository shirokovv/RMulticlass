# config.R

library(tidyverse)

CONFIG <- list(
  paths = list(
    text_dir = "british_fiction",
    prepared_text_dir = "british_fiction_prepared",
    metadata_path = "metadata.csv",

    output_dir = "chunk_tables",
    chunk_tables_dir = "chunks",
    debug_tables_dir = "debug",

    all_texts_index = "text_index_all_files.csv",
    used_texts_index = "text_index_used_in_tables.csv",
    author_counts = "author_counts.csv",
    dropped_authors = "dropped_authors_from_tables.csv",

    chunk_file_prefix = "chunks_",

    metrics_dir = "metrics",
    metrics_file = "model_runs.csv",
    plots_dir = "plots",
    confusion_matrix_dir = "confusion_matrices",
    feature_correlation_dir = "feature_correlations"
  ),

  metadata_delim = "\t",

  min_texts_per_author = 2,

  min_chunk_chars = 30,
  min_sentence_chars = 10,
  min_paragraph_chars = 50,

  chunk_specs = tribble(
    ~chunk_name,       ~chunk_type,   ~chunk_size,
    "paragraphs",      "paragraphs",  NA_integer_,
    "ten_parts",       "ten_parts",   10,
    "chars_3000",      "chars",       3000,
    "chars_2000",      "chars",       2000,
    "sentences_3",     "sentences",   3,
    "sentence_1",      "sentences",   1
  ),

  enabled_chunk_names = c(
    "paragraphs",
    "ten_parts"
  ),

  preprocessing = list(
    output_column = "clean_chunk_text",

    remove_numbers = TRUE,
    lemmatize = TRUE,

    min_token_chars = 1
  ),

  ml = list(
    outcome_col = "author_id",
    group_col = "text_id",
    text_col = "clean_chunk_text",

    seed = 22052026,
    grid_size = 8,
    selection_metric = "mn_log_loss",

    # Чтобы сначала быстро проверить пайплайн, можно поставить, например, 5.
    # Для полного запуска поставить Inf.
    max_runs = Inf,

    word_ngram_min = 3,
    word_ngram_max = 3,
    char_ngram_min = 3,
    char_ngram_max = 3,

    max_word_ngram_features = 150,
    max_char_ngram_features = 150,
    max_stopword_features = 50,

    upsample_over_ratio = 1,

    feature_selection = tribble(
      ~fs_name,       ~fs_type, ~threshold, ~max_features,
      "pca_loose",    "pca",    NA_real_,   30
    ),

    # Порядок важен: run_all_modeling() запускает модели именно так,
    # от обычно быстрых к самым тяжелым.
    models = c(
      "multinom_glmnet",
      "svm_linear",
      "rand_forest",
      "xgboost",
      "mlp"
    )
  )
)

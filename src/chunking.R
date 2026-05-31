# src/chunking.R

library(tidyverse)
library(stringi)

source(file.path("src", "features.R"), encoding = "UTF-8")

# -----------------------------
# 1. Базовые функции
# -----------------------------

count_words <- function(x) {
  str_count(str_squish(x), "\\S+")
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

# -----------------------------
# 2. Чтение metadata.csv
# -----------------------------

build_text_index <- function(config) {
  all_texts <- read_delim(
    file = config$paths$metadata_path,
    delim = config$metadata_delim,
    show_col_types = FALSE
  ) |>
    rename(
      text_id = textID,
      author_id = authorID
    ) |>
    mutate(
      text_id = as.character(text_id),
      source_file_path = file.path(config$paths$text_dir, filename),
      file_path = file.path(config$paths$prepared_text_dir, filename)
    ) |>
    select(
      text_id,
      author,
      author_id,
      title,
      filename,
      source_file_path,
      file_path
    ) |>
    arrange(author_id, text_id)

  author_counts <- all_texts |>
    count(author_id, author, name = "n_texts")

  used_texts <- all_texts |>
    left_join(author_counts, by = c("author_id", "author")) |>
    filter(n_texts >= config$min_texts_per_author)

  dropped_authors <- author_counts |>
    filter(n_texts < config$min_texts_per_author)

  list(
    all_texts = all_texts,
    used_texts = used_texts,
    author_counts = author_counts,
    dropped_authors = dropped_authors
  )
}

# -----------------------------
# 3. Чтение подготовленных текстов
# -----------------------------

read_prepared_texts <- function(text_index) {
  text_index |>
    mutate(
      text = map_chr(
        file_path,
        ~ readr::read_file(.x, locale = readr::locale(encoding = "UTF-8"))
      ),
      n_words_text = count_words(text),
      n_chars_text = nchar(text)
    )
}

# -----------------------------
# 4. Функции чанкинга
# -----------------------------

chunk_by_sentences <- function(text, n_sentences, min_sentence_chars) {
  sentences <- stringi::stri_split_boundaries(text, type = "sentence")[[1]] |>
    str_squish()

  sentences <- sentences[nchar(sentences) >= min_sentence_chars]

  tibble(sentence = sentences) |>
    mutate(chunk_group = ceiling(row_number() / n_sentences)) |>
    group_by(chunk_group) |>
    summarise(
      chunk = str_c(sentence, collapse = " "),
      .groups = "drop"
    ) |>
    pull(chunk)
}

chunk_by_paragraphs <- function(text, min_paragraph_chars) {
  paragraphs <- str_split(text, "\\n\\s*\\n")[[1]] |>
    str_squish()

  paragraphs[nchar(paragraphs) >= min_paragraph_chars]
}

chunk_by_chars <- function(text, n_chars) {
  text <- str_squish(text)

  starts <- seq(1, str_length(text), by = n_chars)

  map_chr(starts, function(start) {
    end <- min(start + n_chars - 1, str_length(text))
    str_sub(text, start, end)
  }) |>
    str_squish()
}

chunk_into_ten_parts <- function(text) {
  text <- str_squish(text)

  text_length <- str_length(text)

  breaks <- round(seq(1, text_length + 1, length.out = 11))

  starts <- breaks[1:10]
  ends <- breaks[2:11] - 1

  map2_chr(starts, ends, function(start, end) {
    str_sub(text, start, end)
  }) |>
    str_squish()
}

make_chunks <- function(text, chunk_type, chunk_size, config) {
  if (chunk_type == "sentences") {
    chunks <- chunk_by_sentences(
      text = text,
      n_sentences = chunk_size,
      min_sentence_chars = config$min_sentence_chars
    )
  }

  if (chunk_type == "paragraphs") {
    chunks <- chunk_by_paragraphs(
      text = text,
      min_paragraph_chars = config$min_paragraph_chars
    )
  }

  if (chunk_type == "chars") {
    chunks <- chunk_by_chars(
      text = text,
      n_chars = chunk_size
    )
  }

  if (chunk_type == "ten_parts") {
    chunks <- chunk_into_ten_parts(text)
  }

  chunks <- chunks |>
    str_squish()

  if (chunk_type != "ten_parts") {
    chunks <- chunks[nchar(chunks) >= config$min_chunk_chars]
  }

  chunks
}

# -----------------------------
# 5. Сборка таблицы чанков
# -----------------------------

build_chunk_table <- function(prepared_texts, spec, config) {
  output_column <- config$preprocessing$output_column

  prepared_texts |>
    mutate(
      "{output_column}" := map(
        text,
        make_chunks,
        chunk_type = spec$chunk_type,
        chunk_size = spec$chunk_size,
        config = config
      )
    ) |>
    select(
      text_id,
      author,
      author_id,
      title,
      filename,
      n_texts,
      n_words_text,
      n_chars_text,
      all_of(output_column)
    ) |>
    unnest_longer(all_of(output_column)) |>
    group_by(text_id) |>
    mutate(
      chunk_order = row_number()
    ) |>
    ungroup() |>
    mutate(
      chunk_id = str_c(
        text_id,
        "__",
        spec$chunk_name,
        "__",
        str_pad(chunk_order, width = 4, pad = "0")
      ),
      chunk_type = spec$chunk_type,
      chunk_size = spec$chunk_size,
      n_words_chunk = count_words(.data[[output_column]]),
      n_chars_chunk = nchar(.data[[output_column]])
    ) |>
    add_chunk_features(text_col = output_column) |>
    select(
      chunk_id,
      text_id,
      author,
      author_id,
      title,
      filename,
      chunk_order,
      chunk_type,
      chunk_size,
      all_of(output_column),
      n_words_chunk,
      n_chars_chunk,
      starts_with("f_"),
      n_texts,
      n_words_text,
      n_chars_text
    )
}

# -----------------------------
# 6. Главная функция модуля
# -----------------------------

build_all_chunk_tables <- function(config) {
  output_dir <- config$paths$output_dir

  chunk_tables_dir <- file.path(
    output_dir,
    config$paths$chunk_tables_dir
  )

  debug_tables_dir <- file.path(
    output_dir,
    config$paths$debug_tables_dir
  )

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(chunk_tables_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(debug_tables_dir, showWarnings = FALSE, recursive = TRUE)

  text_index <- build_text_index(config)

  write_csv(
    text_index$all_texts,
    file.path(debug_tables_dir, config$paths$all_texts_index)
  )

  write_csv(
    text_index$used_texts,
    file.path(debug_tables_dir, config$paths$used_texts_index)
  )

  write_csv(
    text_index$author_counts,
    file.path(debug_tables_dir, config$paths$author_counts)
  )

  write_csv(
    text_index$dropped_authors,
    file.path(debug_tables_dir, config$paths$dropped_authors)
  )

  prepared_texts <- read_prepared_texts(text_index$used_texts)

  chunk_specs <- get_enabled_chunk_specs(config)
  output_paths <- c()

  for (i in seq_len(nrow(chunk_specs))) {
    spec <- chunk_specs[i, ]

    chunk_table <- build_chunk_table(
      prepared_texts = prepared_texts,
      spec = spec,
      config = config
    )

    output_path <- file.path(
      chunk_tables_dir,
      str_c(config$paths$chunk_file_prefix, spec$chunk_name, ".csv")
    )

    write_csv(chunk_table, output_path)

    message(
      "Saved: ", output_path,
      " | chunks: ", nrow(chunk_table),
      " | texts: ", n_distinct(chunk_table$text_id),
      " | authors: ", n_distinct(chunk_table$author_id)
    )

    output_paths <- c(output_paths, output_path)
  }

  output_paths
}

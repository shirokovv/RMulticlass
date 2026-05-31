# src/preprocessing.R

library(tidyverse)
library(textstem)

# -----------------------------
# 1. Базовая нормализация текста
# -----------------------------

remove_gutenberg <- function(text) {
  text |>
    str_replace(
      regex(
        "^.*?\\*\\*\\*\\s*START OF (THE|THIS) PROJECT GUTENBERG.*?\\*\\*\\*",
        ignore_case = TRUE,
        dotall = TRUE
      ),
      ""
    ) |>
    str_replace(
      regex(
        "^.*?START OF THE PROJECT GUTENBERG EBOOK.*?\\n",
        ignore_case = TRUE,
        dotall = TRUE
      ),
      ""
    ) |>
    str_replace(
      regex(
        "\\*\\*\\*\\s*END OF (THE|THIS) PROJECT GUTENBERG.*$",
        ignore_case = TRUE,
        dotall = TRUE
      ),
      ""
    ) |>
    str_replace(
      regex(
        "END OF THE PROJECT GUTENBERG EBOOK.*$",
        ignore_case = TRUE,
        dotall = TRUE
      ),
      ""
    )
}

clean_text <- function(text) {
  text |>
    str_replace_all("\r\n|\r", "\n") |>
    str_replace_all("\uFEFF", "") |>
    remove_gutenberg() |>
    str_replace_all("[\u2018\u2019]", "'") |>
    str_replace_all("[\u201C\u201D]", "\"") |>
    str_replace_all("[\u2013\u2014]", "-") |>
    str_replace_all("[ \t]+", " ") |>
    str_replace_all(" *\n *", "\n") |>
    str_replace_all("\n{3,}", "\n\n") |>
    str_trim()
}

# -----------------------------
# 2. Предобработка текстового блока
# -----------------------------

preprocess_text <- function(text, config) {
  preprocess_text_vector(text, config)[[1]]
}

preprocess_text_vector <- function(text, config) {
  params <- config$preprocessing

  text <- replace_na(text, "")
  text <- str_squish(text)

  if (params$remove_numbers) {
    text <- str_replace_all(text, "\\d+", " ")
  }

  text <- str_replace_all(text, "\\b'|'\\b", " ")
  text <- str_squish(text)

  token_lists <- str_split(text, "\\s+")
  token_lists <- map(
    token_lists,
    \(tokens) {
      tokens <- tokens[tokens != ""]
      tokens[nchar(tokens) >= params$min_token_chars]
    }
  )

  if (params$lemmatize) {
    unique_tokens <- unique(unlist(token_lists, use.names = FALSE))

    if (length(unique_tokens) > 0) {
      lemma_map <- setNames(
        textstem::lemmatize_words(unique_tokens),
        unique_tokens
      )

      token_lists <- map(token_lists, \(tokens) unname(lemma_map[tokens]))
    }
  }

  map_chr(token_lists, str_c, collapse = " ")
}

# -----------------------------
# 3. Предобработка одного файла
# -----------------------------

preprocess_document_text <- function(text, config) {
  text <- clean_text(text)

  paragraphs <- str_split(text, "\\n\\s*\\n")[[1]]
  paragraphs <- preprocess_text_vector(paragraphs, config)
  paragraphs <- paragraphs[nchar(paragraphs) > 0]

  str_c(paragraphs, collapse = "\n\n")
}

prepare_text_file <- function(input_path, output_path, config) {
  text <- read_file(input_path, locale = locale(encoding = "UTF-8"))
  text <- preprocess_document_text(text, config)

  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  write_file(text, output_path)

  message(
    "Prepared: ",
    output_path,
    " | words: ",
    str_count(str_squish(text), "\\S+"),
    " | chars: ",
    nchar(text)
  )

  output_path
}

# -----------------------------
# 4. Главная функция модуля
# -----------------------------

prepare_all_texts <- function(config) {
  input_dir <- config$paths$text_dir
  output_dir <- config$paths$prepared_text_dir

  input_paths <- list.files(
    path = input_dir,
    pattern = "\\.txt$",
    full.names = TRUE
  )

  input_paths <- sort(input_paths)
  output_paths <- c()

  for (input_path in input_paths) {
    output_path <- file.path(output_dir, basename(input_path))

    prepared_path <- prepare_text_file(
      input_path = input_path,
      output_path = output_path,
      config = config
    )

    output_paths <- c(output_paths, prepared_path)
  }

  output_paths
}

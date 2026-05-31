# src/features.R

library(tidyverse)

# -----------------------------
# 1. Вспомогательные текстовые функции
# -----------------------------

safe_div <- function(x, y) {
  ifelse(y == 0, 0, x / y)
}

words_list <- function(x) {
  str_extract_all(str_to_lower(x), "[a-z']+")
}

word_count_vec <- function(x) {
  str_count(str_squish(x), "\\S+")
}

sentence_count_vec <- function(x) {
  map_int(stringi::stri_split_boundaries(x, type = "sentence"), length)
}

paragraph_count_vec <- function(x) {
  map_int(str_split(x, "\\n\\s*\\n"), length)
}

unique_word_count_vec <- function(x) {
  map_int(words_list(x), ~ n_distinct(.x))
}

unique_char_count_vec <- function(x) {
  map_int(str_split(x, ""), ~ n_distinct(.x))
}

mean_word_len_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)
    mean(nchar(w))
  })
}

median_word_len_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)
    median(nchar(w))
  })
}

min_word_len_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)
    min(nchar(w))
  })
}

max_word_len_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)
    max(nchar(w))
  })
}

var_word_len_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) <= 1) return(0)
    var(nchar(w))
  })
}

q25_word_len_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)
    as.numeric(quantile(nchar(w), 0.25))
  })
}

q75_word_len_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)
    as.numeric(quantile(nchar(w), 0.75))
  })
}

short_word_ratio_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)
    mean(nchar(w) <= 3)
  })
}

long_word_ratio_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)
    mean(nchar(w) >= 8)
  })
}

ttr_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)
    n_distinct(w) / length(w)
  })
}

hapax_ratio_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)

    freq <- table(w)
    sum(freq == 1) / length(w)
  })
}

word_entropy_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)

    p <- as.numeric(table(w)) / length(w)
    -sum(p * log2(p))
  })
}

char_entropy_vec <- function(x) {
  map_dbl(str_split(x, ""), function(chars) {
    chars <- chars[chars != ""]
    if (length(chars) == 0) return(0)

    p <- as.numeric(table(chars)) / length(chars)
    -sum(p * log2(p))
  })
}

repeated_word_ratio_vec <- function(x) {
  map_dbl(words_list(x), function(w) {
    if (length(w) <= 1) return(0)
    mean(w[-1] == w[-length(w)])
  })
}

mean_sentence_words_vec <- function(x) {
  sentences <- stringi::stri_split_boundaries(x, type = "sentence")

  map_dbl(sentences, function(s) {
    if (length(s) == 0) return(0)

    sent_lens <- word_count_vec(s)
    mean(sent_lens)
  })
}

median_sentence_words_vec <- function(x) {
  sentences <- stringi::stri_split_boundaries(x, type = "sentence")

  map_dbl(sentences, function(s) {
    if (length(s) == 0) return(0)

    sent_lens <- word_count_vec(s)
    median(sent_lens)
  })
}

var_sentence_words_vec <- function(x) {
  sentences <- stringi::stri_split_boundaries(x, type = "sentence")

  map_dbl(sentences, function(s) {
    if (length(s) <= 1) return(0)

    sent_lens <- word_count_vec(s)
    var(sent_lens)
  })
}

short_sentence_ratio_vec <- function(x) {
  sentences <- stringi::stri_split_boundaries(x, type = "sentence")

  map_dbl(sentences, function(s) {
    if (length(s) == 0) return(0)

    sent_lens <- word_count_vec(s)
    mean(sent_lens <= 5)
  })
}

long_sentence_ratio_vec <- function(x) {
  sentences <- stringi::stri_split_boundaries(x, type = "sentence")

  map_dbl(sentences, function(s) {
    if (length(s) == 0) return(0)

    sent_lens <- word_count_vec(s)
    mean(sent_lens >= 30)
  })
}

stopword_density_vec <- function(x) {
  stop_words <- stopwords::stopwords("en")

  map_dbl(words_list(x), function(w) {
    if (length(w) == 0) return(0)
    mean(w %in% stop_words)
  })
}

# -----------------------------
# 2. Независимые признаки чанков
# -----------------------------

add_chunk_features <- function(data, text_col) {
  data |>
    mutate(
      f_n_chars = str_length(.data[[text_col]]),
      f_n_letters = str_count(.data[[text_col]], "[A-Za-z]"),
      f_n_digits = str_count(.data[[text_col]], "\\d"),
      f_n_spaces = str_count(.data[[text_col]], "\\s"),
      f_n_lines = str_count(.data[[text_col]], "\\n") + 1,
      f_n_paragraphs = paragraph_count_vec(.data[[text_col]]),
      f_n_sentences = sentence_count_vec(.data[[text_col]]),
      f_n_words = word_count_vec(.data[[text_col]]),
      f_n_unique_words = unique_word_count_vec(.data[[text_col]]),
      f_n_unique_chars = unique_char_count_vec(.data[[text_col]]),

      f_unique_word_ratio = safe_div(f_n_unique_words, f_n_words),
      f_unique_char_ratio = safe_div(f_n_unique_chars, f_n_chars),

      f_mean_word_len = mean_word_len_vec(.data[[text_col]]),
      f_median_word_len = median_word_len_vec(.data[[text_col]]),
      f_min_word_len = min_word_len_vec(.data[[text_col]]),
      f_max_word_len = max_word_len_vec(.data[[text_col]]),
      f_var_word_len = var_word_len_vec(.data[[text_col]]),
      f_q25_word_len = q25_word_len_vec(.data[[text_col]]),
      f_q75_word_len = q75_word_len_vec(.data[[text_col]]),

      f_mean_sentence_words = mean_sentence_words_vec(.data[[text_col]]),
      f_median_sentence_words = median_sentence_words_vec(.data[[text_col]]),
      f_var_sentence_words = var_sentence_words_vec(.data[[text_col]]),

      f_short_word_ratio = short_word_ratio_vec(.data[[text_col]]),
      f_long_word_ratio = long_word_ratio_vec(.data[[text_col]]),
      f_short_sentence_ratio = short_sentence_ratio_vec(.data[[text_col]]),
      f_long_sentence_ratio = long_sentence_ratio_vec(.data[[text_col]]),

      f_ttr = ttr_vec(.data[[text_col]]),
      f_hapax_ratio = hapax_ratio_vec(.data[[text_col]]),
      f_word_entropy = word_entropy_vec(.data[[text_col]]),
      f_char_entropy = char_entropy_vec(.data[[text_col]]),

      f_stopword_density = stopword_density_vec(.data[[text_col]]),
      f_repeated_word_ratio = repeated_word_ratio_vec(.data[[text_col]]),

      f_punct_density = safe_div(str_count(.data[[text_col]], "[[:punct:]]"), f_n_chars),
      f_dot_density = safe_div(str_count(.data[[text_col]], stringr::fixed(".")), f_n_chars),
      f_comma_density = safe_div(str_count(.data[[text_col]], stringr::fixed(",")), f_n_chars),
      f_colon_density = safe_div(str_count(.data[[text_col]], stringr::fixed(":")), f_n_chars),
      f_semicolon_density = safe_div(str_count(.data[[text_col]], stringr::fixed(";")), f_n_chars),
      f_question_density = safe_div(str_count(.data[[text_col]], stringr::fixed("?")), f_n_chars),
      f_exclamation_density = safe_div(str_count(.data[[text_col]], stringr::fixed("!")), f_n_chars),
      f_quote_density = safe_div(str_count(.data[[text_col]], "\"|'"), f_n_chars),
      f_dash_density = safe_div(str_count(.data[[text_col]], "-"), f_n_chars),
      f_bracket_density = safe_div(str_count(.data[[text_col]], "\\(|\\)|\\[|\\]"), f_n_chars),
      f_ellipsis_density = safe_div(str_count(.data[[text_col]], stringr::fixed("...")), f_n_chars),

      f_uppercase_ratio = safe_div(str_count(.data[[text_col]], "[A-Z]"), f_n_letters),
      f_non_alnum_ratio = safe_div(str_count(.data[[text_col]], "[^A-Za-z0-9\\s]"), f_n_chars)
    )
}

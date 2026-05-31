# main.R

source("config.R", encoding = "UTF-8")
source(file.path("src", "features.R"), encoding = "UTF-8")
source(file.path("src", "preprocessing.R"), encoding = "UTF-8")
source(file.path("src", "chunking.R"), encoding = "UTF-8")
source(file.path("src", "modeling.R"), encoding = "UTF-8")

# prepared_texts <- prepare_all_texts(CONFIG)
# chunk_tables <- build_all_chunk_tables(CONFIG)

ml_results <- run_all_modeling(CONFIG)

print(prepared_texts)
print(chunk_tables)
print(ml_results)

# Adi Tantravahi


setwd("/Users/at34/Dropbox (Princeton)/DevFin Research Community/Adi Tantravahi/05 - Conflict and Bonds")

rm(list = ls())

# load functions
source("02 - Code/Lexis ETL/Packages.R")
source("02 - Code/Lexis ETL/Extract.R")
source("02 - Code/Lexis ETL/Transform.R")
source("02 - Code/Lexis ETL/Load.R")


key <- "S2D35QFJPXQMJJDSRSXPNJZ6FGKMRT"
secret <- "2QX5VD65J4D6R35MG7RVB5PTCVMC3G9MQ7WCXCCZ"

# load conflict search data

lexis_auth(key = key, 
           secret = secret)

lexis_extract(searchdata = conflict_keywords, 
              url = conflict_keywords$conflict_lexis_urls, 
              searchdata_id = conflict_keywords$conflict_id,
              result_id = "conflict_id")

lexis_transform(data = result_comb$Document.Content,
                orig_data = result_comb)

lexis_corpus_process(data = final_result,
                     text_var = final_result$main_text)

library(tidyverse)
library(vroom)
library(DBI)
library(odbc)
library(batch)
batch::parseCommandArgs(evaluate=TRUE)

options(tibble.width = Inf,
        readr.show_progress = FALSE)

con <- DBI::dbConnect(odbc::odbc(), driver = "PostgreSQL Unicode", 
                      database = "dbmt_results", UID = "postgres", PWD = "password", 
                      host = "localhost", port = 5432)

dbListTables(con)


## results argument is a string defining the sub-directory of /results_tbl/
## and the group of results to be inserted in the database.
## Examples: "mixed_1y_EA", "AMLMDS_1y_EA"


res_files <- dir(glue::glue("results_tbl/{results}/"))
cat("Analysis started on",
      format(Sys.time(), "%H:%M:%S %b %d %Y"),
      "\n",
      file = glue::glue("/data/DBMT_results_database/results_tbl/insert_log/{results}.log"))
  
for (res_file in res_files) {
  chr_res <- vroom(
    glue::glue("results_tbl/{results}/", res_file),
    col_types = cols(
      snp_str = col_character(),
      ALT_METAL = col_character(),
      COEF_M = col_double(),
      SE.COEF_M = col_double(),
      COEF_c1 = col_double(),
      SE.COEF_c1 = col_double(),
      COEF_c2 = col_double(),
      SE.COEF_c2 = col_double(),
      SAMP_FREQ_ALT_c1 = col_double(),
      SAMP_FREQ_ALT_c2 = col_double(),
      HetISq = col_double(),
      HetPVal = col_double(),
      PVALUE_M_nlog10 = col_double(),
      PVALUE_c1_nlog10 = col_double(),
      PVALUE_c2_nlog10 = col_double()
    )
  )
  message("\nFile ", res_file, " is imported into R.\n")
  
  chr_res <- chr_res %>%
    mutate(
      outcome_str = str_remove(res_file, "_CHR[0-9]+.res"),
      snp_id = openssl::md5(snp_str),
      outcome_id = openssl::md5(outcome_str)
    ) %>%
    select(-snp_str,-outcome_str)
  
  colnames(chr_res) <-
    tolower(gsub("[.]", "_", colnames(chr_res)))
  
  time <- system.time({
    odbc::dbWriteTable(con, "results", chr_res, append = TRUE)
  })
  
  elap_secs <- time[["elapsed"]]
  
  message(
    "\n ********************************************",
    "\n",
    str_remove(res_file, glue::glue("results_tbl/{results}/")),
    " inserted in ",
    elap_secs %/% 60,
    " minutes and ",
    elap_secs %/% 60,
    " seconds",
    "\n ********************************************"
  )
  
  cat(
    str_remove(res_file, glue::glue("results_tbl/{results}/")),
    "inserted in",
    elap_secs %/% 60,
    "minutes and",
    elap_secs %/% 60,
    "seconds\n",
    file = glue::glue(
      "/data/DBMT_results_database/results_tbl/insert_log/{results}.log"
    ),
    append = TRUE
  )
}



##### Insertion problems ######

# res_tbl <- tbl(con, "results")
# head(res_tbl)
# myids <- chr_res$snp_id[1:10]
# 
# snp_tbl <- tbl(con, "snp")
# head(snp_tbl)
# snp_tbl %>%
#   filter(chr == 2, 
#          rsid == "rs61573637")
# snp_tbl %>%
#   filter(chr==2) %>%
#   collect() -> chr2
# 
# db_snpids <- chr2$snp_id %>% 
#   str_remove_all("[-]") %>%
#   tolower()
# 
# chr_res %>%
#   filter(snp_id %in% db_snpids) %>% head
# 
# missing_ids <- setdiff(chr_res$snp_id, db_snpids)
# 
# chr_res %>%
#   filter(str_detect(snp_str, "rs61573637"))

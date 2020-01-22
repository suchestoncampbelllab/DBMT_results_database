library(tidyverse)
library(vroom)
library(DBI)
library(odbc)
library(glue)
options(readr.show_progress = FALSE)

## get arguments from command line
args=(commandArgs(TRUE))
if(length(args)==0){
  print("No arguments supplied.")
  # supply default values here if you want
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

con <- DBI::dbConnect(odbc::odbc(), driver = "PostgreSQL Unicode", 
                      database = "dbmt_results", UID = "postgres", PWD = "password", 
                      host = "localhost", port = 5432)

dbListTables(con)


## results argument is a string defining the sub-directory of /results_tbl/
## and the group of results to be inserted in the database.
## Examples: "mixed_1y_EA", "AMLMDS_1y_EA"
#results <- "AMLMDS_1y_EA"

message(glue("Result files from {results} will be processed and inserted to DB."))

res_files <- dir(glue::glue("results_tbl/{results}/"))
res_files <- res_files[which(res_files == "MM_AMLMDS_PFS_1y_EA_NA_CHR12.res"):264]



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

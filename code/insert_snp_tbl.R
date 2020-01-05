require(odbc)
require(DBI)

con <- DBI::dbConnect(odbc::odbc(),
                      driver = "PostgreSQL Unicode",
                      database = "dbmt_results",
                      UID    = "postgres",
                      PWD    = "password",
                      host = "localhost",
                      port = 5432)
dbListTables(con)


snp_tbl <- vroom::vroom("/data/DBMT_results_database/snp_tbl/clean_snp_tbl_1to22chr.tsv")
snp_tbl <- dplyr::select(
  snp_tbl,
  snp_str,
  rsid,
  chr,
  pos,
  ref,
  alt,
  typed,
  info = INFO,
  public_maf = publicMAF,
  dbmt_maf = BMT_MAF
)


for( i in sort(unique(snp_tbl$chr)) ){
  snp_tbl_chr <- snp_tbl[snp_tbl$chr == i,]
  snp_tbl_chr$snp_id <- openssl::md5(snp_tbl_chr$snp_str)
  snp_tbl_chr$snp_str <- NULL
  odbc::dbWriteTable(con, "snp", snp_tbl_chr, append=TRUE)
  print(paste("Inserted CHR", i ))
}

library(dbplyr)
library(tidyverse)
snp_tbl <- tbl(con, "snp")
snp_tbl %>% filter(
  chr == 22,
  pos > 51238349
)

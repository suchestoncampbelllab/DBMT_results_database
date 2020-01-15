library(tidyverse)
library(DBI)
library(odbc)
library(dbplyr)
options(tibble.width = Inf)

con <- DBI::dbConnect(odbc::odbc(), driver = "PostgreSQL Unicode", 
                      database = "dbmt_results", 
                      UID = "postgres", PWD = "password", 
                      host = "localhost", port = 5432)

dbListTables(con)

res_tbl <- tbl(con, "results")
snp_tbl <- tbl(con, "snp")
out_tbl <- tbl(con, "outcome")


### query for a region and one outcome

snp_tbl2 <- snp_tbl %>%
  filter(chr == 1,
         pos > 6e7,
          pos < 60.5e6)

out_tbl2 <- out_tbl %>%
  filter(genome == "R",
         outcome == "DRM", 
         censor_time == "1y",
         disease_grp == "mixed",
         ethnicity == "EA")

res2 <- snp_tbl2 %>%
  inner_join(res_tbl, by="snp_id") %>%
  semi_join(out_tbl2, by="outcome_id") %>%
  select(-snp_id, -outcome_id) %>%
  arrange(-pvalue_m_nlog10) %>%
  filter(info > 0.8,
         dbmt_maf > 0.1) 


res2_df <- collect(res2) 
nrow(res2_df)


### query for a snp and multiple outcomes

snps <- c("rs10889150", "rs568100880")
snp_tbl3 <- snp_tbl %>%
  filter(rsid %in% !!snps)

res3 <- res_tbl %>%
  semi_join(snp_tbl3) %>%
  inner_join(out_tbl)

res3_df <- collect(res3)
res3_df %>%
  select(-snp_id, -outcome_id)

### query for full genome one outcome and just pvalues

snp_tbl %>%
  inner_join(res_tbl) %>%
  semi_join(out_tbl2) %>%
  select(rsid:dbmt_maf, contains("pvalue")) %>%
  mutate(pvalue_m = 10^(-pvalue_m_nlog10)) %>%
  arrange(-pvalue_m_nlog10) -> res4
head(res4)

res4_df <- collect(res4)
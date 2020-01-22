library(tidyverse)
library(DBI)
library(odbc)
library(dbplyr)
library(batch)
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
  filter(chr == 11,
         pos > 6e7,
          pos < 6e7+10000)

out_tbl2 <- out_tbl %>%
  filter(genome == "MM",
         outcome == "PFS", 
         censor_time == "1y",
         disease_grp == "AMLMDS",
         ethnicity == "EA")

res2 <- snp_tbl2 %>%
  inner_join(res_tbl, by="snp_id") %>%
  semi_join(out_tbl2, by="outcome_id") %>%
  select(-snp_id, -outcome_id) %>%
  arrange(-pvalue_m_nlog10) %>%
  filter(info > 0.8,
         dbmt_maf > 0.1)

res2_df <- collect(res2)
res2_df <- res2_df %>%
  mutate_at(vars(starts_with("coef_")), list(hr = exp)) %>%
  mutate_at(vars(starts_with("pvalue_")), list(OG = ~10^(.) )) %>%
  mutate(ci_ub_c1 = exp(coef_c1+1.96*se_coef_c1),
         ci_ub_c2 = exp(coef_c2+1.96*se_coef_c2),
         ci_ub_m = exp(coef_m+1.96*se_coef_m),
         
         ci_lb_c1 = exp(coef_c1-1.96*se_coef_c1),
         ci_lb_c2 = exp(coef_c2-1.96*se_coef_c2),
         ci_lb_m = exp(coef_m-1.96*se_coef_m)) 

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
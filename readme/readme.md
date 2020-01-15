DBMT Results Database
================

# How to use the database

## 1\. Connect to the database.

``` r
library(tidyverse)
library(DBI)
library(odbc)

con <- DBI::dbConnect(odbc::odbc(), driver = "PostgreSQL Unicode", 
                      database = "dbmt_results", 
                      UID = "postgres", PWD = "password", 
                      host = "localhost", port = 5432)
```

## 2\. List available tables in the database.

``` r
dbListTables(con)
```

## 3\. Connect to tables in the database

``` r
res_tbl <- tbl(con, "results")
snp_tbl <- tbl(con, "snp")
out_tbl <- tbl(con, "outcome")
```

## 4\. Join tables to query results.

Use `inner_join()` to keep columns from **both** joned tables.  
Use `semi_join()` to keep columns from **only** LHS table.

### Query for a region and one outcome:

Since query is only for one outcome, use `semi_join()` and don’t enclude
columns from the *outcome\_table*. This way you will avoid adding
columns containing unnecessary repetititve rows.

``` r
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
```

### Query for couple of snps and multiple outcomes

``` r
snps <- c("rs10889150", "rs568100880")
snp_tbl3 <- snp_tbl %>%
  ## be careful using local variables!
  ## don't forget to add `!!` 
  filter(rsid %in% !!snps) 

res3 <- res_tbl %>%
  semi_join(snp_tbl3) %>%
  inner_join(out_tbl)

res3_df <- collect(res3)
res3_df %>%
  select(-snp_id, -outcome_id)
```

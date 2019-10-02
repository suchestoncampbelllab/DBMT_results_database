# create table results (
#   c1 REAL,
#   c2	REAL,
#   c3 REAL );


library(odbc)
dbListTables(con)

tibble::tribble(~c1, ~c2, ~c3,
        0.13931199,  0.02506484, -0.79125919,
        10.33094041, 234.22916199, -34.84636072,
        1, 2, Inf) -> xx


odbc::dbWriteTable(con, "results", xx, append=TRUE)

xd <- dplyr::tbl(con, "results")
xd

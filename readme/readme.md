Creating DBMT Results Database
================

# SNP table

## SNP Table Prep

Working directory on
    CCR:

    /projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC

### Data and Libraries

`Sanger_HRC_allChr_final.txt` has nul character values in `name` column,
which causes errors while character wrangling. Currently (Sep 2019) dev
version of `fread()` skips nul characters by default and used to import
the file.

``` r
### data.table dev version
# install.packages("data.table", repos="https://Rdatatable.gitlab.io/data.table")

library(data.table)
library(tidyverse)
library(openssl)

file_path <- "/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/Sanger_HRC_allChr_final.txt"
snp_tbl <- fread(file_path)
```

### Diagnostics

#### Check low MAF and Info snps

Total number of snps

``` r
nrow(snp_tbl)
```

    39131578

##### SNPs with 0 MAF

``` r
nrow(snp_tbl[BMT_MAF ==0 | BMT_MAF == 1])
```

    7247568

##### Low quality/frequency SNPs

``` r
nrow(snp_tbl[c(BMT_MAF < 0.001 | BMT_MAF > 1-0.001 | INFO < 0.3)])
```

    24418707

#### Save low quality/frequency SNPs and good SNPs in separate files

``` r
low_snps <- snp_tbl[c(BMT_MAF < 0.001 | BMT_MAF > 1-0.001 | INFO < 0.3)]
write.table(low_snps, "DBprep/low_snps.txt", sep="\t", quote = FALSE, row.names = FALSE)
```

``` r
ok_snps <- snp_tbl[!c(BMT_MAF < 0.001 | BMT_MAF > 1-0.001 | INFO < 0.3)]
write.table(ok_snps, "DBprep/ok_snps.txt", sep="\t", quote = FALSE, row.names = FALSE)
```

##### Separate `name` col for further diagnostics

``` r
ok_snps <- separate(ok_snps, name, c("chr", "pos", "rsid", "ref", "alt"), sep = "_",remove = FALSE)
```

##### SNPs with missing rsID

``` r
missing_rsid_idx <- ok_snps$rsid == "."
sum(missing_rsid_idx)
```

    555263

Keep SNPs with rsID only

``` r
good_snps <- ok_snps[!missing_rsid_idx,]
nrow(good_snps)
```

    14157608

Save good
snps

``` r
write.table(good_snps, "DBprep/good_snps.txt", sep="\t", quote = FALSE, row.names = FALSE)
```

#### Check indels

``` r
ref_indel <- nchar(good_snps$ref) > 1
sum(ref_indel)
```

``` 
0
```

``` r
alt_indel <- nchar(good_snps$alt) > 1
sum(alt_indel)
```

``` 
0
```

No indels\!

### Wrangling

1.  Change `typed` col from “TYPED” or “UNTYPED” to `TRUE` or `FALSE`  
    This is better for the database, boolean takes *much* less space
    than a 5/7 character column.
2.  Unite `name` with `typed` to create `snp_str` this column now has
    the format:  
    **“10\_98087\_rs11252127\_C\_T\_TRUE”**

NOTE: This `snp_str` column will then be hashed with MD5 algorithm
creating `snp_id` for database.

``` r
good_snps <- good_snps %>%
 mutate(
    typed = ifelse(typed == "TYPED", T, F),
    typed2 = typed) %>%
  unite(snp_str, name, typed2)
```

Save good snps ready to
hash

``` r
write.table(good_snps, "DBprep/good_snps.txt", sep="\t", quote = FALSE, row.names = FALSE)
```

### Transfer files to cloud

Send file to cloud location
`/projects/DBMT_results_database/snp_tbl/`

``` bash
scp /projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep/good_snps.txt ezgikara@199.109.192.6:/projects/DBMT_results_database/snp_tbl/
```

### Add hashed IDs

Working directory on cloud:

    /projects/DBMT_results_database/snp_tbl

See `/projects/DBMT_results_database/code/insert_snp_tbl.R` for actual
code.

``` r
snp_tbl <- vroom::vroom("good_snps.txt")
```

Following is the example of inserting CHR1 SNPs into the snp table.  
See `/projects/DBMT_results_database/code/insert_snp_tbl.R` for actual
code.

``` r
snp_tbl_chr <- snp_tbl[snp_tbl$chr == 1,]
snp_tbl_chr$snp_id <- openssl::md5(snp_tbl_chr$snp_str)
head(snp_tbl_chr)
```

``` 
  snp_str     chr    pos rsid  ref   alt   typed publicMAF BMT_MAF  INFO snp_id
  <chr>     <dbl>  <dbl> <chr> <chr> <chr> <lgl>     <dbl>   <dbl> <dbl> <chr>
1 1_752566…     1 752566 rs30… G     A     TRUE     0.820   0.821  1     eada7a…
2 1_752721…     1 752721 rs31… A     G     TRUE     0.814   0.813  1     3ce3ff…
3 1_753269…     1 753269 rs61… C     G     FALSE    0.997   0.997  0.849 5ab9b4…
4 1_753405…     1 753405 rs31… C     A     FALSE    0.853   0.850  0.945 88deac…
5 1_753541…     1 753541 rs20… G     A     FALSE    0.151   0.152  0.939 cddb52…
6 1_754063…     1 754063 rs12… G     T     FALSE    0.0409  0.0306 0.667 6a5b2f…
```

``` r
class(snp_tbl_chr$snp_id)
```

    "hash" "md5"

### Attention\!

It is very important to keep any additional `snp_str` in
`[CHR]_[POS]_[RSID]_[REF]_[ALT]_[TYPED(boolean)]` format e.g.
`10_98087_rs11252127_C_T_TRUE` \! This is how the hash is created and
this unique ID will be used as foreign key on the results table.

## Create table

Table was created on postgres side with the following SQL code:

``` sql
create table snp (
  snp_id UUID NOT NULL PRIMARY KEY,
    RSID VARCHAR(50) NOT NULL,
    CHR SMALLINT NOT NULL,
    POS INTEGER NOT NULL,
  REF CHAR(1),
  ALT CHAR(1),
  TYPED BOOLEAN,
    INFO REAL,
    public_MAF REAL,
    DBMT_MAF REAL
);
```

## Insert SNP Table

Connect to database

``` r
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
```

Drop `snp_str` column, not needed for the database and insert to `snp`
table.

``` r
snp_tbl_chr <- dplyr::select(snp_tbl_chr,
                             snp_id,
                             rsid,
                             chr,
                             pos,
                             ref, 
                             alt, 
                             typed,
                             info=INFO,
                             public_maf=publicMAF,
                             dbmt_maf = BMT_MAF)
dbWriteTable(con, "snp", snp_tbl_chr, append=TRUE)
```

# Outcome Table

## Outcome Table Prep

### Transfer files to cloud

``` bash
scp /projects/rpci/lsuchest/lsuchest/DBMT_PhenoData/DBMT_PhenoData_EA_long_allVar_20190223.txt ezgikara@199.109.192.6:/projects/DBMT_results_database/outcome_tbl/

scp /projects/rpci/lsuchest/lsuchest/DBMT_PhenoData/mismatch_phenotype_wide_100d_20190223.txt ezgikara@199.109.192.6:/projects/DBMT_results_database/outcome_tbl/
```

### Table Prep in R

See `/projects/DBMT_results_database/code/insert_outcome_tbl.R`

Working directory on cloud:

    /projects/DBMT_results_database

All code below is to wrangle the data to get it ready for database
insertion.  
It also counts the number of events and samples per cohort, disease
group etc.  
Any further insertions doesn’t need to follow the same code. Modify as
appropriate.

``` r
library(tidyverse)
pheno_rd <- vroom::vroom("outcome_tbl/DBMT_PhenoData_EA_long_allVar_20190223.txt")
pheno_mm <- vroom::vroom("outcome_tbl/mismatch_phenotype_wide_100d_20190223.txt") %>%
  drop_na(sample_type) %>%
  mutate(sample_type = "MM")

pheno_rd %>%
  bind_rows(pheno_mm) %>%
  select(sample_type, 
         cohort,
         disease,
         contains("_1Y"),
         contains("_100d"),
         -contains("S_1Y"),
         -intxsurv_1Y, -intxrel_1Y, -other_1Y,
         lfs_1Y) -> outcm_tbl0

outcm_tbl1 <- outcm_tbl0 %>%
  mutate(disease = "mixed")

outcm_tbl2 <- outcm_tbl0 %>%
  filter(disease %in% c("AML", "MDS")) %>%
  mutate(disease = "AMLMDS")

outcm_tbl0 %>%
  bind_rows(outcm_tbl1, 
            outcm_tbl2) -> outcm_tbl0

outcm_tbl <- 
  outcm_tbl0 %>%
  group_by(sample_type,
           cohort,
         disease) %>%
  summarise(n = n(), 
            OS_1y = sum(dead_1Y),
            TRM_1y = sum(TRM_1Y),
            DRM_1y = sum(disease_death_1Y),
            PFS_1y = sum(lfs_1Y),
            ORGF_1y = sum(OF_1Y),
            GVHD_1y = sum(GVHD_death_1Y),
            INF_1y = sum(infection_1Y),
            OS_100d = sum(dead_100d),
            DRM_100d = sum(drm_100d),
            TRM_100d = sum(trm_100d),
            PFS_100d = sum(lfs_100d),
            GVHD_100d = sum(gvhd_100d),
            INF_100d = sum(inf_100d),
            ORGF_100d = sum(of_100d)) %>%
  ungroup()  %>%
  gather(key=outcome, value=nevent, -(sample_type:n)) %>%
  gather("key", "value", n, nevent) %>%
  mutate(cohort= paste0("c", cohort)) %>%
  unite(key, key, cohort) %>%
  spread(key, value) %>%
  separate(outcome, c("outcome", "censor_time")) %>%
  mutate(genome = recode(sample_type,
                         donor="D",
                         recipient="R")) %>%
  mutate(ethnicity = "EA",
         pt_subset = as.character(NA)) %>%
  select(genome, outcome, disease_grp=disease, everything(), -sample_type) 
```

### Add hashed IDs

Again the order to generate `outcome_str` is very important as this will
be used to generate the unique `outcome_id`s\!

``` r
outcm_tbl %>%
  unite(outcome_str, 
        genome, disease_grp, outcome, censor_time,ethnicity, pt_subset,
        remove = FALSE) -> outcm_tbl

write_tsv(outcm_tbl, "outcome_tbl/outcome_tbl.tsv")

head(outcm_tbl)
```

| outcome\_str               | genome | outcome | disease\_grp | censor\_time | n\_c1 | n\_c2 | nevent\_c1 | nevent\_c2 | ethnicity | pt\_subset |
| :------------------------- | :----- | :------ | :----------- | :----------- | ----: | ----: | ---------: | ---------: | :-------- | :--------- |
| D\_ALL\_DRM\_100d\_EA\_NA  | D      | DRM     | ALL          | 100d         |   468 |    93 |         17 |          7 | EA        | NA         |
| D\_ALL\_DRM\_1y\_EA\_NA    | D      | DRM     | ALL          | 1y           |   468 |    93 |         91 |         19 | EA        | NA         |
| D\_ALL\_GVHD\_100d\_EA\_NA | D      | GVHD    | ALL          | 100d         |   468 |    93 |         16 |          2 | EA        | NA         |

Finally add the md5 generated hashed IDs.

``` r
outcm_tbl %>%
  mutate(outcome_id = openssl::md5(outcome_str)) %>%
  select(-outcome_str) -> outcm_tbl
```

### How columns are coded (currently)

| Columns      | Categories                         |
| :----------- | :--------------------------------- |
| genome       | D, MM, R                           |
| outcome      | DRM, GVHD, INF, ORGF, OS, PFS, TRM |
| disease\_grp | ALL, AML, AMLMDS, MDS, mixed       |
| censor\_time | 100d, 1y                           |
| ethnicity    | EA                                 |
| pt\_subset   | NA                                 |

## Create Outcome Table

``` sql
create table outcome (
  outcome_id UUID NOT NULL PRIMARY KEY,
  genome VARCHAR(2) NOT NULL,
  outcome   VARCHAR(10) NOT NULL,
  censor_time VARCHAR(10) NOT NULL,
  disease_grp VARCHAR(15) NOT NULL,
  pt_subset VARCHAR(50),
  ethnicity VARCHAR(5) NOT NULL,
  N_c1 SMALLINT,
  N_c2 SMALLINT,
  Nevent_c1 SMALLINT,
  Nevent_c2 SMALLINT
);
```

## Insert Outcome Table

``` r
require(DBI)
require(odbc)

con <- DBI::dbConnect(odbc::odbc(),
                      driver = "PostgreSQL Unicode",
                      database = "dbmt_results",
                      UID    = "postgres",
                      PWD    = "password",
                      host = "localhost",
                      port = 5432)
dbListTables(con)


dbWriteTable(con, "outcome", outcm_tbl, append=TRUE)
```

# Results Table

## Data Prep

On CCR side:

``` r
library(tidyverse)
library(vroom)

path_in <- "/projects/rpci/lsuchest/lsuchest/DBMT_results/DBMT_mixed/analyses/mixed_EA_results/out/"
path_out <- "/projects/rpci/lsuchest/lsuchest/DBMT_results/DBprep/"

res_files <- dir(path_in)

for (res_file in res_files){
  
  # string manipulation for file names and outcome_str
  res_file %>%
    str_remove(".res") %>%
    str_split(., "_") %>% .[[1]] -> outcm_splt
  
  save_file_as <- paste(
    outcm_splt[2],
    outcm_splt[3],
    outcm_splt[4],
    "1y",
    "EA",
    "NA",
    ifelse(
      as.numeric(outcm_splt[1]) < 10,
      paste0("CHR0", outcm_splt[1], ".res"),
      paste0("CHR", outcm_splt[1], ".res")),
    sep = "_"
  )
  
  # load file
  chr_res <- vroom::vroom(
    paste0(path_in, res_file),
    col_select = list(
      CHR,
      POS,
      RSID,
      REF = REF.O,
      ALT = ALT.O,
      TYPED,
      ALT_METAL = ALT,
      contains("COEF"),
      contains("PVALUE"),
      SAMP_FREQ_ALT_c1,
      SAMP_FREQ_ALT_c2,
      HetISq,
      HetPVal),
    progress = FALSE
  )
  
  # modify and save
  chr_res %>%
    mutate_at(vars(matches("PVALUE*")), list(nlog10 = ~ log10(.) * -1)) %>%
    unite(snp_str, CHR:TYPED) %>%
    select(-matches("PVALUE_*"),
           ends_with("nlog10")) %>%
    write_tsv(path = paste0(path_out, save_file_as))
  message(save_file_as, "______DONE")
}
```

### Transfer files to cloud

Send file to cloud location
`/projects/DBMT_results_database/results_tbl/`

``` bash
scp /projects/rpci/lsuchest/lsuchest/DBMT_results/DBprep/D_mixed_PFS_1y_EA_NA_CHR10.res ezgikara@199.109.192.6:/projects/DBMT_results_database/results_tbl/
```

## Create Table

``` sql
create table results (
    PRIMARY KEY (snp_id, outcome_id),
    snp_id UUID REFERENCES snp (snp_id),
    outcome_id UUID REFERENCES outcome (outcome_id),
    alt_metal CHAR(1),
  coef_c1 REAL,
  coef_c2 REAL,
  coef_m REAL,
  se_coef_c1 REAL,
  se_coef_c2 REAL,
  se_coef_m REAL,
  pvalue_c1_nlog10 REAL,
  pvalue_c2_nlog10 REAL,
  pvalue_m_nlog10 REAL,
  samp_freq_alt_c1 REAL,
  samp_freq_alt_c2 REAL,
  hetisq REAL,
  hetpval REAL);
```

## Insert Table

On cloud via R, add UUIDs and insert results.

``` r
library(tidyverse)
library(DBI)
library(odbc)
options(tibble.width = Inf)

con <- DBI::dbConnect(odbc::odbc(), driver = "PostgreSQL Unicode", 
    database = "dbmt_results", UID = "postgres", PWD = "password", 
    host = "localhost", port = 5432)

res_files <- dir("results_tbl")
res_file <- res_files[1]

chr_res <- vroom::vroom(paste0("results_tbl/", res_file))

chr_res %>% head %>%
  mutate(outcome_str = str_remove(res_file, "_CHR[0-9]+.res"))
  head

"D_mixed_DRM_1y_EA_NA_CHR01.res" %>%
  str_remove("_CHR[0-9]+.res")


openssl::md5(outcm_str)
"2ee36ecd-d90c-e373-328a-c6f7ee302d28"






outcm_tbl %>%
  unite(outcome_str, 
        genome, disease_grp, outcome, censor_time,ethnicity, pt_subset,
        remove = FALSE) -> outcm_tbl

write_tsv(outcm_tbl, "outcome_tbl.tsv")

outcm_tbl %>%
  mutate(outcome_id = openssl::md5(outcome_str)) %>%
  select(-outcome_str) -> outcm_tbl


snp_tbl <- tbl(con, "snp")
snp_tbl %>%
  filter(pos == 118527881,
         chr == 10)
```

# eQTL Table

library(tidyverse)
pheno_rd <- vroom::vroom("DBMT_PhenoData_EA_long_allVar_20190223.txt")
pheno_mm <- vroom::vroom("mismatch_phenotype_wide_100d_20190223.txt") %>%
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


head(outcm_tbl) 

outcm_tbl %>%
  unite(outcome_str, 
        genome, disease_grp, outcome, censor_time,ethnicity, pt_subset,
        remove = FALSE) %>%
  mutate(outcome_id = openssl::md5(outcome_str)) %>%
  select(-outcome_str) -> outcm_tbl



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

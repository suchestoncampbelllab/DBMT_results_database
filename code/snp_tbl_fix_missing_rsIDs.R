library(tidyverse)
options(tibble.width=Inf)
setwd("/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep")

### CHR 1-22 ####

chr1_22 <- vroom::vroom("split_snp_tbl_1to22chr.tsv")
message("CHR1-22 data loaded. \n")

miss_chr1_22 <- vroom::vroom("missingsnpRsidDict_cleaned_1to22chr.txt", n_max = 10000) %>%
  select(-INFO, -allele) %>%
  rename(INFO=info,
         name0 = name) %>%
  mutate(name = glue::glue("{chr}_{pos}_{chr}:{pos}_{ref}/{alt}_{ref}_{alt}"))
message("Missing CHR1-22 data loaded. \n")


norsid_snps <- chr1_22 %>%
  filter(!(name %in% miss_chr1_22$name0) & rsid == ".") %>%
  mutate(name = glue::glue("{chr}_{pos}_{chr}:{pos}_{ref}/{alt}_{ref}_{alt}"),
         rsid = glue::glue("{chr}:{pos}_{ref}/{alt}"))
  
clean_snptbl <- chr1_22 %>%
  filter(!name %in% miss_chr1_22$name0 & rsid != ".") %>%
  bind_rows(select(miss_chr1_22,-name0)) %>%
  bind_rows(norsid_snps) %>%
  arrange(chr, pos) %>%
  mutate(typed = ifelse(typed == "TYPED", T, F)) %>%
  unite(snp_str, name, typed, sep = "_", remove = FALSE) %>%
  select(-name)

write_tsv(clean_snptbl, "clean_snp_tbl_1to22chr.tsv")
message("Saved clean table. \n")


### CHR X ####

chrX <- vroom::vroom("split_snp_tbl_Xchr.tsv")
cat("CHR X data loaded. \n")

miss_chrX <- vroom::vroom("missingsnpRsidDict_cleaned_Xchr.txt") 

miss_chrX %>%
  select(-INFO, -allele) %>%
  rename(INFO=info,
         name0 = name) %>%
  mutate(name = glue::glue("{chr}_{pos}_{chr}:{pos}_{ref}/{alt}_{ref}_{alt}"))
cat("Missing CHR X data loaded. \n")

norsid_snps <- chrX %>%
  filter(!name %in% miss_chrX$name0 & rsid == ".") %>%
  mutate(name = glue::glue("{chr}_{pos}_{chr}:{pos}_{ref}/{alt}_{ref}_{alt}"))

chrX_snps <- chrX %>%
  filter(!name %in% miss_chrX$name0 & rsid != ".") %>%
  bind_rows(select(miss_chrX, -name0)) %>%
  bind_rows(norsid_snps) %>%
  arrange(pos) %>%
  mutate(typed = ifelse(typed == "TYPED", T, F)) %>%
  unite(snp_str, name, typed, sep = "_", remove = FALSE) %>%
  select(-name)

write_tsv(chrX_snps, "clean_snp_tbl_Xchr.tsv")
message(glue::glue("saved CHRX"))





library(tidyverse)
options(tibble.width=Inf)


### CHR 1-22 ####

chr1_22 <- vroom::vroom("split_snp_tbl_1to22chr.tsv", n_max = 10000)

miss_chr1_22 <- vroom::vroom("missingsnpRsidDict_cleaned_1to22chr.txt", n_max = 10000) %>%
  select(-INFO, -allele) %>%
  rename(INFO=info)

chr1_22 %>%
  colnames(.) %>%
  matrix(data=., nrow = 1) %>%
  write.table(., "clean_snp_tbl_1to22chr.tsv", sep="\t", row.names = FALSE, quote = FALSE, col.names = FALSE)


for(chr in 1:22){
  
  chr_snps <- filter(chr1_22, chr == chr)
  
  miss_chr_snps <- filter(miss_chr1_22, chr == chr) %>%
    rename(name0 = name) %>%
    mutate(name = glue::glue("{chr}_{pos}_{chr}:{pos}_{ref}/{alt}_{ref}_{alt}"))
    
  norsid_snps <- chr_snps %>%
    filter(!name %in% miss_chr_snps$name0 & rsid == ".") %>%
    mutate(name = glue::glue("{chr}_{pos}_{chr}:{pos}_{ref}/{alt}_{ref}_{alt}"))
  
  chr_snps <- chr_snps %>%
    filter(!name %in% miss_chr_snps$name0 & rsid != ".") %>%
    bind_rows(select(miss_chr_snps, -name0)) %>%
    bind_rows(norsid_snps) %>%
    arrange(pos) %>%
    mutate(typed = ifelse(typed == "TYPED", T, F)) %>%
    unite(snp_str, name, typed, sep = "_", remove = FALSE) %>%
    select(-name)
    
  write_tsv(chr_snps, "clean_snp_tbl_1to22chr.tsv", append = TRUE)
  message(glue::glue("saved CHR{chr}"))
  
}


### CHR X ####

chrX <- vroom::vroom("split_snp_tbl_Xchr.tsv", n_max = 10000)

miss_chrX <- vroom::vroom("missingsnpRsidDict_cleaned_Xchr.txt", n_max = 10000) %>%
  select(-INFO, -allele) %>%
  rename(INFO=info,
         name0 = name) %>%
  mutate(name = glue::glue("{chr}_{pos}_{chr}:{pos}_{ref}/{alt}_{ref}_{alt}"))

chrX %>%
  colnames(.) %>%
  matrix(data=., nrow = 1) %>%
  write.table(., "clean_snp_tbl_Xchr.tsv", sep="\t", row.names = FALSE, quote = FALSE, col.names = FALSE)

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

write_tsv(chrX_snps, "clean_snp_tbl_Xchr.tsv", append = TRUE)
message(glue::glue("saved CHRX"))





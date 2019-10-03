### data.table dev version
# install.packages("data.table", repos="https://Rdatatable.gitlab.io/data.table")

library(data.table)
library(tidyverse)
options(tibble.width = Inf)
setwd("/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep")

file_path <- "/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/Sanger_HRC_allChr_final.txt"
snp_tbl <- fread(file_path)

## low snps
low_snps <- snp_tbl[c(BMT_MAF < 0.001 | BMT_MAF > 1-0.001 | INFO < 0.3)]
write.table(low_snps, "low_snps.txt", sep="\t", quote = FALSE, row.names = FALSE)

## ok snps
ok_snps <- snp_tbl[!c(BMT_MAF < 0.001 | BMT_MAF > 1-0.001 | INFO < 0.3)]
write.table(ok_snps, "ok_snps.txt", sep="\t", quote = FALSE, row.names = FALSE)

ok_snps <- separate(ok_snps, name, c("chr", "pos", "rsid", "ref", "alt"), sep = "_",remove = FALSE)

## check indels

ref_indel <- nchar(ok_snps$ref) > 1
sum(ref_indel)

alt_indel <- nchar(ok_snps$alt) > 1
sum(alt_indel)


## missing rsID snps
missing_rsid_idx <- ok_snps$rsid == "."
sum(missing_rsid_idx)
miss_snps <- ok_snps[missing_rsid_idx,]
write_tsv(miss_snps, "missingRSID_snps.txt")

## get rsIDs back
rsid_snps <- vroom::vroom("missingsnpRsidDict_cleaned.txt")
head(rsid_snps)

ref_indel <- nchar(rsid_snps$ref) > 1
sum(ref_indel)

alt_indel <- nchar(rsid_snps$alt) > 1
sum(alt_indel)


rsid_snps <- rsid_snps %>%
  mutate(rsid_str = glue::glue("{chr}:{pos}_{ref}/{alt}"),
         typed = ifelse(typed == "TYPED", T, F)) %>%
  unite(snp_str, chr, pos, rsid_str, ref, alt, typed, remove = FALSE) %>%
  select(-INFO, -allele, -name, -rsid_str) %>%
  rename(INFO = info)

good_snps <- ok_snps[!missing_rsid_idx,]
good_snps <- good_snps %>%
  mutate(
    typed = ifelse(typed == "TYPED", T, F),
    typed2 = typed,
    chr = as.numeric(chr),
    pos = as.numeric(pos)) %>%
  unite(snp_str, name, typed2) %>%
  bind_rows(rsid_snps)

dim(good_snps)

anyDuplicated(good_snps$snp_str)

good_snps %>%
  arrange(chr, pos) %>%
  write_tsv("good_snps.txt")

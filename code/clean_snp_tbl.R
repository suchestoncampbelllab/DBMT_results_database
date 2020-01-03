### data.table dev version
# install.packages("data.table", repos="https://Rdatatable.gitlab.io/data.table")

library(data.table)
library(tidyverse)
options(tibble.width = Inf)
setwd("/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep")

all_chr <- "/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/Sanger_HRC_allChr_final.txt"
x_chr <- "/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/Sanger_HRC_chrX_final.txt"

snp_tbl <- fread(all_chr)
xchr_snp_tbl <- fread(x_chr)

separate(snp_tbl[1:5,], name, c("chr", "pos", "rsid", "ref", "alt"), sep = "_",remove = FALSE) %>%
  colnames(.) %>%
  matrix(data=., nrow = 1) -> save_cols

write.table(save_cols, "split_snp_tbl.tsv", sep="\t", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(save_cols, "missing_snps.tsv", sep="\t", row.names = FALSE, quote = FALSE, col.names = FALSE)


for(chr in 1:22){
  chr_rows <- grep(paste0(chr, "_[0-9]"), snp_tbl$name)
  chr_snp <- separate(snp_tbl[chr_rows], name, c("chr", "pos", "rsid", "ref", "alt"), sep = "_",remove = FALSE)
  miss_snp <- chr_snp[rsid == "."]
  
  write_tsv(chr_snp, "split_snp_tbl.tsv", append = TRUE)
  write_tsv(miss_snp, "missing_snps.tsv", append = TRUE)
}

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




c("10_60494_rs568182971_A_G",
  "10_60494_rs5681871_A_G",
  "10_60494_rs68182971_A_G",
  "1_60410_rs568182971_A_G",
  "1_rs5681829710_A_G
  13_1060494_rs5681829710_A_G",
  "13_60494_10:1234134_A/T_A_G") -> tx


low_info <- snp_tbl$INFO < 0.1


system.time({iris[sample(1:150, 50),]})

chr_idx <- grepl("/10_*")
chr_snptbl <- snp_tbl[]




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

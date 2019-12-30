library(data.table, lib.loc="/user/ezgikara/R/x86_64-pc-linux-gnu-library/3.5")
library(tidyverse)

setwd("/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep")
all_chr <- "/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/Sanger_HRC_allChr_final.txt"
x_chr <- "/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/Sanger_HRC_chrX_final.txt"

### CHR 1-22 ####
snp_tbl <- fread(all_chr)
separate(snp_tbl[1:5,], name, c("chr", "pos", "rsid", "ref", "alt"), sep = "_", remove = FALSE) %>%
  colnames(.) %>%
  matrix(data=., nrow = 1) -> save_cols

write.table(save_cols, "split_snp_tbl_1to22chr.tsv", sep="\t", row.names = FALSE, quote = FALSE, col.names = FALSE)
write.table(save_cols, "missing_snps_1to22chr.tsv", sep="\t", row.names = FALSE, quote = FALSE, col.names = FALSE)


for(chr in 1:22){
  chr_rows <- grep(glue::glue("^{chr}_[0-9]"), snp_tbl$name)
  chr_snp <- separate(snp_tbl[chr_rows], name, c("chr", "pos", "rsid", "ref", "alt"), sep = "_",remove = FALSE)
  miss_snp <- chr_snp[rsid == "."]
  
  write_tsv(chr_snp, "split_snp_tbl_1to22chr.tsv", append = TRUE)
  message(glue::glue("saved CHR{chr}"))
  write_tsv(miss_snp, "missing_snps_1to22chr.tsv", append = TRUE)
  message(glue::glue("saved CHR{chr} missing snps"))
}

### CHR X ####

xchr_snp_tbl <- fread(x_chr) %>%
  rename(INFO=INFP)
xchr_snp <- separate(xchr_snp_tbl, name, c("chr", "pos", "rsid", "ref", "alt"), sep = "_",remove = FALSE)
xmiss_snp <- xchr_snp[rsid == "."]


write_tsv(xchr_snp, "split_snp_tbl_Xchr.tsv")
message("Saved X chr")
write_tsv(xmiss_snp, "missing_snps_Xchr.tsv")
message("Saved X chr missing snps")

quit("no")


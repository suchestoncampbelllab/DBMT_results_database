library(data.table)
library(tidyverse)

setwd("/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/DBprep")
all_chr <- "/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/Sanger_HRC_allChr_final.txt"
x_chr <- "/projects/rpci/lsuchest/lsuchest/Rserve/BMT/genetic_data/PLINK2VCF/Sanger_HRC/Sanger_HRC_chrX_final.txt"

### CHR 1-22
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
  write_tsv(miss_snp, "missing_snps_1to22chr.tsv", append = TRUE)
}

### CHR X
xchr_snp_tbl <- fread(x_chr) %>%
  rename(INFO=INFP)
xchr_snp <- separate(xchr_snp_tbl, name, c("chr", "pos", "rsid", "ref", "alt"), sep = "_",remove = FALSE)
xmiss_snp <- xchr_snp[rsid == "."]

write_tsv(xchr_snp, "split_snp_tbl_Xchr.tsv", append = TRUE)
write_tsv(xmiss_snp, "missing_snps_Xchr.tsv", append = TRUE)



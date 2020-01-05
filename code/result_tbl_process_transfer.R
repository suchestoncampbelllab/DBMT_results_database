library(tidyverse)
library(vroom)
library(batch)
batch::parseCommandArgs(evaluate=TRUE)

# path_in <- "/projects/rpci/lsuchest/lsuchest/DBMT_results/DBMT_mixed/analyses/mixed_EA_results/out"
# path_out <- "/projects/rpci/lsuchest/lsuchest/DBMT_results/DBprep/out"
# patt <- "2_D_mixed_DRM"

message("path_in:", path_in)
message("path_out: ", path_out)
message("patt: ", patt)

res_file <- dir(path_in, glue::glue("^{patt}"), full.names = TRUE)

# string manipulation for file names and outcome_str
patt %>%
  str_split(., "_") %>% unlist() -> outcm_splt

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
    paste0("CHR", outcm_splt[1], ".res")
  ),
  sep = "_"
)
message("Output will be saved as ", save_file_as)

# load file
chr_res <- vroom::vroom(
  res_file,
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
    HetPVal
  ),
  progress = FALSE
)

message("Result file loaded.")

# modify and save
chr_res %>%
  mutate_at(vars(matches("PVALUE*")), list(nlog10 = ~ log10(.) * -1)) %>%
  unite(snp_str, CHR:TYPED) %>%
  select(-matches("PVALUE_*"),
         ends_with("nlog10")) %>% 
  write_tsv(path = paste(path_out, save_file_as, sep="/"))
message("File ", save_file_as, " is saved")


system(glue::glue("scp {path_out}/{save_file_as} ezgikara@199.109.192.37:/data/DBMT_results_database/results_tbl/"))
message("File transfer complete.")

## code to prepare `DATASET` dataset goes here
library(haven)
library(dplyr)

pm_ae <- read_sas("NCT00339183/ae.sas7bdat") %>%
  select(SUBJID, AESTDYI, AESOC) %>%
  mutate(
    AESTDYI = sample(AESTDYI),
    AESOC = sample(AESOC)
  )

usethis::use_data(pm_ae, overwrite = TRUE)

fns <- dir("NCT00119316")
fns <- fns[grep("sas7bdat", fns)]
dl <- map(fns, ~ read_sas(file.path("NCT00119316", .x)))
prefix <- gsub(".sas7bdat", "", fns)

names(dl) <- prefix

usethis::use_data(dl, overwrite = TRUE)

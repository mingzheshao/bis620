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

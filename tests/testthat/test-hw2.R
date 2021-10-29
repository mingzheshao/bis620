library(haven)
library(dplyr)
library(tibble)
library(purrr)
library(tidyr)
library(forcats)
library(ggplot2)

test_that("The fibs function works." , {
  expect_equal(fibs(6),8)
  expect_equal(fibs(1),1)
  expect_equal(fibs(2),1)
  expect_equal(fibs(11),89)
})

test_that("The MEDHXCD_label function works." , {
  expect_equal(MEDHXCD_label(3), "Resporatory")
  expect_equal(MEDHXCD_label(24), "Unknown")
})

test_that("The get_baseline function works." , {
  expect_equal(get_baseline(tibble(HGBchange_1=NULL,HGBchange_2=0,HGBchange_3=2,HGBchange_4=NULL)), 0)
  expect_equal(get_baseline(HGBchange[1,]), 10.6)
  expect_equal(get_baseline(HGBchange[91,]),NA_real_)
})



data(dl)
map(dl, ~ names(.x)[grep("TX", names(.x))])

trt <- dl$a_eendpt %>%
  select(SUBJID, TX)

attributes(dl)
attributes(dl$c_keyvar)
attributes(dl$c_disp)

dl$c_keyvar %>%
  as.data.frame() %>%
  head()

ncol(dl$c_keyvar)
table(dl$c_keyvar$SITEID)

dl$c_disp %>%
  as.data.frame() %>%
  head()

dl$c_disp[, c("DISRSP", "SUBJID")] %>%
  head(50) %>%
  as.data.frame()
aec <- dl$c_ae %>%
  mutate(SEVR =
           factor(SEVR,
                  levels = c("Mild", "Moderate", "Severe",
                             "Life threatening", "Fatal"))) %>%
  mutate(sevr_num = as.integer(SEVR)) %>%
  select(SUBJID, AGE, SEX, SEVR, AEBCTERM, STUDYDAY, AEDUR, HITERM1, sevr_num) %>%
  nest(aes = -c(SUBJID, AGE, SEX)) %>%
  mutate(num_ae = map_int(aes, nrow))
ar <- full_join(aec, dl$c_disp %>% select(SUBJID,DISRSP))

test_that("The get_worst_ae function works." , {
  expect_equal(get_worst_ae(ar$aes[[2]]), factor(c("Severe"),levels = c("Mild", "Moderate", "Severe","Life threatening", "Fatal")))
  expect_equal(get_worst_ae(ar$aes[[116]]), factor(c("Moderate"),levels = c("Mild", "Moderate", "Severe","Life threatening", "Fatal")))
})

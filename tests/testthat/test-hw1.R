test_that("The MEDHXCD_label function works." , {
  expect_equal(MEDHXCD_label(3), "Resporatory")
  expect_equal(MEDHXCD_label(24), "Unknown")
})

library(tibble)
test_that("The get_baseline function works." , {
  expect_equal(get_baseline(tibble(HGBchange_1=NULL,HGBchange_2=0,HGBchange_3=2,HGBchange_4=NULL)), 0)
  expect_equal(get_baseline(HGBchange[1,]), 10.6)
  expect_equal(get_baseline(HGBchange[91,]),NA_real_)
})

test_that("The plot_hgb function works." , {
  expect_true(inherits(plot_hgb(data.frame(STUDYWEEK=c(1,2,3,4),value=c(3.1,5.2,1.3,8.4))), "ggplot"))
})

# test_that("The get_worst_ae function works." , {
#   expect_equal(as.character(get_worst_ae(ar$aes[[2]])), "Severe")
#   expect_equal(as.character(get_worst_ae(ar$aes[[116]])), "Moderate")
# })

#' @title MEDHXCD_label
#'
#' @description This function matches medical history code to specific medical history name
#' @param x a number, medical history code
#' @return a disease
#' @examples
#' MEDHXCD_label(2)
#' @export
#'
MEDHXCD_label = function(x){
  if (x == 2){
    "Cardiovascular"
  }else if(x == 3){
    "Resporatory"
  }else if(x == 4){
    "Gastrointestinal"
  }else if(x == 5){
    "Hepatic/Biliary"
  }else if(x == 7){
    "Renal"
  }else if(x == 8){
    "Endocrine/Metabilic"
  }else if(x == 10){
    "Hematologic/Lymphatic"
  }else if(x == 11){
    "Neurologic/Psychiatric"
  }else if(x == 16){
    "Immunologic"
  }else if(x == 20){
    "Hypertension"
  }else if(x == 22){
    "Thrombosis Not Central Line"
  }else{
    "Unknown"
  }
}

#' @title get_baseline
#'
#' @description This function gets the baseline
#' @param x a
#' @return NA or a number
#' @examples
#' get_baseline(tibble(HGBchange_1=NULL,HGBchange_2=0,HGBchange_3=2,HGBchange_4=NULL))
#' @export
data(dl)
HGBchange <- dl$a_eendfu %>% select(starts_with("HGB")|TXGROUP)

get_baseline <- function(x){
  # browser()
  if (!is.na(x[1])){
    x[[1]][1]
  }else if (!is.na(x[2])){
    x[[2]][1]
  }else if (!is.na(x[3])){
    x[[3]][1]
  }else{
    x[[4]][1]
  }
}

#' @title plot_hgb
#'
#' @description This function plots the relationship between STUDYWEEK and hgbvalue
#' @param x a tibble with column "STUDYWEEK" and "value"
#' @return a ggplot object
#' @examples
#' plot_hgb(data.frame(STUDYWEEK=c(1,2,3,4),value=c(3.1,5.2,1.3,8.4))
#' @export
#'
plot_hgb <- function(x) {
  ggplot(x, aes(x = STUDYWEEK, y = value)) +
    geom_line() +
    geom_point() +
    theme_minimal()+
    labs(x="Study Week", y="HGB Value")
}


#' @title get_worst_ae
#'
#' @description This function gets the worst adverse event through the whole process
#' @param x a
#' @return "Mild", "Moderate", "Severe", "Life threatening", "Fatal"
#' @examples
#' get_worst_ae(ar$aes[[116]])
#' @export
#'
data(dl)
library(purrr)
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

get_worst_ae <- function(x) {
 na_ret <- factor(NA,
                  levels = c("Mild", "Moderate", "Severe",
                             "Life threatening", "Fatal"))
 if (is.null(x) || nrow(x) == 0) {
   return(na_ret)
 }
 ret <- x$SEVR[which.max(as.integer(x$SEVR))]
 if (length(ret)  == 0) {
   ret <- na_ret
 }
ret
}

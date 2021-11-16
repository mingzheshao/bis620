library(haven)
library(dplyr)
library(tibble)
library(purrr)
library(tidyr)
library(forcats)
library(ggplot2)

#' @title fibs
#'
#' @description This function gets Fibonacci sequence
#' @param n a number
#' @return 1,1,2,3,5,8,13...
#' @export
fibs <- function(n){
  if(n==1 | n==2){
    return(1)
  }
  else{
    return(fibs(n-1)+fibs(n-2))
  }
}


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

#' @title get the baseline
#' @name get_baseline
#' @description This function gets the baseline
#' @param x a
#' @return NA or a number
#' @importFrom dplyr select
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

#' @title plot the hgb
#' @name plot_hgb
#' @description This function plots the relationship between STUDYWEEK and hgbvalue
#' @param x a tibble with column "STUDYWEEK" and "value"
#' @return a ggplot object
#' @importFrom ggplot2 labs
#' @export
#'
library(ggplot2)
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
#' @export
#'
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

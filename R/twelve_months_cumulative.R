#' Twelve months cumulative variation
#'
#' @description Computes twelve months cumulative variation of a variable.
#'
#' @param x input data set
#' @param time time variable
#' @param percent if true, the result is multiplied by 100
#'
#' @export
#'
#' @examples
#' library(lubridate)
#' x <- data.frame( date = seq(as.Date("1910/1/1"), as.Date("1920/1/1"), "months"), x = seq(1:121))
#' twelve_months_cumulative(x$x, time =  x$date)
twelve_months_cumulative <- function(x, time, percent = F){
if(isTRUE(percent)) {percent = 100} else{percent = 1}

    percent * (
      slider::slide_dbl(x, sum, .before = 11, .complete = T)/
      (
        slider::slide_dbl(x, sum, .before = 23, .complete = T) - slider::slide_dbl(x, sum, .before = 11, .complete = T)
      ) -1
              )


}


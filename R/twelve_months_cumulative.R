#' twelve_months_cumulative
#'
#' @param x
#' @param time
#' @param percent
#'
#' @return
#' @export
#'
#' @examples
#' library(lubridate)
#' x <- data.frame( date = seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years"), x = seq(1:90))
#' twelve_months_cumulative(x$x, time =  x$date)
#'
#'
twelve_months_cumulative <- function(x, time, percent = F){
if(isTRUE(percent)) {percent = 100} else{percent = 1}

    percent * (slider::slide_dbl(x, sum, .before = 11, .complete = T)/
    statar::tlag(slider::slide_dbl(x, sum, .before = 11, .complete = T),months(12), time = time )-1)


}


#' Variation
#'
#' @param x input data set
#' @param n a positive integer of length 1 or period given by lubridate package. See statar and lubridate packages
#' @param time time variable
#' @param percent if true, the result is multiplied by 100
#'
#' @export
#'
#' @examples
#' library(lubridate)
#' x <- data.frame( date = seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years"), x = seq(1:90))
#' variation(x$x, n = years(1), time =  x$date)
#'
variation <- function(x, n, time, percent = F) {
  default = NA
  if (!is.numeric(n) | (length(n)>1)) stop("n must be a numeric of length one")
  if (dplyr::n_distinct(time) < length(time)) stop("time has duplicate elements")
  if(isTRUE(percent)) {percent = 100} else{percent = 1}



  out <- percent*((x / statar::tlag(x, n, time, default = NA)) - 1)


  if (!is.na(default)) out[which(is.na(index))] <- default
  attributes(out) <- attributes(x)
  out


}

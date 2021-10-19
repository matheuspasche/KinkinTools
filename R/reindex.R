#' Reindex
#'
#' @description This function is intended to change the reference date of an index variable.
#'
#' @param x input data set.
#' @param time time variable
#' @param base new base. Must be date format.
#'
#' @export
#'
#' @examples
#' library(lubridate)
#' x <- data.frame( date = seq(as.Date("1910/1/1"), as.Date("1999/1/1"), "years"), x = seq(1:90))
#' reindex(x$x, time =  x$date, base = "1985-01-01")
#'
#'
#'
reindex <- function(x, time, base){

    tibble::tibble({{x}}, {{time}}) %>%
    dplyr::mutate(reindex = dplyr::case_when({{time}} == {{base}} ~ {{x}}),
                  reindex = max(reindex, na.rm = T),
                  reindex = 100*({{x}}/reindex)) %>%
    dplyr::pull(reindex)
}

#' Title
#' @description
#' @param x
#' @param time
#' @param percent
#'
#' @return
#' @export
#'
#' @examples
year_cumulative <- function(x, time, percent = F){
if(isTRUE(percent)) {percent = 100} else{percent = 1}


    tibble::tibble({{x}}, {{time}}) %>%
    dplyr ::group_by(year = lubridate::year({{time}})) %>%
    dplyr ::mutate({{x}} := cumsum({{x}})) %>%
    dplyr ::ungroup() %>%
    dplyr ::mutate({{x}} := percent * (variation({{x}}, n = months(12), time =  {{time}} ))) %>%
    dplyr ::pull({{x}})
 }

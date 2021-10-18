reindex <- function(x, time, base){

    tibble::tibble({{x}}, {{time}}) %>%
    dplyr::mutate(reindex = dplyr::case_when({{time}} == {{base}} ~ {{x}}),
                  reindex = max(reindex, na.rm = T),
                  reindex = 100*({{x}}/reindex)) %>%
    dplyr::pull(reindex)
}




assess_pH <- function(df){

  criteria = 8.5

  df_pH <- df %>%
    dplyr::filter(parameter == "pH") %>%
    dplyr::mutate(excursion = ifelse(result > criteria, result/criteria, NA),
                  beyond_limit = ifelse(result > criteria, 1, 0))

  df_summary <- df_pH %>%
    dplyr::group_by(location_id) %>%
    dplyr::summarise(n = dplyr::n(),
                     n_beyond_limit = sum(beyond_limit),
                     excursion = sum(excursion, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nse = excursion / n,
                  f2 = n_beyond_limit / n * 100,
                  f3 = nse / (0.01 * nse + 0.01),
                  analyte_wqi = 100 - (sqrt((f2^2)+(f3^2))/1.414))





}

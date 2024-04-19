


assess_pH <- function(df, parameter_name, max_criteria = NULL, min_criteria = NULL){


  df_pH <- df %>%
    dplyr::filter(parameter == parameter_name) %>%
    dplyr::mutate(excursion = NA,
                  outside_limit = NA)


  for(i in 1:nrow(df_pH)){
    if(!is.null(max_criteria) ){
      if(df_pH$result[i] > max_criteria){
        df_pH[i, 'excursion'] = (df_pH$result[i]/max_criteria)-1
        df_pH[i, 'outside_limit'] = 1
      }
    }
    if(!is.null(min_criteria) ){
      if(df_pH$result[i] < min_criteria){
        df_pH[i, 'excursion'] = (min_criteria/df_pH$result[i])-1
        df_pH[i, 'outside_limit'] = 1
      }
    }
  }

  df_pH <- df_pH %>%
    tidyr::replace_na(list(outside_limit = 0))



  df_summary <- df_pH %>%
    dplyr::group_by(location_id) %>%
    dplyr::summarise(n = dplyr::n(),
                     n_outside_limit = sum(outside_limit),
                     excursion = sum(excursion, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nse = excursion / n,
                  f2 = n_outside_limit / n * 100,
                  f3 = nse / (0.01 * nse + 0.01),
                  analyte_wqi = 100 - (sqrt((f2^2)+(f3^2))/1.414))



  return(df_summary)


}

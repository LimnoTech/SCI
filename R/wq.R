


assess_wq <- function(df, parameter_name, max_criteria = NULL, min_criteria = NULL){


  df_wq <- df %>%
    dplyr::filter(parameter == parameter_name) %>%
    dplyr::mutate(excursion = NA,
                  outside_limit = NA)


  for(i in 1:nrow(df_wq)){
    if(!is.null(max_criteria) ){
      if(df_wq$result[i] > max_criteria){
        df_wq[i, 'excursion'] = (df_wq$result[i]/max_criteria)-1
        df_wq[i, 'outside_limit'] = 1
      }
    }
    if(!is.null(min_criteria) ){
      if(df_wq$result[i] < min_criteria){
        df_wq[i, 'excursion'] = (min_criteria/df_wq$result[i])-1
        df_wq[i, 'outside_limit'] = 1
      }
    }
  }

  df_wq <- df_wq %>%
    tidyr::replace_na(list(outside_limit = 0))



  df_summary <- df_wq %>%
    dplyr::group_by(location_id) %>%
    dplyr::summarise(n_tests = dplyr::n(),
                     n_tests_failed = sum(outside_limit),
                     excursion = sum(excursion, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nse = excursion / n_tests,
                  f2 = n_tests_failed / n_tests * 100,
                  f3 = nse / (0.01 * nse + 0.01),
                  wqi = 100 - (sqrt((f2^2)+(f3^2))/1.414))



  return(df_summary)


}

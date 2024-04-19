


assess_wq <- function(df, parameter_name, unit, max_criteria = NULL, min_criteria = NULL){


  df_results <- df %>%
    dplyr::filter(parameter == parameter_name,
                  unit == unit) %>%
    dplyr::mutate(excursion = NA,
                  outside_limit = NA)


  for(i in 1:nrow(df_results)){
    if(!is.null(max_criteria) ){
      if(df_results$result[i] > max_criteria){
        df_results[i, 'excursion'] = (df_results$result[i]/max_criteria)-1
        df_results[i, 'outside_limit'] = 1
      }
    }
    if(!is.null(min_criteria) ){
      if(df_results$result[i] < min_criteria){
        df_results[i, 'excursion'] = (min_criteria/df_results$result[i])-1
        df_results[i, 'outside_limit'] = 1
      }
    }
  }

  df_results <- df_results %>%
    tidyr::replace_na(list(outside_limit = 0))



  df_summary <- df_results %>%
    dplyr::group_by(location_id) %>%
    dplyr::summarise(n_tests = dplyr::n(),
                     n_tests_failed = sum(outside_limit),
                     excursion = sum(excursion, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(nse = excursion / n_tests,
                  f2 = n_tests_failed / n_tests * 100,
                  f3 = nse / (0.01 * nse + 0.01),
                  wqi = 100 - (sqrt((f2^2) + (f3^2)) / 1.414),
                  score = wqi / 10)


  df_score <- df_summary %>%
    dplyr::select(location_id, score)



  return(list(results = df_results, summary = df_summary, score = df_score))


}

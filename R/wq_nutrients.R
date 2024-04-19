


assess_wq_nutrients <- function(df, parameter_name, piedmont_criteria, coastal_plain_criteria){


  df_results <- df %>%
    dplyr::filter(parameter == parameter_name) %>%
    dplyr::left_join(location, by = "location_id") %>%
    dplyr::mutate(excursion = NA,
                  outside_limit = NA)


  for(i in 1:nrow(df_results)){

    ecoregion <- df_results$ecoregion[i]


    if(!is.na(ecoregion)) {
      if(df_results$ecoregion[i] == "Piedmont") {
        if(df_results$result[i] > piedmont_criteria){
          df_results[i, 'excursion'] = (df_results$result[i]/piedmont_criteria)-1
          df_results[i, 'outside_limit'] = 1
        } else {
          df_results[i, 'outside_limit'] = 0
        }
      } else if(ecoregion == "Coastal Plain") {
        if(df_results$result[i] > coastal_plain_criteria){
          df_results[i, 'excursion'] = (df_results$result[i]/coastal_plain_criteria)-1
          df_results[i, 'outside_limit'] = 1
        } else {
          df_results[i, 'outside_limit'] = 0
        }
      }
    } else {
      df_results[i, 'outside_limit'] = NA
    }
  }


  df_summary <- df_results %>%
    dplyr::group_by(location_id) %>%
    dplyr::summarise(n_tests = dplyr::n(),
                     n_tests_failed = sum(outside_limit),
                     excursion = sum(excursion, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(excursion = dplyr::case_when(is.na(n_tests_failed) ~ NA)) %>%   # Clear out values when ecoregion not set for a given location
    dplyr::mutate(nse = excursion / n_tests,
                  f2 = n_tests_failed / n_tests * 100,
                  f3 = nse / (0.01 * nse + 0.01),
                  wqi = 100 - (sqrt((f2^2)+(f3^2))/1.414),
                  score = wqi / 10)

  df_score <- df_summary %>%
    dplyr::select(location_id, score)


  return(list(results = df_results, summary = df_summary, score = df_score))


}

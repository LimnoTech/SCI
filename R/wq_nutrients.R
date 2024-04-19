


assess_wq_nutrients <- function(df, parameter_name, piedmont_criteria, coastal_plain_criteria){


  df_wq <- df %>%
    dplyr::filter(parameter == parameter_name) %>%
    dplyr::left_join(location, by = "location_id") %>%
    dplyr::mutate(excursion = NA,
                  outside_limit = NA)


  for(i in 1:nrow(df_wq)){

    ecoregion <- df_wq$ecoregion[i]


    if(!is.na(ecoregion)) {
      if(df_wq$ecoregion[i] == "Piedmont") {
        if(df_wq$result[i] > piedmont_criteria){
          df_wq[i, 'excursion'] = (df_wq$result[i]/piedmont_criteria)-1
          df_wq[i, 'outside_limit'] = 1
        } else {
          df_wq[i, 'outside_limit'] = 0
        }
      } else if(ecoregion == "Coastal Plain") {
        if(df_wq$result[i] > coastal_plain_criteria){
          df_wq[i, 'excursion'] = (df_wq$result[i]/coastal_plain_criteria)-1
          df_wq[i, 'outside_limit'] = 1
        } else {
          df_wq[i, 'outside_limit'] = 0
        }
      }
    } else {
      df_wq[i, 'outside_limit'] = NA
    }
  }


  df_summary <- df_wq %>%
    dplyr::group_by(location_id) %>%
    dplyr::summarise(n_tests = dplyr::n(),
                     n_tests_failed = sum(outside_limit),
                     excursion = sum(excursion, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(excursion = dplyr::case_when(is.na(n_tests_failed) ~ NA)) %>%   # Clear out values when ecoregion not set for a given location
    dplyr::mutate(nse = excursion / n_tests,
                  f2 = n_tests_failed / n_tests * 100,
                  f3 = nse / (0.01 * nse + 0.01),
                  wqi = 100 - (sqrt((f2^2)+(f3^2))/1.414))



  return(df_summary)


}

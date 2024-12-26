#' Calculate water quality scores
#'
#' @param df DOEE Ambient Water Quality Data that has been processed and formatted
#' @param parameter_name String to indicate target parameter: "Conductivity",
#'     "Dissolved oxygen (DO)", "Escherichia coli", "pH", "Temperature, water",
#'     "Turbidity"
#' @param unit String to indicate unit for water quality parameter
#' @param max_criteria DC water quality criteria as a maximum value
#' @param min_criteria DC water quality criteria as a minimum value
#'
#' @return List of three dataframes: results, summary, score
#' @export
#'
#' @examples assess_wq(df_wq_processed,
#'                     parameter_name = "Temperature, water",
#'                     unit = "deg C",
#'                     max_criteria = 24)
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
    dplyr::left_join(location_id, by = "location_id") %>%
    dplyr::left_join(location_name, by = "location_name") %>%
    dplyr::group_by(sci_subshed) %>%
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
    dplyr::select(sci_subshed, score) %>%
    dplyr::rename(!!parameter_name := score)



  return(list(results = df_results, summary = df_summary, score = df_score))


}

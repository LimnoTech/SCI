#' Calculate scores for effective impervious area (EIA)
#'
#' @return List of 2 dataframes: summary and score.
#' @export
#'
#' @examples assess_eia()
assess_eia <- function() {


  df_subsheds <- eia_subsheds %>%
    dplyr::group_by(sci_subshed) %>%
    dplyr::summarise(subshed = sum(analysis_subshed_m2),
                     impervious = sum(analysis_impervious_m2),
                     impervious_treated = sum(analysis_impervious_treated_m2))

  df_rock_creek <- eia_rock_creek %>%
    dplyr::group_by(sci_subshed) %>%
    dplyr::summarise(subshed = sum(analysis_subshed_m2),
                     impervious = sum(analysis_impervious_m2),
                     impervious_treated = sum(analysis_impervious_treated_m2))


  df_summary <- df_subsheds %>%
    dplyr::union(df_rock_creek) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(effective_impervious = impervious - impervious_treated,
                  percent_effective_impervious = effective_impervious / subshed * 100,
                  score = assign_score_from_maximum(percent_effective_impervious, eia_score, "max_eia_percent"))


  df_score <- df_summary %>%
    dplyr::select(sci_subshed,
                  `Effective Impervious Area` = score)


  return(list(summary = df_summary, score = df_score))


}

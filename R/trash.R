#' Calculate trash scores.
#'
#' @param df_reach Reach data from Rapid Stream Assessment (RSA) dataset.
#' @param start_date Beginning of timeframe being evaluated (ex. first day of
#'     five-year assessment period).
#' @param end_date End of timeframe being evaluated (ex. last day of
#'     five-year assessment period).
#' @param reach_prefix_from_table Field name prefix sourced from RSA reach
#'     attribute table. Should be consistent with the name of the stream reach
#'     attribute table from the RSA geodatabase.
#' @param reach_prefix_from_layer Field name prefix sourced from RSA stream
#'     reach layer. Should be consistent with the name of the stream reach
#'     polyline layer from the RSA geodatabase.
#'
#' @return List of 2 dataframes: summary and score.
#' @export
#'
#' @examples assess_trash(df_reach_example, "2020-01-01", "2024-12-31",
#'     "StreamReachAttributes", "StreamReaches")


assess_trash <- function(df_reach, start_date, end_date, reach_prefix_from_table, reach_prefix_from_layer) {

  df_summary <- df_reach %>%
    dplyr::select(location_name = dplyr::all_of(paste0(reach_prefix_from_layer, ".subshed")),
                  id = dplyr::all_of(paste0(reach_prefix_from_table, ".featureGlobalID_key")),
                  date = dplyr::all_of(paste0(reach_prefix_from_table, ".assessment_time")),
                  trash_extent = dplyr::all_of(paste0(reach_prefix_from_table, ".trash_extent"))) %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
    dplyr::group_by(id) %>%
    dplyr::slice_max(date, n=1, with_ties = FALSE) %>% # Remove duplicate reaches. Keep most recent reach assessment. If same date/time and ID, maintain only one record
    dplyr::ungroup() %>%
    dplyr::left_join(trash_score, by = "trash_extent") %>%
    dplyr::left_join(location_name, by = "location_name") %>%
    dplyr::filter(!is.na(trash_extent))   # Remove data records with no trash extent entry


  df_score <- df_summary %>%
    dplyr::group_by(sci_subshed) %>%
    dplyr::summarise(score = mean(score)) %>%
    tidyr::drop_na(sci_subshed)



  df_score <- df_score %>%
    dplyr::select(sci_subshed,
                  Trash = score)


  return(list(summary = df_summary, score = df_score))


}

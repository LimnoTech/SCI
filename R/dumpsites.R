#' Assign score based on lookup on maximum number of dumpsites per mile
#'
#' @param value Number of dumpsites per mile for a given subwatershed.
#' @param lookup_table Lookup table of number of dumpsites per mile allowed per
#'     score (1-10).
#' @param lookup_field Column name containing the maximum number of sites per
#'     mile.
#'
#' @return Numerical score (1-10)
#' @export
#'
#' @examples assign_score_from_maximum(sites_per_mile, dumpsite_score, "max_sites_per_mile")

assign_score_from_maximum <- function(value, lookup_table, lookup_field) {

  # Make sure dumpsite scores are in descending order
  lookup_table <- lookup_table[order(-lookup_table$score),]

  score <- NA
  for (i in 1:nrow(lookup_table)) {
    max <- lookup_table[[lookup_field]][i]
    if(!is.na(max) && !is.null(max)) {
      if (value <= max) {
        score <- lookup_table$score[i]
        break
      } else {
        score = 1
      }
    }

  }
  return(score)
}

#' Calculate dumpsite scores
#'
#' @param df_point Point data from Rapid Stream Assessment (RSA) dataset.
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
#' @param point_prefix_from_table Field name prefix sourced from RSA point
#'     attribute table. Should be consistent with the name of the point
#'     assessment attribute table from the RSA geodatabase.
#' @param point_prefix_from_layer Field name prefix sourced from RSA point
#'     layer. Should be consistent with the name of the intersected stream point
#'     layer from the RSA geodatabase.
#'
#' @return List of 2 dataframes: summary and score.
#' @export
#'
#' @examples assess_dumpsites(df_point, df_reach, "2020-01-01", "2024-12-31",
#'     "StreamReachAttributes", "StreamReaches",
#'     "StreamPointAttributes", "StreamPoints_Intersect" )

assess_dumpsites <- function(df_point, df_reach, start_date, end_date, reach_prefix_from_table, reach_prefix_from_layer, point_prefix_from_table, point_prefix_from_layer){

  df_length <- df_reach %>%
    dplyr::select(location_name = dplyr::all_of(paste0(reach_prefix_from_layer, ".subshed")),
                  reach_length = dplyr::all_of(paste0(reach_prefix_from_layer, ".Shape_Length")),
                  id = dplyr::all_of(paste0(reach_prefix_from_table, ".featureGlobalID_key")),
                  date = dplyr::all_of(paste0(reach_prefix_from_table, ".assessment_time"))) %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
    dplyr::group_by(id) %>%
    dplyr::slice_max(date, n=1, with_ties = FALSE) %>% # Remove duplicate reaches. Keep most recent reach assessment. If same date/time and ID, maintain only one record
    dplyr::ungroup() %>%
    dplyr::left_join(location_name, by = "location_name") %>%
    dplyr::group_by(sci_subshed) %>%
    dplyr::summarise(subshed_length_meters = sum(reach_length))

  df_dump <- df_point %>%
    dplyr::select(location_name = dplyr::all_of(paste0(point_prefix_from_layer, ".SUBSHED")),
                  assessment_type = dplyr::all_of(paste0(point_prefix_from_table, ".assessment_type")),
                  date = dplyr::all_of(paste0(point_prefix_from_table, ".assessment_time")),
                  d_impact = dplyr::all_of(paste0(point_prefix_from_table, ".d_impact")),
                  ) %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
    dplyr::group_by(id) %>%
    dplyr::slice_max(date, n=1, with_ties = FALSE) %>% # Remove duplicate point. Keep most recent point assessment. If same date/time and ID, maintain only one record
    dplyr::ungroup() %>%
    dplyr::left_join(location_name, by = "location_name") %>%
    dplyr::filter(assessment_type == "dumpsite") %>%
    dplyr::left_join(dumpsite_weight, by = "d_impact") %>%
    tidyr::drop_na(sci_subshed)



  df_summary <- df_dump %>%
    dplyr::group_by(sci_subshed) %>%
    dplyr::summarise(dumpsite_count = dplyr::n(),
                     applied_influence = min(score_influence)) %>%  # Use most conservative
    dplyr::left_join(df_length, by = "sci_subshed") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(subshed_length_miles = subshed_length_meters / 1609.344,
                  sites_per_mile = dumpsite_count / subshed_length_miles,
                  score = assign_score_from_maximum(sites_per_mile, dumpsite_score, "max_sites_per_mile"),
                  score_weighted = score + applied_influence) %>%
    dplyr::mutate(score_weighted = dplyr::case_when(score_weighted < 1 ~ 1,
                                   TRUE ~ score_weighted)) # Score cannot be less than 1


  sheds <- unique(location_name$sci_subshed)
  df_sheds <- data.frame(sci_subshed = sheds)

  df_score <- df_summary %>%
    dplyr::select(sci_subshed,
           Dumpsites = score_weighted) %>%
    dplyr::right_join(df_sheds, by = "sci_subshed") %>%
    dplyr::mutate(Dumpsites = tidyr::replace_na(Dumpsites, 10)) # Any sci_subshed without dumpsite data gets a score of 10

  return(list(summary = df_summary, score = df_score))


}

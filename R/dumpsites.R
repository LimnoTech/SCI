#' Calculate dumpsite scores
#'
#' @param df_point Point data from Rapid Stream Assessment (RSA) dataset.
#' @param df_reach Reach data from Rapid Stream Assessment (RSA) dataset.
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
#' @examples assess_dumpsites(df_point_example,
#'                            df_reach_example,
#'                            "StreamReachAttributes",
#'                            "StreamReaches",
#'                            "StreamPointAttributes",
#'                            "StreamPoints_Intersect" )

assess_dumpsites <- function(df_point, df_reach, reach_prefix_from_table, reach_prefix_from_layer, point_prefix_from_table, point_prefix_from_layer){

  df_length <- df_reach %>%
    dplyr::select(location_name = dplyr::all_of(paste0(reach_prefix_from_layer, ".subshed")),
                  reach_length = dplyr::all_of(paste0(reach_prefix_from_layer, ".Shape_Length")),
                  id = dplyr::all_of(paste0(reach_prefix_from_table, ".featureGlobalID_key")),
                  date = dplyr::all_of(paste0(reach_prefix_from_table, ".assessment_time"))) %>%
    dplyr::group_by(id) %>%
    dplyr::slice_max(date, n=1, with_ties = FALSE) %>% # Remove duplicate reaches. Keep most recent reach assessment. If same date/time and ID, maintain only one record
    dplyr::ungroup() %>%
    dplyr::left_join(location_name, by = "location_name") %>%
    dplyr::group_by(sci_subshed) %>%
    dplyr::summarise(subshed_length_meters = sum(reach_length))

  df_dump <- df_point %>%
    dplyr::select(location_name = dplyr::all_of(paste0(point_prefix_from_layer, ".SUBSHED")),
                  id = dplyr::all_of(paste0(point_prefix_from_table, ".featureGlobalID_key")),
                  assessment_type = dplyr::all_of(paste0(point_prefix_from_table, ".assessment_type")),
                  date = dplyr::all_of(paste0(point_prefix_from_table, ".assessment_time")),
                  d_impact = dplyr::all_of(paste0(point_prefix_from_table, ".d_impact")),
                  ) %>%
    tidyr::drop_na(d_impact) %>% # remove NA records in d_impact field. Likely assessments of existing points that did not change. Need to reference original record.
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

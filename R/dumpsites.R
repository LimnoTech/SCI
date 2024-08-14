


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

assess_dumpsites <- function(df_point, df_reach){

  df_length <- df_reach %>%
    dplyr::select(id = StreamReaches_attributes.featureGlobalID_key,
           watershed = StreamReaches_20231002_INT.WATERSHED,
           location_name = StreamReaches_20231002_INT.SUBSHED,
           reach_length = StreamReaches_20231002_INT.Shape_Length) %>%
    dplyr::left_join(location_name, by = "location_name") %>%
    dplyr::group_by(sci_subshed) %>%
    dplyr::summarise(subshed_length_meters = sum(reach_length))

  df_dump <- df_point %>%
    dplyr::select(assessment_type = StreamPoints_attributes.assessment_type,
                  d_impact = StreamPoints_attributes.d_impact,
                  location_name = StreamPoints_20231002_INT.SUBSHED) %>%
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

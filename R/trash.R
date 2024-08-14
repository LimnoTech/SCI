



assess_trash <- function(df) {

  df_summary <- df %>%
    dplyr::select(id = StreamReaches_attributes.featureGlobalID_key,
                  date = StreamReaches_attributes.assessment_time,
                  trash_extent = StreamReaches_attributes.trash_extent,
                  # reach_length = StreamReaches_20231002_INT.Shape_Length,
                  watershed = StreamReaches_20231002_INT.WATERSHED,
                  location_name = StreamReaches_20231002_INT.SUBSHED) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(date == max(date)) %>%  # Include only the most recent survey for each reach
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

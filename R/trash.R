



assess_trash <- function(df) {

  df_recent <- df %>%
    dplyr::select(id = StreamReaches_attributes.featureGlobalID_key,
                  date = StreamReaches_attributes.assessment_time,
                  trash_extent = StreamReaches_attributes.trash_extent,
                  # reach_length = StreamReaches_20231002_INT.Shape_Length,
                  watershed = StreamReaches_20231002_INT.WATERSHED,
                  sci_subshed = StreamReaches_20231002_INT.SUBSHED) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(date == max(date)) %>%  # Include only the most recent survey for each reach
    dplyr::ungroup() %>%
    dplyr::left_join(trash_score, by = "trash_extent")

  df_score <- df_recent %>%
    dplyr::filter(!is.na(trash_extent)) %>%   # Remove data records with no trash extent entry
    dplyr::group_by(sci_subshed) %>%
    dplyr::summarise(score = mean(score))
                     # sum_length = sum(reach_length))

  return(df_score)


}

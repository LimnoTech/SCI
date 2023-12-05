



assess_trash <- function(df) {

  df_recent <- df %>%
    dplyr::select(id = StreamReaches_attributes.featureGlobalID_key,
                  date = StreamReaches_attributes.assessment_time,
                  trash_extent = StreamReaches_attributes.trash_extent,
                  # reach_length = StreamReaches_20231002_INT.Shape_Length,
                  watershed = StreamReaches_20231002_INT.WATERSHED,
                  subshed = StreamReaches_20231002_INT.SUBSHED) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(trash_score, by = "trash_extent")

  df_summary <- df_recent %>%
    dplyr::filter(!is.na(trash_extent)) %>%   # Remove data records with no trash extent entry
    dplyr::group_by(subshed) %>%
    dplyr::summarise(avg_score = mean(score))
                     # sum_length = sum(reach_length))




}

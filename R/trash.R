



assess_trash <- function(df, start_date, end_date, reach_prefix_from_table, reach_prefix_from_layer) {

  df_summary <- df %>%
    dplyr::select(location_name = dplyr::all_of(paste0(reach_prefix_from_layer, ".subshed")),
                  id = dplyr::all_of(paste0(reach_prefix_from_table, ".featureGlobalID_key")),
                  date = dplyr::all_of(paste0(reach_prefix_from_table, ".assessment_time")),
                  trash_extent = dplyr::all_of(paste0(reach_prefix_from_table, ".trash_extent"))) %>%
    dplyr::filter(date >= start_date & date <= end_date) %>%
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

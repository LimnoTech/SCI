

assess_eia <- function() {


  df_subsheds <- eia_subsheds %>%
    dplyr::group_by(sci_watershed) %>%
    dplyr::summarise(subshed = sum(analysis_subshed_m2),
                     impervious = sum(analysis_impervious_m2),
                     impervious_treated = sum(analysis_impervious_treated_m2))

  df_rock_creek <- eia_rock_creek %>%
    dplyr::group_by(sci_watershed) %>%
    dplyr::summarise(subshed = sum(analysis_subshed_m2),
                     impervious = sum(analysis_impervious_m2),
                     impervious_treated = sum(analysis_impervious_treated_m2))


  df <- df_subsheds %>%
    dplyr::union(df_rock_creek) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(effective_impervious = impervious - impervious_treated,
                  percent_effective_impervious = effective_impervious / subshed * 100,
                  score = assign_score_from_maximum(percent_effective_impervious, eia_score, "max_eia_percent"))


  df_score <- df %>%
    dplyr::select(sci_watershed, score)


  return(list(summary = df, score = df_score))

  # # Import select columns and specify data types
  # types <- c("skip", "skip", "text", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
  #            "skip", "skip", "skip", "skip", "skip", "skip", "skip", "numeric", "skip", "skip",
  #            "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
  #            "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
  #            "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
  #            "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
  #            "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip",
  #            "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "skip", "text",
  #            "text", "text", "text", "skip", "text", "skip", "skip", "skip", "skip", "skip",
  #            "skip")
  # df_bmps <- readxl::read_excel(bmp_file, col_types = types)
  #
  #
  # # df <- df_bmps %>%
  # #   dplyr::mutate(mainstem_lookup = paste0(SewerTypeL1, " - ", WatershedL1),
  # #                 subshed_lookup = paste0(SewerTypeL1, " - ", WatershedL3),
  # #                 watts_lookup = paste0(SewerTypeL1, " - ", WatershedL4)) %>%
  # #   dplyr::left_join(eia_mainstem, by = "mainstem_lookup") %>%
  # #   dplyr::left_join(eia_subshed, by = "subshed_lookup") %>%
  # #   dplyr::left_join(eia_watts, by = "watts_lookup")
  #
  # # writexl::write_xlsx(df, "eia_test.xlsx")
  #
  #
  # # Confirm all of the segments in the lookup table have a match in the excel file
  #
  # # Calculate the impervious_treated from the Post_project_Impervious_land_cover_area field
  # df_mainstem <- df_bmps %>%
  #   dplyr::group_by(WatershedL1) %>%
  #   dplyr::summarise(treated_sqft = sum(Post_project_Impervious_land_cover_area, na.rm = TRUE)) %>%
  #   dplyr::filter(WatershedL1 == "Rock Creek") %>%
  #   dplyr::rename(watershed = WatershedL1)
  #
  # target_sheds <- location$watershed
  #
  # df_subshed <- df_bmps %>%
  #   dplyr::group_by(WatershedL3) %>%
  #   dplyr::summarise(treated_sqft = sum(Post_project_Impervious_land_cover_area, na.rm = TRUE)) %>%
  #   dplyr::filter(WatershedL3 %in% target_sheds) %>%
  #   dplyr::rename(watershed = WatershedL3)
  #
  #
  # df_treated <- df_mainstem %>%
  #   dplyr::union(df_subshed)





  # caclulate EIA score



}

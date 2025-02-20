#' Calculate grade for each index and subwatershed
#'
#' @param all_scores compiled scores for each metric and subwatershed
#'
#' @returns List of three dataframes: index_grades, overall_grades, graphics
#' @export
#'
#' @examples calculate_grades(df_all_scores_example)
calculate_grades <- function(all_scores){

  # Create index dataframe
  index <- data.frame(
    metric = c("Trash",
               "Dumpsites",
               "Escherichia coli",
               "Effective Impervious Area",
               "Temperature, water",
               "pH",
               "Dissolved oxygen (DO)",
               "Turbidity",
               "Conductivity",
               "Nitrogen",
               "Phosphorus, Total (as P)",
               "Connectivity",
               "Fish",
               "Habitat",
               "Macroinvertebrates"),
    index = c("Human Health",
              "Human Health",
              "Human Health",
              "Hydrology",
              "Water Quality",
              "Water Quality",
              "Water Quality",
              "Water Quality",
              "Water Quality",
              "Water Quality",
              "Water Quality",
              "Aquatic Life",
              "Aquatic Life",
              "Aquatic Life",
              "Aquatic Life"),
    max_metric_score = 10)



  # Reformat scores and remove rows with NA/blank metric scores
  df_scores <- all_scores %>%
    tidyr::pivot_longer(cols = 2:16, names_to = "metric", values_to = "metric_score") %>%
    tidyr::drop_na(metric_score) %>%
    dplyr::left_join(index, by= "metric")



  # Calculate grades for each index using scores
  df_grades <- df_scores %>%
    dplyr::group_by(sci_subshed, index) %>%
    dplyr::summarise(index_score = sum(metric_score),
              max_index_score = sum(max_metric_score)) %>%
    dplyr::ungroup()


  df_grades <- df_grades %>%
    dplyr::mutate(index_percent = index_score / max_index_score * 100,
           index_grade = dplyr::case_when(index_percent >= 90  ~ "A",
                                   index_percent < 90 & index_percent >= 80 ~ "B",
                                   index_percent < 80 & index_percent >= 70 ~ "C",
                                   index_percent < 70 & index_percent >= 60 ~ "D",
                                   index_percent < 60 ~ "F"))


  # Calculate overall grades for each watershed
  df_overall_grades <- df_scores %>%
    dplyr::group_by(sci_subshed) %>%
    dplyr::summarise(max_overall_score = sum(max_metric_score),
              overall_percent = sum(metric_score)/max_overall_score * 100,
              overall_grade = dplyr::case_when(overall_percent >= 90  ~ "A",
                                        overall_percent < 90 & overall_percent >= 80 ~ "B",
                                        overall_percent < 80 & overall_percent >= 70 ~ "C",
                                        overall_percent < 70 & overall_percent >= 60 ~ "D",
                                        overall_percent < 60 ~ "F"))




  # Determine the corresponding graphic file
  df_grades <- df_grades %>%
    dplyr::mutate(index_underscore = gsub(" ", "_", index),
           image_file = paste0("template_files/graphic_pieces/",index_underscore, "_", index_grade, ".png"))

  df_overall_grades <- df_overall_grades %>%
    dplyr::mutate(index = "Overall",
           image_file = paste0("template_files/graphic_pieces/Overall_", overall_grade, ".png"))


  df_graphics <- df_grades %>%
    dplyr::bind_rows(df_overall_grades) %>%
    dplyr::select(sci_subshed, index, image_file)


  return(list(index_grades = df_grades, overall_grades = df_overall_grades, graphics = df_graphics))




}

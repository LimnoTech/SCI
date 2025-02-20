#' Populate powerpoint template to create graphic for each subwatershed
#'
#' @param df_graphics dataframe returned from \code{\link{calculate_grades}}
#' that includes filepaths to graphic pieces that will makeup final subwatershed
#' graphic
#' @param output_path filepath of output powerpoint slide deck (.pptx)
#'
#' @returns powerpoint slide deck containing graphics for each subwatershed
#' @export
create_graphics <- function(df_graphics, output_path) {



  # Initialize template
  template <- officer::read_pptx('template_files/empty_template/SCI_template.pptx')


  # Get list of each subwatershed
  subshed_list <- unique(df_graphics$sci_subshed)


  # Loop through all subwatersheds
  for (i in 1:length(subshed_list)) {


    # Filter for target subwatershed
    df_graphics_filtered <- df_graphics %>%
      dplyr::filter(sci_subshed == subshed_list[i])

    # Get image location for target subwatershed
    water_quality_grade <- df_graphics_filtered$image_file[df_graphics_filtered$index == "Water Quality"]
    hydrology_grade <- df_graphics_filtered$image_file[df_graphics_filtered$index == "Hydrology"]
    human_health_grade <- df_graphics_filtered$image_file[df_graphics_filtered$index == "Human Health"]
    aquatic_life_grade <- df_graphics_filtered$image_file[df_graphics_filtered$index == "Aquatic Life"]
    overall_grade <- df_graphics_filtered$image_file[df_graphics_filtered$index == "Overall"]
    subshed_name <- subshed_list[i]

    # Define corresponding item/location in PowerPoint template
    template <- template %>%
      officer::on_slide(index = i) %>%
      officer::ph_with(officer::external_img(water_quality_grade), location = officer::ph_location_label(ph_label = "water_quality_grade")) %>%
      officer::ph_with(officer::external_img(hydrology_grade), location = officer::ph_location_label(ph_label = "hydrology_grade")) %>%
      officer::ph_with(officer::external_img(human_health_grade), location = officer::ph_location_label(ph_label = "human_health_grade")) %>%
      officer::ph_with(officer::external_img(aquatic_life_grade), location = officer::ph_location_label(ph_label = "aquatic_life_grade")) %>%
      officer::ph_with(officer::external_img(overall_grade), location = officer::ph_location_label(ph_label = "overall_grade")) %>%
      officer::ph_with(value = subshed_name, location = officer::ph_location_label(ph_label = "tributary_name"))


    # Adds slide to the end of presentation (for next subwatershed)
    template <- officer::add_slide(template, layout = "SCI", master = "Office Theme")



  } # End for loop


  # Print  PowerPoint
  print(template, target = output_path)




}

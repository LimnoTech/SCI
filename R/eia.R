

assess_eia <- function(bmp_file) {


  df <- readxl::read_excel(bmp_file) %>%
    dplyr::mutate(segment_lookup = paste0(WatershedL2, " - ", SewerTypeL1, " - ", WatershedL3)) %>%
    dplyr::left_join(eia_location_id, by = "segment_lookup")

  # Confirm all of the segments in the lookup table have a match in the excel file

  # Calculate the impervious_treated from the Post_project_Impervious_land_cover_area field

  # caclulate EIA score



}



assess_eia <- function() {


  area <- subsheds %>%
    dplyr::mutate(segment_lookup = paste(WATERSHED_SEGMENTS, "-", SEWER_SYSTEM, "-", SUBSHED )) %>%
    dplyr::left_join(eia_area, by="segment_lookup") %>%
    tidyr::drop_na(location_id) %>%
    dplyr::group_by(location_id) %>%
    dplyr::summarise(area = sum(ACRES)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(sqft = acres * 43560)
}

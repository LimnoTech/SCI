
format_results <- function(df){

  # Qualifiers to be moved to a separate field
  quals_orig <- c("< ", "> ", "\\? ","<", ">", "\\?")
  quals_new <- c("<", ">", "?","<", ">", "?")

  df <- df %>%
    dplyr::mutate(
      result_format = Result,
      qualifier_format = "")

  for(i in 1:nrow(df)){

    for(j in 1:length(quals_orig)){

      # If one of the qualifier symbols are found in the Results column:
      #   1) remove it in the results_format field
      #   2) add it to the qualifier_format column
      if(grepl(quals_orig[j], df$result_format[i]) == TRUE){
        df$result_format[i] = gsub(quals_orig[j], '', df$Result[i])
        df$qualifier_format[i] = quals_new[j]
      }

    }
  }


  df <- df %>%
    dplyr::mutate(result_format = dplyr::case_when(result_format == "" ~ NA,
                                                   TRUE ~ result_format)) %>%
    dplyr::mutate(location_format = gsub("\\s+", "", Location)) %>% # Remove all spaces from location_id
    dplyr::mutate(date_format = as.Date(Date, format = "%d %b %Y"),
                  result_format = as.numeric(result_format)) # Results in warning that NAs introduced by coercion - this is intended result)


  return(df)



}

process_wq <- function(df, start_date, end_date){

  # Filter data for target locations and parameters and target date range
  loc_id <- location_id$location_id
  params <- c("Conductivity", "Dissolved oxygen (DO)", "Escherichia coli", "pH", "Phosphorus, Total (as P)", "Nitrogen", "Temperature, water", "Turbidity")

  df_processed <- df %>%
    dplyr::filter(location_format %in% loc_id,
                  Analyte %in% params,
                  qualifier_format != "?",
                  date_format >= start_date & date_format <= end_date)

  # Remove results that are NA
  df_processed <- df_processed %>%
    tidyr::drop_na(result_format)

  # Rename and select key fields
  df_processed <- df_processed %>%
    dplyr::select(
      sample_id = `Sample ID`,
      location_id = location_format,
      date = date_format,
      parameter = Analyte,
      unit = Unit,
      result = result_format,
      qualifier = qualifier_format
    )




  return(df_processed)


}





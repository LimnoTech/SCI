
format_results <- function(df){

  # Qualifiers to be moved to a separate field
  quals_orig <- c("< ", "> ", "\\? ","<", ">", "\\?")
  quals_new <- c("<", ">", "?","<", ">", "?")

  df <- df %>%
    dplyr::mutate(
              result_processed = Result,
              qualifier_processed = "")

  for(i in 1:nrow(df)){

    for(j in 1:length(quals_orig)){

      # If one of the qualifier symbols are found in the Results column:
      #   1) remove it in the results_processed field
      #   2) add it to the qualifier_processed column
      if(grepl(quals_orig[j], df$result_processed[i]) == TRUE){
        df$result_processed[i] = gsub(quals_orig[j], '', df$Result[i])
        df$qualifier_processed[i] = quals_new[j]
      }

    }
  }


  df <- df %>%
    dplyr::mutate(result_processed = dplyr::case_when(result_processed == "" ~ NA,
                                                TRUE ~ result_processed)) %>%
    dplyr::mutate(date_processed = as.Date(Date, format = "%d %b %Y"),
                  result_processed = as.numeric(result_processed)) # Results in warning that NAs introduced by coercion - this is intended result)


  return(df)



}

process_wq <- function(df){

  # Filter data for target locations and parameters
  loc_id <- location$location_id
  params <- c("Conductivity", "Dissolved oxygen (DO)", "Escherichia coli", "pH", "Phosphorus, Total (as P)", "Nitrogen", "Temperature, water", "Turbidity")

  df_processed <- df %>%
    dplyr::filter(Location %in% loc_id,
                  Analyte %in% params,
                  qualifier_processed != "?")

  # Remove results that are NA
  df_processed <- df_processed %>%
    tidyr::drop_na(result_processed)

  # Rename and select key fields
  df_processed <- df_processed %>%
    dplyr::select(
      sample_id = `Sample ID`,
      location_id = Location,
      date = date_processed,
      parameter = Analyte,
      unit = Unit,
      result = result_processed,
      qualifier = qualifier_processed
    )




  return(df_processed)


}





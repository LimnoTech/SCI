
format_results <- function(df){

  # Qualifiers to be moved to a separate field
  quals_orig <- c("< ", "> ", "\\? ","<", ">", "\\?")
  quals_new <- c("<", ">", "?","<", ">", "?")

  df <- df %>%
    dplyr::mutate(
              result_processed = Result,
              qualifier_processed = "")

  for(i in 1:nrow(df)){

    for(j in 1:length(quals)){

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
                                                TRUE ~ result_processed))

  return(df)



}

process_wq <- function(df){


  loc_id <- location$location_id

  df_processed <- dplyr::mutate(results_processed <- dplyr::case_when(grepl("< ", Result ~ paste(Result, "<"))))

  df_processed <- df %>%
    dplyr::filter(Location %in% loc_id)


}





#' Assign score based on lookup of maximum number. Used for dumpsites and EIA.
#'
#' @param value Number of dumpsites per mile for a given subwatershed.
#' @param lookup_table Lookup table of number of dumpsites per mile allowed per
#'     score (1-10).
#' @param lookup_field Column name containing the maximum number of sites per
#'     mile.
#'
#' @return Numerical score (1-10)
#' @export
#'
#' @examples assign_score_from_maximum(0.5, dumpsite_score, "max_sites_per_mile")

assign_score_from_maximum <- function(value, lookup_table, lookup_field) {

  # Make sure dumpsite scores are in descending order
  lookup_table <- lookup_table[order(-lookup_table$score),]

  score <- NA
  for (i in 1:nrow(lookup_table)) {
    max <- lookup_table[[lookup_field]][i]
    if(!is.na(max) && !is.null(max)) {
      if (value <= max) {
        score <- lookup_table$score[i]
        break
      } else {
        score = 1
      }
    }

  }
  return(score)
}

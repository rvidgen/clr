#' Sanitizes the author list
#'
#' @param authorVector
#' @import stringr
#' @import magrittr
#' @return Vector

formatAuthorList <- function(authorVector){

  fix_spaces <- function(x){
    gsub(" ", "", x, fixed = TRUE)
  }

  fix_periods <- function(x){
    gsub(".", "", x, fixed = TRUE)
  }

  fix_commas <- function(x){
    gsub(",", "", x, fixed = TRUE)
  }

  auths = strsplit(authorVector, ".,", fixed=T) %>%
    lapply(str_trim) %>%
    # The next line should be extracted into a named function as well in order to understand its functionality
    lapply(function(z) z[!is.na(as.logical(sapply(z, function(x) grep(' ', x))))]) %>%
    lapply(fix_spaces) %>%
    lapply(fix_periods) %>%
    lapply(fix_commas)

  return(auths)
}


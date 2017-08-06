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

  auths = strsplit(authorVector, ".,", fixed=T)
  auths = lapply(auths, str_trim)
  auths = lapply(auths, function(z) z[!is.na(as.logical(sapply(z, function(x) grep(' ', x))))])
  auths = lapply(auths, fix_spaces)
  auths = lapply(auths, fix_periods)
  auths = lapply(auths, fix_commas)

  return(auths)
}


#' Load and process articles CSV files.
#'
#' @param files_path Path to files.
#' @param data_source Data source..
#' @return dataframe.
#' @examples
#' articles_df <- getArticles(files_path = read_data, data_source = data_source)
#'
#' @import dplyr
#' @import ggplot2
#'
getArticles <- function(files_path, data_source = "Scopus"){

  # Read in CSV files
  files <- list.files(path = files_path,
                      pattern = "\\.(csv|CSV)$")

  files_csv <- lapply(files, function(x) read.csv(file = paste(read_data, x, sep = "/"), stringsAsFactors = FALSE))

  # Change column names and classes, bind and remove duplicates
  cleanDFs <- function(x){
    x <- x %>%
      select_(.dots = variable_map$Scopus.field) %>%
      setNames(variable_map$CLR.variable) %>%
      mutate_at(.vars = vars(Authors:Title, DOI:EID),
                .funs = funs(as.character)) %>%
      mutate_at(.vars = vars(PageStart, PageEnd),
                .funs = funs(as.numeric)) %>%
      mutate_at(.vars = vars(Year, Volume, Issue, ArtNo, PageCount, NumberCites),
                .funs = funs(as.integer))
    return(x)
  }


  articles_df <- lapply(files_csv, function(x) cleanDFs(x)) %>%
    bind_rows() %>%
    unique(.)

  # Remove entries without a Year
  articles_df <- articles_df[!is.na(articles_df$Year), ]

  # Convert NAs to 0 for NumberCities column
  articles_df$NumberCites[is.na(articles_df$NumberCites)] <- 0

  # Process SourceTitle column -- why??

  # Article ID column
  articles_df$Id <- seq(1, nrow(articles_df), 1)
  articles_df <- articles_df %>% select(Id, Authors:EID)

  # Text processing
  articles_df$Abstract <- gsub("[^[:alnum:]///' ]", " ", articles_df$Abstract)

  # Print some summaries
  cat(nrow(articles_df), "articles, spanning", min(articles_df$Year), "to", max(articles_df$Year))

  return(articles_df)
}

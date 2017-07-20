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

  return(articles_df)
}

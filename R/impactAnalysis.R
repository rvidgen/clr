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


impactAnalysis <- function(articles_df){


  impact_df <- articles_df %>%
    group_by(Year) %>%
    summarise(narticles = n())

  impact_by_journal <- articles_df %>%
    group_by(SourceTitle) %>%
    summarise(Titles = n(),
              Cites = sum(NumberCites),
              Min_year = min(Year),
              Max_year = max(Year),
              Num_years = max(Year) - min(Year) + 1) %>%
    mutate(Impact = Cites / Titles,
           Papers_per_Year = Titles / Num_years) %>%
    arrange(desc(Impact)) %>%
    left_join(articles_df %>%
                group_by(SourceTitle) %>%
                arrange(desc(NumberCites)) %>%
                mutate(cumulative_count = match(Id, unique(Id))) %>%
                filter(cumulative_count <= NumberCites, !is.na(NumberCites)) %>%
                summarise(hindex = max(cumulative_count)),
              by = c("SourceTitle" = "SourceTitle"))

  # What does summary do?

  impact_list <- list(impact_df = impact_df, articles_df = articles_df, impact_by_journal = impact_by_journal)
  class(impact_list) <- "impactCLR"

  return(impact_list)
}

plot.impactCLR <- function(impact_list){
  ggplot(impact_list$impact_df, aes(x = Year, y = narticles)) + geom_line()
}


topCites <- function(x, n){
  UseMethod("topCites", x)
}

topCites.impactCLR <- function(x, n = 5){
  x$articles_df %>%
    select(Id, NumberCites) %>%
    top_n(n = n, wt = NumberCites) %>%
    arrange(desc(NumberCites))
}

topSources <- function(x, n){
  UseMethod("topSources", x)
}

topSources.impactCLR <- function(x, n = 5){
  x$articles_df %>%
    group_by(SourceTitle) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    top_n(n, Count)
}



# summary.impactCLR <- function(impact_list){
#
# }

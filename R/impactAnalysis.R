#' Perform Impact Analysis
#'
#' @param articles_df Dataframe of articles. Output from getArticles() function.
#' @return List
#' @examples
#' impact <- impactAnalysis(articles_df = articles_df)
#'
#' @import dplyr
#' @import ggplot2
#' @import stringr


impactAnalysis <- function(articles_df){

  # Number of articles by year df
  impact_df <- articles_df %>%
    group_by(Year) %>%
    summarise(narticles = n())

  # Df with source, n titles in source, n cites, min year, max year, number of years between, impact score (cites / titles), n papers per year and hindex
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
                summarise(Hindex = max(cumulative_count)),
              by = c("SourceTitle" = "SourceTitle"))

  # Format authors vector
  authors_df <- formatAuthorList(authorVector = articles_df$Authors)
  authors_df <- as.list(authors_df)
  names(authors_df) <- articles_df$Id
  authors_df <- stack(authors_df)[c(2,1)]
  colnames(authors_df) <- c("Id", "Author")
  authors_df$Id <- as.numeric(authors_df$Id)

  # Count of articles by author
  authors_art_count <- authors_df %>%
    group_by(Author) %>%
    summarise(NumberArticles = n()) %>%
    arrange(desc(NumberArticles))

  # Sum of cites by author
  authors_cites <- authors_df %>%
    left_join(articles_df %>%
                select(Id, NumberCites)) %>%
    group_by(Author) %>%
    summarise(TotalCites = sum(NumberCites, na.rm = T)) %>%
    arrange(desc(TotalCites))

  # Join
  authors_stats <- authors_art_count %>%
    inner_join(authors_cites) %>%
    mutate(Impact = ifelse(TotalCites > 0 & NumberArticles > 0, TotalCites / NumberArticles, NA)) %>%
    arrange(desc(Impact)) %>%
    left_join(authors_df %>%
                left_join(articles_df %>%
                            select(Id, NumberCites)) %>%
                group_by(Author) %>%
                arrange(desc(NumberCites)) %>%
                mutate(cumulative_count = match(Id, unique(Id))) %>%
                filter(cumulative_count <= NumberCites, !is.na(NumberCites)) %>%
                summarise(Hindex = max(cumulative_count)),
              by = c("Author" = "Author")) %>%
    arrange(desc(Hindex))


  # Create impactCLR object
  impact_list <- list(impact_df = impact_df, articles_df = articles_df, impact_by_journal = impact_by_journal, authors_stats = authors_stats, authors_df = authors_df)
  class(impact_list) <- "impactCLR"

  # Return object (list) of class impactCLR
  return(impact_list)
}

##################
# Object methods #
##################

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

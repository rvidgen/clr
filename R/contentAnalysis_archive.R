#' Perform Content Analysis
#'
#' @param articles_df Path to files
#' @return List
#' @examples
#' structure <- structureAnalysis(impact_object = impact)
#' plot(structure, loess = TRUE)
#'
#' @import dplyr
#'
#' Borrows from http://tidytextmining.com/_book/topicmodeling.html


contentAnalysis <- function(articles_df){

  # Read stopwords
  stopwords <- c(stopwords("english"))

  # Create corpus
  abstracts <- gsub("[^[:alnum:]///' ]", " ", articles_df$Abstract)
  corpus <- Corpus(VectorSource(abstracts)) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removePunctuation)

  # Create DTM
  dtm <- DocumentTermMatrix(corpus)

  #



  # Create contentCLR object
  content_list <- list()
  class(content_list) <- "contentCLR"

  return(content_list)
}


# Plot componentz vs size
plot.contentCLR <- function(content_list){

}

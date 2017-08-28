#' Perform Content Analysis
#'
#' Perform content analysis; build LDA model on abstract text
#'
#' @param articles_df Dataframe of articles. Output from getArticles() function.
#' @param k Number of topics
#' @param words_to_remove (Optional) Additional stopwords to be removed
#' @return List
#' @examples
#' content <- contentAnalysis(articles_df =  articles_df, k = 10)
#'
#' # Add your own stop words
#' words_to_remove <- c("stopword1", "stopword2")
#' content <- contentAnalysis(articles_df =  articles_df, k = 10, words_to_remove = words_to_remove)
#'
#' # Word cloud
#' wordCloud(content)
#'
#' # Top words in corpus
#' topNWordsInDoc(content, n = 10)
#'
#' # Plot top words
#' topNWordsInDoc(content, n = 10, plot = TRUE)
#'
#' # Top n words in topics
#' topNWordsInTopic(content, n = 3)
#'
#' # Plot top n words in topics
#' topNWordsInTopic(content, n = 3, plot = TRUE)
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import tidytext
#' @import tm
#' @import stringr
#' @import topicmodels
#' @import wordcloud
#' @import RColorBrewer
#'
#' Borrows from http://tidytextmining.com/_book/topicmodeling.html


contentAnalysis <- function(articles_df, k, words_to_remove){

  # Load stopwords
  data("stop_words")

  if(!is.null(words_to_remove)){stop_words <- bind_rows(stop_words,
                                                        data_frame(word = words_to_remove,
                                                                   lexicon = "custom")) }

  # Get Id / Source / Word DF, tokensize
  by_doc_word <- articles_df %>%
    select(Id, SourceTitle, Abstract) %>%
    unnest_tokens(word, Abstract)

  # Remove stopwords, remove numbers and stem
  by_doc_word <- by_doc_word %>%
    anti_join(stop_words) %>%
    filter(!str_detect(word, '[0-9]')) %>%
    mutate(word = SnowballC::wordStem(word))

  # Get word counts
  word_counts <- by_doc_word %>%
    count(word, sort = TRUE)

  # Word count by article
  word_counts_by_article <- by_doc_word %>%
    count(Id, word, sort = TRUE)

  # Create DTM
  dtm <- word_counts_by_article %>%
    cast_dtm(Id, word, n)

  # Model
  lda_mod <- LDA(dtm, k = k, control = list(seed = 1234))

  # Tidy - get betas
  betas <- tidy(lda_mod)

  # Tidy - get gammas
  gammas <- tidy(lda_mod, matrix = "gamma")

  # Get topic assignment for each doc
  doc_classifications <- gammas %>%
    group_by(document) %>%
    top_n(1, gamma) %>%
    ungroup() %>%
    arrange(gamma)

  # Create contentCLR object
  content_list <- list(by_doc_word = by_doc_word, word_counts = word_counts, dtm = dtm, model = lda_mod, betas = betas, gammas = gammas, doc_classifications = doc_classifications)
  class(content_list) <- "contentCLR"

  return(content_list)
}


# Plot components vs size
plot.contentCLR <- function(content_list){
  content_list$by_doc_word %>%
    count(word, sort = TRUE) %>%
    top_n(10) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
}

# Plot word clouds by topic
wordCloud <- function(content_clr){
  model <- content_clr$model
  dtm <- content_clr$dtm
  k <- content$model@k
  for (i in 1:k){
    plotWordCloud(model, dtm, i, 30)
    title(paste("Topic ", i))
  }
}


# Get and plot top words in corpus
topNWordsInDoc <- function(content_clr, n, plot = FALSE){

  top_n_words <- content_clr$by_doc_word %>%
    count(word, sort = TRUE) %>%
    top_n(n) %>%
    mutate(word = reorder(word, n))

  if(plot){
    output <- top_n_words %>%
      ggplot(aes(word, n)) +
        geom_col() +
        xlab(NULL) +
        ylab("Count") +
        coord_flip() +
        theme_minimal()
  }else{
    output <- top_n_words
  }
  return(output)
}

# Get and plot top words in topics
topNWordsInTopic <- function(content_clr, n, plot = FALSE){

  top_n_words <- content_clr$betas %>%
    group_by(topic) %>%
    top_n(n, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

  if(plot){
    output <- top_n_words %>%
      mutate(term = reorder(term, beta)) %>%
      ggplot(aes(term, beta)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ topic, scales = "free") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 15, angle = 90, hjust = 1))
  }else{
    output <- top_n_words
  }
  return(output)
}

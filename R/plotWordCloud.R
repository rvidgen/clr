# Function to plot wordcloud
plotWordCloud<-function(model, myDtm, index, numTerms) {

  model_terms<-terms(model,numTerms)
  model_topics<-topics(model)

  terms_i<-model_terms[,index]
  topic_i<-model_topics==index
  dtm_i<-myDtm[topic_i,terms_i]
  frequencies_i<-colSums(as.matrix(dtm_i))
  wordcloud(terms_i,frequencies_i,min.freq=0, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
}


############################################
#                                          #
#   clR vignette                           #
#   requires R version 3.4.x or higher     #
#                                          #
############################################


# To install the clR package uncomment these lines
#library(devtools)
#install_github("rvidgen/clr")

library(clR)

############################################
#                                          #
#  Set parameters for the analysis         #
#                                          #
############################################

# This is where the input csv files are
read_data <- "/Users/ ... /data"

# Results files are written to a sub directory in read_data
results_dir = "results40"

# Source of data - currently only Scopus (WoS will be added)
data_source <- "Scopus"

# Set the number of topics (k) to be used in the topic model.
# Typical values are 30, 50, 100.
# Large datasets with high vaues of k will take longer to run
k = 40

# Set the words to remove from the topic model
words_to_remove = c("abstract", "study", "research", "results", "paper", "available",
                    "copyright", "acm", "chapter", "author", "authors", "proceedings",
                    "rights", "reserved", "one", "two", "three", "four",
                    "findings", "analysis", "elsevier", "limited", "emerald",
                    "ieee", "ltd", "taylor", "francis", "igi", "springer", "verlag",
                    "wiley", "sons", "section", "introduction", "methodology",
                    "discussion", "conclusion", "palgrave", "papers", "can")

# Add more words related to the search terms used to extract articles, e.g.,
words_to_remove = append(words_to_remove, c(
                    "virtual", "game", "games", "world", "worlds", "augmented", "reality", "user", "system"))

############################################
#                                          #
##  The analysis begins here               #
#                                          #
############################################

setwd(read_data)
list.files()

if (!file.exists(results_dir)){
  dir.create(file.path(read_data, results_dir))
}

##### Load data files
articles_df <- getArticles(files_path = read_data,
                           data_source = data_source)
nrow(articles_df)
names(articles_df)

# remove articles with no abstract
articles_df = articles_df[which(articles_df$Abstract != " No abstract available "), ]
nrow(articles_df)

# remove articles with no author
articles_df = articles_df[which(articles_df$Authors != "[No author name available]"), ]
nrow(articles_df)

# select on year
articles_df = articles_df[articles_df$Year >1800,]
articles_df = articles_df[articles_df$Year <2100,]
nrow(articles_df)

# write out articles sorted by citation
articlecites = articles_df[order(articles_df$NumberCites,decreasing=TRUE),]
fileout = paste(results_dir,"CLRarticleanalysis.csv", sep="/")
write.csv(articlecites, file=fileout, row.names=F)

##### Impact analysis
impact <- impactAnalysis(articles_df = articles_df)

# write out author impact
authImpact = impact$authors_stats
authImpact = authImpact[order(authImpact$Hindex,decreasing=TRUE),]
fileout = paste(results_dir,"CLRauthoranalysis.csv", sep="/")
write.csv(authImpact, file=fileout, row.names=F)

# write out journal impact
journalImpact = impact$impact_by_journal
journalImpact = journalImpact[order(journalImpact$Hindex,decreasing=TRUE),]
fileout = paste(results_dir,"CLRjournalanalysis.csv", sep="/")
write.csv(journalImpact, file=fileout, row.names=F)

# Plot n articles over time period
plot(impact)

# Top cited papers
topCites(impact, n = 10)

# Most common sources
topSources(impact, n = 6)


##### Structure analysis
structure <- structureAnalysis(impact_object = impact)

# Components vs Size
plot(structure, loess = T)

# Plot structure node : edges
showGraphs(structure)

# Plot components
graphComponents(structure, n = 4)


##### Content analysis

# Topic model
content <- contentAnalysis(articles_df =  articles_df, k = k, words_to_remove = words_to_remove)
perplexity(content$model)
tmodel = content$model
fileout = paste(results_dir,"CLRtopicmodel.RData", sep="/")
save(tmodel, file = fileout)


# Word cloud
wordCloud(content)

# save word clouds as a pdf
fileout = paste(results_dir,"topic_cloud.pdf", sep="/")
pdf(fileout)
wordCloud(content)
dev.off()

# Top words in corpus
topNWordsInDoc(content, n = 10)

# Plot top words
topNWordsInDoc(content, n = 10, plot = TRUE)

# Top n words in topics
topNWordsInTopic(content, n = 3)

# Plot top n words in topics
topNWordsInTopic(content, n = 4, plot = TRUE)

# Join articles to posteriors and write out
LDAPost = posterior(content$model)
LDAPostDF = as.data.frame(LDAPost$topics)
LDAPostDF$Id = row.names(LDAPostDF)
str(LDAPostDF)
head(LDAPostDF)

mergeTopic <- as.data.frame(content$doc_classifications) %>%
  merge(x = articles_df, by.x = "Id", by.y = "document", all = TRUE) %>%
  merge(x = LDAPostDF, by.x = "Id", by.y = "Id", all = TRUE) %>%
  arrange(topic)

fileout = paste(results_dir,"CLRarticleTopic.csv", sep="/")
write.csv(mergeTopic, file=fileout, row.names=F)


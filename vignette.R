####################
#                  #
#   clR vignette   #
#                  #
####################


library(clR)

# Set parameters
read_data <- "/Users/duncan/Work/RichardV/Testing/input_files"
data_source <- "Scopus"
output_data <- "/Users/duncan/Work/RichardV/Testing/output_folder"


# Load data files
articles_df <- getArticles(files_path = read_data,
                          data_source = data_source)


impact <- impactAnalysis(articles_df = articles_df)

plot(impact)
topCites(impact, n = 10)
topSources(impact, n = 6)

head(impact[['impact_by_journal']])

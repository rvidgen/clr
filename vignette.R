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

# Impact analysis
impact <- impactAnalysis(articles_df = articles_df)

# Plot n articles over time period
plot(impact)

# Top cited papers
topCites(impact, n = 10)

# Most common sources
topSources(impact, n = 6)


# Structure analysis
structure <- structureAnalysis(impact_object = impact)

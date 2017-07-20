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
articlesDF <- getArticles(files_path = read_data,
                          data_source = data_source)




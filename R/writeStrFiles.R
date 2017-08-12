#' Perform Structure Analysis
#'
#' @param structure_list structureCLR object
#' @param impact_list impactCLR object
#' @param gml_name gml file name
#' @param txt_name Output UCINET file name
#' @param path Output path
#'
#' @return NA
#'
#'

writeStrFiles <- function(structure_list, impact_list, gml_name, txt_name, path = getwd()){
  write.graph(graph = structure_list$graph_df, file = paste0(path, "/", gml_name), format = 'gml')
  writeUCINET(outfile = paste0(path, "/", txt_name),
              authors = formatAuthorList(authorVector = impact_list$articles_df$Authors),
              nodecount = 3222)
}

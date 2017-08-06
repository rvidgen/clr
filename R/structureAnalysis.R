#' Perform Structure Analysis
#'
#' @param articles_df Path to files
#' @return List
#' @examples
#' structure <- structureAnalysis(impact_object = impact)
#'
#' @import dplyr
#' @import igraph


structureAnalysis <- function(impact_object){

  # Create edges df
  edges_df <- impact_object[['authors_df']] %>% group_by(Id) %>%
    filter(n()>=2) %>% group_by(Id) %>%
    do(data.frame(t(combn(.$Author, 2)), stringsAsFactors=FALSE)) %>%
    ungroup() %>%
    mutate(weight = 1) %>%
    select(-Id) %>%
    rename(Auth1 = X1, Auth2 = X2) %>%
    group_by(Auth1, Auth2) %>%
    summarise(weight = sum(weight))

  graph_df <- graph.data.frame(edges_df, directed = FALSE, vertices = impact_object[['authors_stats']])
  graph_df <- simplify(graph_df, remove.multiple = TRUE)
  graph_df <- delete.vertices(graph_df, V(graph_df)[ degree(graph_df)==0 ])
  E(graph_df)$label = E(graph_df)$weight
  graph_decomp <- decompose.graph(graph_df, max.comp=50)
  comps = t(sapply(seq_along(graph_decomp), function(x) c(Subgraph = x, NodeCount = vcount(graph_decomp[[x]]), EdgeCount = ecount(graph_decomp[[x]]))))
  cv = as.data.frame(sapply(graph_decomp, vcount))
  cv$comp = rownames(cv)
  colnames(cv) <- c("vcountcomp", "comp")
  cv = cv[order(cv$vcountcomp,decreasing=TRUE),]
  cv$ID <- seq.int(nrow(cv))
  main = as.numeric(cv$comp[1])
  deg = as.data.frame(degree(graph_decomp[[main]]))
  names(deg) <- 'degree'


  return()
}


# Add methods as in original script

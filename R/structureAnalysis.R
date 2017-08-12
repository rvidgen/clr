#' Perform Structure Analysis
#'
#' @param articles_df Path to files
#' @return List
#' @examples
#' structure <- structureAnalysis(impact_object = impact)
#' plot(structure, loess = TRUE)
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

  # Create graph df
  graph_df <- graph.data.frame(edges_df, directed = FALSE, vertices = impact_object[['authors_stats']])
  graph_df <- simplify(graph_df, remove.multiple = TRUE)
  graph_df <- delete.vertices(graph_df, V(graph_df)[ degree(graph_df)==0 ])
  E(graph_df)$label = E(graph_df)$weight
  graph_decomp <- decompose.graph(graph_df, max.comp=50)

  # Decompose graph into components
  dg <- decompose.graph(graph_df, max.comp=50)

  #
  comps = t(sapply(seq_along(graph_decomp), function(x) c(Subgraph = x, NodeCount = vcount(graph_decomp[[x]]), EdgeCount = ecount(graph_decomp[[x]]))))
  cv = as.data.frame(sapply(graph_decomp, vcount))
  cv$comp = rownames(cv)
  colnames(cv) <- c("vcountcomp", "comp")
  cv = cv[order(cv$vcountcomp,decreasing=TRUE),]
  cv$ID <- seq.int(nrow(cv))
  main = as.numeric(cv$comp[1])
  deg = as.data.frame(degree(graph_decomp[[main]]))
  names(deg) <- 'degree'

  getCloseness <- function(dg, x){
    c = as.numeric(x["comp"])
    compcl <- dg[c]
    compcl <- closeness(compcl[[1]], normalized = T) %>%
      as.data.frame(.) %>%
      tibble::rownames_to_column("Author") %>%
      rename_(compcl = ".") %>%
      arrange(desc(compcl)) %>%
      left_join(impact[['authors_stats']] %>%
                  select(Author, TotalCites),
                by = c("Author" = "Author"))
    return(compcl)
  }

  component_cites <- apply(cv, 1, function(x) getCloseness(dg = dg, x = x))

  # Plots
  p1 <- plot(graph_df, layout=layout.auto, vertex.label=NA, edge.label=NA,
             main='Complete network', vertex.size=3,
             vertex.color="yellow", edge.color='lightblue')

  l <- layout.kamada.kawai(dg[[main]])
  p2 <- plot(dg[[main]], layout=l, vertex.label=NA, edge.label=NA,
             main='Main component', vertex.size=3,
             vertex.color="yellow", edge.color='lightblue')

  list_of_plots <- lapply()

  # Create impactCLR object
  structure_list <- list(cv = cv, component_cites = component_cites)
  class(structure_list) <- "structureCLR"
  return(structure_list)
}


# Plot componentz vs size
plot.structureCLR <- function(structure_list, loess = FALSE){
  if(loess){
    ggplot(structure_list$cv, aes(x = ID, y = vcountcomp)) + geom_point() + geom_line() +
      theme_minimal() + xlab("Component") + ylab("Size") + geom_smooth(method = 'loess')
  }else{
    ggplot(structure_list$cv, aes(x = ID, y = vcountcomp)) + geom_point() + geom_line() +
      theme_minimal() + xlab("Component") + ylab("Size")
  }
}

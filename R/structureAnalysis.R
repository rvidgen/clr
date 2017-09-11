#' Perform Structure Analysis
#'
#' Perform structure analysis on impactCLR object
#'
#' @param articles_df Path to files
#' @return List
#' @examples
#' structure <- structureAnalysis(impact_object = impact)
#'
#' # Components vs Size
#' plot(structure, loess = T)
#'
#' # Plot structure node : edges
#' showGraphs(structure)
#'
#' # Plot components
#' graphComponents(structure, n = 5)
#'
#' @import dplyr
#' @import igraph
#' @import ggraph
#' @import networkD3


structureAnalysis <- function(impact_object){

  # Create edges df
  edges_df <- impact_object[['authors_df']] %>%
    group_by(Id) %>%
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
  comps <- t(sapply(seq_along(graph_decomp), function(x) c(Subgraph = x, NodeCount = vcount(graph_decomp[[x]]), EdgeCount = ecount(graph_decomp[[x]]))))
  cv <- as.data.frame(sapply(graph_decomp, vcount))
  cv$comp <- rownames(cv)
  colnames(cv) <- c("vcountcomp", "comp")
  cv <- cv[order(cv$vcountcomp,decreasing=TRUE),]
  cv$ID <- seq.int(nrow(cv))
  main <- as.numeric(cv$comp[1])
  deg <- as.data.frame(degree(graph_decomp[[main]]))
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


  # Create structureCLR object
  structure_list <- list(cv = cv, component_cites = component_cites, graph_df = graph_df, edges_df = edges_df, dg_comps = dg, main_comp = main)
  class(structure_list) <- "structureCLR"

  return(structure_list)
}

# Str
str.structureCLR <- function(structure_list){
  str_object <- structure_list[c('cv', 'component_cites', 'graph_df', 'edges_df')]
  return(utils::str(str_object))
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

showGraphs <- function(structure_list, main_only = FALSE, dynamic = FALSE){
  if(main_only){
    p1 <- ggraph(structure_list$dg_comps[[structure_list$main]], layout = 'kk') +
        geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
        geom_node_point() +
        theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
  }else{
    if(dynamic){
      vertices <- data.frame(
        name = V(structure_list$graph_df)$name,
        group = edge.betweenness.community(structure_list$graph_df)$membership,
        betweenness = (betweenness(structure_list$graph_df,directed=F,normalized=T)*115)+0.1
      )

      df <- data.frame(source.index = match(structure_list$edges_df$Auth1, vertices$name)-1,
                       target.index = match(structure_list$edges_df$Auth2, vertices$name)-1)

      p1 <- forceNetwork(Links = df, Nodes = vertices,
                         Source = 'source.index', Target = 'target.index',
                         NodeID = 'name',
                         Group = 'group',
                         charge = -1,
                         linkDistance = 20,
                         zoom = F,
                         opacity = 1,
                         fontSize=24)
    }else{
      p1 <- ggraph(structure_list$graph_df) +
            geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
            geom_node_point() +
            theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
    }
  }
  show(p1)
}

graphComponents <- function(structure_list, n = 1){
  ggraph(structure_list$dg_comps[[as.numeric(structure_list$cv$comp[structure_list$cv$ID==n])]], layout = 'kk') +
    geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
    geom_node_point() +
    geom_node_text(aes(label = name)) +
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white') +
    ggtitle(paste0("Co-authorship component ", n))
}

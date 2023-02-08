#' Generate a Guidelines for Reporting About Network Data (GRAND) narrative summary
#'
#' The `grand.text` function writes a narrative summary of GRAND attributes that were
#'     added to an `igraph` object using [grand()].
#'
#' @param G An \code{\link{igraph}} object with GRAND attributed
#' @param digits numeric: number of decimal places to report
#'
#' @return string: Narrative summary of G
#' @export
#'
#' @examples
#' #A weighted, directed network
#' data(airport)  #Load example data
#' narrative <- grand.text(airport)  #Generate narrative
#'
#' #A bipartite network
#' data(cosponsor)  #Load example data
#' narrative <- grand.text(cosponsor)  #Generate narrative
#'
#' #A signed network
#' data(senate)  #Load example data
#' narrative <- grand.text(senate)  #Generate narrative
grand.text <- function(G, digits = 3) {

  #### Check input ####
  if (!methods::is(G, "igraph")) {stop("The input must be an igraph object")}
  if (!"grand" %in% names(igraph::get.graph.attribute(G))) {stop("This graph does not have GRAND attributes. Please run `grand()` first.")}
  bipartite <- igraph::is_bipartite(G)

  #### Name, Characteristics, Nodes, Edges ####
  narrative <- ""

  if (igraph::is_directed(G)) {directed <- "directed"} else {directed <- "undirected"}
  if ("weight" %in% names(igraph::get.edge.attribute(G))) {if (all(igraph::E(G)$weight %in% c(-1,1))) {weighted <- "signed"} else {weighted <- "weighted"}} else {weighted <- "unweighted"}

  if (is.na(G$grand$name)) {start <- paste0("This ", directed, " and ", weighted, " network represents ")}
  if (!is.na(G$grand$name)) {start <- paste0("The ", G$grand$name, " is a ", directed, " and ", weighted, " network that represents ")}

  if (!bipartite & weighted != "signed") {narrative <- paste0(narrative, start, igraph::gorder(G), " ", tolower(G$grand$vertex1), " connected by ", igraph::gsize(G)," ", tolower(G$grand$edge.pos),". ")}
  if (!bipartite & weighted == "signed") {narrative <- paste0(narrative, start, igraph::gorder(G), " ", tolower(G$grand$vertex1), " connected by ", sum(igraph::E(G)$weight==1)," ", tolower(G$grand$edge.pos)," and ", sum(igraph::E(G)$weight==-1)," ", tolower(G$grand$edge.neg),". ")}
  if (bipartite & weighted != "signed") {narrative <- paste0(narrative, start, sum(igraph::V(G)$type==FALSE), " ", tolower(G$grand$vertex1), " and ", sum(igraph::V(G)$type==TRUE), " ", tolower(G$grand$vertex2), " connected by ", igraph::gsize(G), " ", tolower(G$grand$edge.pos),". ")}
  if (bipartite & weighted == "signed") {narrative <- paste0(narrative, start, sum(igraph::V(G)$type==FALSE), " ", tolower(G$grand$vertex1), " and ", sum(igraph::V(G)$type==TRUE), " ", tolower(G$grand$vertex2), " connected by ", sum(igraph::E(G)$weight==1)," ", tolower(G$grand$edge.pos)," and ", sum(igraph::E(G)$weight==-1)," ", tolower(G$grand$edge.neg),". ")}

  #### Node Missingness ####
  #Unbounded
  if (G$grand$vertex1.total==0 & G$grand$vertex2.total==0) {
    narrative <- paste0(narrative, "Its node set is unbounded, so node missingness cannot be evaluated. ")
  }

  if (!bipartite) {
    #No missing
    if (G$grand$vertex1.total == igraph::gorder(G)) {narrative <- paste0(narrative, "All ", tolower(G$grand$vertex1), " included in the network's boundary are represented as nodes (i.e., no node missingness). ")}

    #Some missing
    if (G$grand$vertex1.total > igraph::gorder(G)) {
      missing <- G$grand$vertex1.total - igraph::gorder(G)
      narrative <- paste0(narrative, "It is missing ", missing, " ", tolower(G$grand$vertex1), " (", round((missing / G$grand$vertex1.total)*100,2), "%). " )
    }
  }

  if (bipartite) {

    #No missing
    if (G$grand$vertex1.total == sum(igraph::V(G)$type==FALSE) &  G$grand$vertex2.total == sum(igraph::V(G)$type==TRUE)) {
      narrative <- paste0(narrative, "All ", tolower(G$grand$vertex1), " and ", tolower(G$grand$vertex1), " included in the network's boundary are represented as nodes (i.e., no node missingness). ")
    }

    #Both missing
    if (G$grand$vertex1.total > sum(igraph::V(G)$type==FALSE) & G$grand$vertex2.total > sum(igraph::V(G)$type==TRUE)) {
      missing1 <- G$grand$vertex1.total - sum(igraph::V(G)$type==FALSE)
      missing2 <- G$grand$vertex2.total - sum(igraph::V(G)$type==TRUE)
      narrative <- paste0(narrative, "It is missing ", missing1, " ", tolower(G$grand$vertex1), " (", round((missing1 / G$grand$vertex1.total)*100,2), "%) and ", missing2, " ", tolower(G$grand$vertex2), " (", round((missing2 / G$grand$vertex2.total)*100,2), "%). ")
    }

    #Vertex1 missing
    if (G$grand$vertex1.total > sum(igraph::V(G)$type==FALSE) & G$grand$vertex2.total <= sum(igraph::V(G)$type==TRUE)) {
      missing1 <- G$grand$vertex1.total - sum(igraph::V(G)$type==FALSE)
      narrative <- paste0(narrative, "It is missing ", missing1, " ", tolower(G$grand$vertex1), " (", round((missing1 / G$grand$vertex1.total)*100,2), "%). ")
    }

    #Vertex2 missing
    if (G$grand$vertex1.total <= sum(igraph::V(G)$type==FALSE) & G$grand$vertex2.total > sum(igraph::V(G)$type==TRUE)) {
      missing2 <- G$grand$vertex2.total - sum(igraph::V(G)$type==TRUE)
      narrative <- paste0(narrative, "It is missing ", missing2, " ", tolower(G$grand$vertex2), " (", round((missing2 / G$grand$vertex2.total)*100,2), "%). ")
    }
  }

  #### Edge weights ####
  if (weighted == "weighted") {narrative <- paste0(narrative, "The edges are weighted by ", tolower(G$grand$weight), ", which was measured on a ", tolower(G$grand$measure), " scale. ")}

  #### Measurement ####
  narrative <- paste0(narrative, "These data were collected in ", G$grand$year, " using ", tolower(G$grand$mode), " methods. ")

  #### Topology ####
  #Create unweighted undirected graph for computing topology
  Gstar <- igraph::as.undirected(G, mode = "collapse")
  if ("weight" %in% names(igraph::get.edge.attribute(G))) {Gstar <- igraph::delete_edge_attr(Gstar, "weight")}

  while (length(G$grand$topology) != 0) {  #Loop over desired topological characteristics
    metric <- G$grand$topology[1]

    if (metric == "density") {
      narrative <- paste0(narrative, paste0("The network's density is ",round(igraph::edge_density(Gstar),digits), ". "))
    }

    if (metric == "mean degree") {
      narrative <- paste0(narrative, paste0("The network's mean degree is ",round(mean(igraph::degree(Gstar)),digits), ". "))
    }

    if (metric == "transitivity") {
      narrative <- paste0(narrative, paste0("The network's transitivity is ",round(igraph::transitivity(Gstar, type = "global"),digits), ". "))
    }

    if (metric == "clustering coefficient") {
      narrative <- paste0(narrative, paste0("The network's clustering coefficient is ",round(igraph::transitivity(Gstar, type = "localaverage"),digits), ". "))
    }

    if (metric == "mean path length") {
      narrative <- paste0(narrative, paste0("The network's mean path length is ",round(igraph::mean_distance(Gstar),digits), ". "))
    }

    if (metric == "diameter") {
      narrative <- paste0(narrative, paste0("The network's diameter is ",round(igraph::diameter(Gstar),digits), ". "))
    }

    if (metric == "structural balance") {
      if ("weight" %in% names(igraph::get.edge.attribute(G))) {mat <- igraph::as_adjacency_matrix(G, sparse = FALSE, attr = "weight")} else {mat <- igraph::as_adjacency_matrix(G, sparse = FALSE)}
      trace <- function(x){sum(diag(x))}  #Find matrix trace
      matcube <- function(x){x%*%x%*%x}  #Find matrix^3
      balance <- ((trace(matcube(mat)) + trace(matcube(abs(mat))))/3) / ((2 * trace(matcube(abs(mat))))/3)
      narrative <- paste0(narrative, paste0("The network's degree of balance is ",round(balance,digits), ". "))
    }

    if (metric == "efficiency") {
      narrative <- paste0(narrative, paste0("The network's efficiency is ",round(igraph::global_efficiency(Gstar),digits), ". "))
    }

    if (metric == "degree centralization") {
      narrative <- paste0(narrative, paste0("The network's degree centralization is ",round(igraph::centr_degree(Gstar)$centralization,digits), ". "))
    }

    if (metric == "number of components") {
      narrative <- paste0(narrative, paste0("The network contains ", round(igraph::count_components(Gstar),digits), " components. "))
    }

    if (metric == "modularity") {
      narrative <- paste0(narrative, paste0("A Leiden partition of this network has a modularity of ", round(igraph::modularity(Gstar, igraph::cluster_leiden(Gstar, objective_function = "modularity")$membership),digits), ". "))
    }

    if (metric == "number of communities") {
      narrative <- paste0(narrative, paste0("A Leiden partition of this network yields ", round(igraph::cluster_leiden(Gstar, objective_function = "modularity")$nb_clusters,digits), " communities. "))
    }

    if (metric == "degree distribution") {
      pl <- igraph::fit_power_law(igraph::degree(Gstar), implementation = "plfit")
      narrative <- paste0(narrative, paste0("Fitting a power law to this network's degree distribution implies that k^-", round(pl$alpha,digits), " for k >= ", pl$xmin, ". "))
    }

    G$grand$topology <- G$grand$topology[-1]
  }

  #### Open Science ####
  if (!is.na(G$grand$doi) & !is.na(G$grand$url)) {narrative <- paste0(narrative, paste0("This network is described in ", G$grand$doi, " and is available from ", G$grand$url, "."))}
  if (is.na(G$grand$doi) & !is.na(G$grand$url)) {narrative <- paste0(narrative, paste0("This network is available from ", G$grand$url, "."))}
  if (!is.na(G$grand$doi) & is.na(G$grand$url)) {narrative <- paste0(narrative, paste0("This network is described in ", G$grand$doi, "."))}

  return(narrative)
}

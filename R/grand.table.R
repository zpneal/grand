#' Generate a Guidelines for Reporting About Network Data (GRAND) summary table
#'
#' The `grand.table` function plots a tabular summary of GRAND attributes that were
#'     added to an `igraph` object using [grand()].
#'
#' @param G An \code{\link{igraph}} object with GRAND attributed
#' @param digits numeric: number of decimal places to report
#'
#' @return A plot
#' @export
#'
#' @examples
#' #A weighted, directed network
#' data(airport)  #Load example data
#' grand.table(airport)  #Generate narrative
#'
#' #A bipartite network
#' data(cosponsor)  #Load example data
#' grand.table(cosponsor)  #Generate narrative
#'
#' #A signed network
#' data(senate)  #Load example data
#' grand.table(senate)  #Generate narrative
grand.table <- function(G, digits = 3) {

  #### Check graph ####
  if (!methods::is(G, "igraph")) {stop("The input must be an igraph object")}
  if (!"grand" %in% names(igraph::get.graph.attribute(G))) {stop("This graph does not have GRAND attributes. Please run `grand()` first.")}
  bipartite <- igraph::is.bipartite(G)
  if ("weight" %in% names(igraph::get.edge.attribute(G))) {if (all(igraph::E(G)$weight %in% c(-1,1))) {weighted <- "signed"} else {weighted <- "weighted"}} else {weighted <- "unweighted"}

  #### Determine number of rows ####
  rows <- 0
  rows <- 3 + (!is.na(G$grand$name))*1  #Header
  if (!bipartite) {rows <- rows + 2} else {rows <- rows + 4} #Nodes
  if (weighted == "signed") {rows <- rows + 4} else {rows <- rows + 3}  #Edges
  rows <- rows + length(G$grand$topology)  #Topology
  rows <- rows + (!is.na(G$grand$doi))*.5 + (!is.na(G$grand$url))*.5  #Open Science
  rows <- rows + 0  #Footer
  row <- rows  #Current row counter

  #### Setup ####
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::par(mar = c(0,0,0,0), oma = c(0.1,0.1,0.1,0.1))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0,5), ylim = c(0,rows))
  graphics::box(col = "black", lwd = 1)  #Border

  #### Header ####
  graphics::text(2.5, row-.5, "Network Facts", cex = 2.5, font = 2)
  row <- row - 2

  if (!is.na(G$grand$name)) {
    graphics::text(0, row, G$grand$name, cex = 1, pos = 4, offset = 0)
    row <- row - 1
  }

  graphics::text(0, row, paste0("Source: ", G$grand$mode), cex = 1, pos = 4, offset = 0)
  graphics::text(5, row, paste0("Year: ", G$grand$year), cex = 1, pos = 2, offset = 0)
  graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 10, lend = 2)
  row <- row - 1

  #### Nodes ####
  #Unipartite
  #Definition of node1
  graphics::text(0, row-.1, "Nodes", cex = 1, pos = 4, offset = 0, font = 2)
  graphics::text(1, row-.1, tools::toTitleCase(G$grand$vertex1), cex = 1, pos = 4, offset = 0)
  if (!bipartite) {graphics::text(5, row-.1, igraph::gorder(G), cex = 1, pos = 2, offset = 0)}
  if (bipartite) {graphics::text(5, row-.1, sum(igraph::V(G)$type==FALSE), cex = 1, pos = 2, offset = 0)}
  graphics::segments(x0 = 1, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
  row <- row-1

  #Rate of node1 missingness
  graphics::text(.75, row-.1, "     Missingness", cex = 1, pos = 4, offset = 0, font = 3)
  if (G$grand$vertex1.total == 0) {graphics::text(5, row-.1, "Unbounded", cex = 1, pos = 2, offset = 0)}
  if (!bipartite & G$grand$vertex1.total != 0) {graphics::text(5, row-.1, paste0(round(((G$grand$vertex1.total - igraph::gorder(G)) / G$grand$vertex1.total)*100,digits), "%"), cex = 1, pos = 2, offset = 0)}
  if (bipartite & G$grand$vertex1.total != 0) {graphics::text(5, row-.1, paste0(round(((G$grand$vertex1.total - sum(igraph::V(G)$type==FALSE)) / G$grand$vertex1.total)*100,digits), "%"), cex = 1, pos = 2, offset = 0)}
  graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
  row <- row-1

  #Bipartite
  if (bipartite) {
    #Definition of node2
    graphics::text(1, row-.1, tools::toTitleCase(G$grand$vertex2), cex = 1, pos = 4, offset = 0)
    graphics::text(5, row-.1, sum(igraph::V(G)$type==TRUE), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 1, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1

    #Rate of node2 missingness
    graphics::text(.75, row-.1, "     Missingness", cex = 1, pos = 4, offset = 0, font = 3)
    if (G$grand$vertex2.total == 0) {graphics::text(5, row-.1, "Unbounded", cex = 1, pos = 2, offset = 0)}
    if (G$grand$vertex2.total != 0) {graphics::text(5, row-.1, paste0(round(((G$grand$vertex2.total - sum(igraph::V(G)$type==TRUE)) / G$grand$vertex2.total)*100,digits), "%"), cex = 1, pos = 2, offset = 0)}
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  #### Edges ####
  graphics::segments(x0 = 0, x1 = 5, y0 = row+.5, y1 = row+.5, lwd = 5, lend = 2)    #Thick line above

  #Definition of edge.pos
  graphics::text(0, row-.1, "Edges", cex = 1, pos = 4, offset = 0, font = 2)
  graphics::text(1, row-.1, tools::toTitleCase(G$grand$edge.pos), cex = 1, pos = 4, offset = 0)
  if (weighted!="signed") {graphics::text(5, row-.1, igraph::gsize(G), cex = 1, pos = 2, offset = 0)}
  if (weighted=="signed") {graphics::text(5, row-.1, sum(igraph::E(G)$weight==1), cex = 1, pos = 2, offset = 0)}
  row <- row-1

  #Definition of edge.neg
  if (weighted=="signed") {
    graphics::segments(x0 = 1, x1 = 5, y0 = row+.5, y1 = row+.5, lwd = 1, lend = 2)  #Partial line above
    graphics::text(1, row-.1, tools::toTitleCase(G$grand$edge.neg), cex = 1, pos = 4, offset = 0)
    graphics::text(5, row-.1, sum(igraph::E(G)$weight==-1), cex = 1, pos = 2, offset = 0)
    row <- row-1
  }

  #Edge directedness
  graphics::segments(x0 = 0, x1 = 5, y0 = row+.5, y1 = row+.5, lwd = 1, lend = 2)  #Full line above
  graphics::text(0, row-.1, "     Reciprocity", cex = 1, pos = 4, offset = 0, font = 3)
  if (!igraph::is.directed(G)) {graphics::text(5, row-.1, "Undirected", cex = 1, pos = 2, offset = 0)}
  if (igraph::is.directed(G)) {graphics::text(5, row-.1, round(igraph::reciprocity(G),digits), cex = 1, pos = 2, offset = 0)}
  graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
  row <- row-1

  #Edge weight definition and measurement
  graphics::text(0, row-.1, "     Weights", cex = 1, pos = 4, offset = 0, font = 3)
  if (weighted=="unweighted") {graphics::text(5, row-.1, "Unweighted", cex = 1, pos = 2, offset = 0)}
  if (weighted=="signed") {graphics::text(5, row-.1, "Signed", cex = 1, pos = 2, offset = 0)}
  if (weighted=="weighted") {graphics::text(5, row-.1, paste0(tools::toTitleCase(G$grand$measure), " ", tools::toTitleCase(G$grand$weight)), cex = 1, pos = 2, offset = 0)}
  graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
  row <- row-1

  #### Topology ####
  graphics::segments(x0 = 0, x1 = 5, y0 = row+.5, y1 = row+.5, lwd = 5, lend = 2)    #Thick line above

  #Create unweighted undirected graph for computing topology
  Gstar <- igraph::as.undirected(G, mode = "collapse")
  if ("weight" %in% names(igraph::get.edge.attribute(G))) {Gstar <- igraph::delete_edge_attr(Gstar, "weight")}

  while (length(G$grand$topology) != 0) {
    metric <- G$grand$topology[1]

  if (metric == "density") {
    graphics::text(0, row-.1, "Density", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(igraph::edge_density(Gstar),digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "mean degree") {
    graphics::text(0, row-.1, "Mean degree", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(mean(igraph::degree(Gstar)),digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "transitivity") {
    graphics::text(0, row-.1, "Transitivity", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(igraph::transitivity(Gstar, type = "global"),digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "clustering coefficient") {
    graphics::text(0, row-.1, "Clustering coefficient", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(igraph::transitivity(Gstar, type = "localaverage"),digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "mean path length") {
    graphics::text(0, row-.1, "Mean path length", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(igraph::mean_distance(Gstar),digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "diameter") {
    graphics::text(0, row-.1, "Diameter", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(igraph::diameter(Gstar),digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "structural balance") {
    graphics::text(0, row-.1, "Balance", cex = 1, pos = 4, offset = 0, font = 1)
    if ("weight" %in% names(igraph::get.edge.attribute(G))) {mat <- igraph::as_adjacency_matrix(G, sparse = FALSE, attr = "weight")} else {mat <- igraph::as_adjacency_matrix(G, sparse = FALSE)}
    trace <- function(x){sum(diag(x))}  #Find matrix trace
    matcube <- function(x){x%*%x%*%x}  #Find matrix^3
    balance <- ((trace(matcube(mat)) + trace(matcube(abs(mat))))/3) / ((2 * trace(matcube(abs(mat))))/3)
    graphics::text(5, row-.1, round(balance,digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "efficiency") {
    graphics::text(0, row-.1, "Efficiency", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(igraph::global_efficiency(Gstar),digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "degree centralization") {
    graphics::text(0, row-.1, "Degree centralization", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(igraph::centr_degree(Gstar)$centralization,digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "number of components") {
    graphics::text(0, row-.1, "Components", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(igraph::count_components(Gstar),digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "modularity") {
    graphics::text(0, row-.1, "Modularity", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(igraph::modularity(Gstar, igraph::cluster_leiden(Gstar, objective_function = "modularity")$membership),digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "number of communities") {
    graphics::text(0, row-.1, "Communities", cex = 1, pos = 4, offset = 0, font = 1)
    graphics::text(5, row-.1, round(igraph::cluster_leiden(Gstar, objective_function = "modularity")$nb_clusters,digits), cex = 1, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }

  if (metric == "degree distribution") {
    graphics::text(0, row-.1, "Degree distribution", cex = 1, pos = 4, offset = 0, font = 1)
    pl <- igraph::fit_power_law(igraph::degree(Gstar), implementation = "plfit")
    graphics::text(5, row-.1, bquote(italic(k)^-.(round(pl$alpha,digits))~"for"~italic(k)~''>=''~.(pl$xmin)), cex = .75, pos = 2, offset = 0)
    graphics::segments(x0 = 0, x1 = 5, y0 = row-.5, y1 = row-.5, lwd = 1, lend = 2)
    row <- row-1
  }
    G$grand$topology <- G$grand$topology[-1]
  }

  #### Open Science ####
  graphics::segments(x0 = 0, x1 = 5, y0 = row+.5, y1 = row+.5, lwd = 5, lend = 2)  #Thick line above
  if (!is.na(G$grand$doi)) {
    graphics::text(0, row+.2, paste0("Described in ", G$grand$doi), cex = .5, pos = 4, offset = 0, font = 1)
    row <- row-.5
    }
  if (!is.na(G$grand$url)) {
    graphics::text(0, row+.2, paste0("Available from ", G$grand$url), cex = .5, pos = 4, offset = 0, font = 1)
    row <- row-.5
    }

  #### Footer ####
  graphics::segments(x0 = 0, x1 = 5, y0 = row+.5, y1 = row+.5, lwd = 1, lend = 2)  #Thin line above
  graphics::text(0, row, bquote("Formatted"~"using"~bold(G)*"uidelines"~"for"~bold(R)*"eporting"~bold(A)*"bout"~bold(N)*"etwork"~bold(D)*"ata (GRAND; Neal, 2023)"), cex = .5, pos = 4, offset = 0)

}

#' US Air Traffic Network
#'
#' @description A weighted and directed network of passenger air traffic in the United States in 2019.
#'    Each edge represents a single takeoff and landing, and therefore does not consider possible layovers,
#'    connecting flights, round trips, etc. This is the directed version of the undirected air traffic network
#'    used by Neal (2022) to illustrate `backbone::disparity()`. GRAND attributes have already been added
#'    using [grand()].
#'
#' @references Neal, Z. P. (2022). backbone: An R Package to Extract Network Backbones. *PLOS ONE, 17*, e0269137. \doi{10.1371/journal.pone.0269137}
#'
#' @format igraph object
#'
"airport"

#' US Senate Co-Sponsorship Network
#'
#' @description A bipartite network representing US Senators' (co-)sponsorship of Senate Bills during the 116th
#'    session (2019-2020). It was obtained using `incidentally::incidence.from.congress()` following the
#'    procedure described by Neal (2022). GRAND attributes have already been added using [grand()].
#'
#' @references Neal, Z. P. (2022). Constructing legislative networks in R using incidentally and backbone. *Connections, 42*, 1-9. \doi{10.2478/connections-2019.026}
#'
#' @format igraph object
#'
"cosponsor"

#' US Senate Network
#'
#' @description A signed network representing US Senators' alliances and antagonisms, inferred from
#'    [cosponsor()] using `backbone::sdsm()` following the procedure described by Neal (2022).
#'    GRAND attributes have already been added using [grand()].
#'
#' @references Neal, Z. P. (2022). Constructing legislative networks in R using incidentally and backbone. *Connections, 42*, 1-9. \doi{10.2478/connections-2019.026}
#'
#' @format igraph object
#'
"senate"

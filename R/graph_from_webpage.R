#' Graph_from_webpage function : This function allows to get a graph object from a certain webpage.
#' 
#' @param target The target file you are targeting (Usually a web adress).
#' @export 
#' 
#' @importFrom stats na.omit
#' @importFrom igraph graph_from_edgelist
#' @import igraph
#' 
#' @examples 
#' 
#' target = "https://en.wikipedia.org/wiki/R_(programming_language)"
#'
#' g <- graph_from_webpage(target)
#' 
#' plot(g,
#'     layout = igraph::layout_with_fr,
#'     vertex.size=4,
#'     vertex.label=NA,
#'     vertex.label.dist=0.5,
#'     vertex.color="red",
#'     edge.arrow.size=0.5)

graph_from_webpage = function(target) {
    el <- edgelist_of(target)
    el <- na.omit(el)
    graph_from_edgelist(as.matrix(el[,c("FROM","TO")]), directed = FALSE)
}

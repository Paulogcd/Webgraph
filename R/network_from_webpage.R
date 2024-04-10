
#' Network_from_webpage1 function : This function allows to get a graph object from a certain webpage. The network_from_webpage function differs from the get_graph function due to the fact that get_graph only gets the graph for one page, and network_from_webpage gets the graph of the connected pages also.
#' 
#' @param target The target file you are targeting (Usually a web adress).
#' @param iteration The number of iteration you want. If set to one, this function is equivalent to the "graph_from_webpage() function."
#' @export 
#' 
#' @importFrom stats na.omit
#' @importFrom igraph graph_from_edgelist
#' @import igraph
#' 
#' @examples 
#' 
#' target = "https://www.google.com/"
#' 
#' N <- network_from_webpage1(target, iteration = 1)
#' class(N)
#' dim(N)
#'
#' N2 <- network_from_webpage1(target, iteration = 2)
#' class(N2)
#' dim(N2)
#' 

# With the first iteration being the equivalent as the graph of the intial page
network_from_webpage1 = function (target, iteration = 1) {
   start_time <- Sys.time()

    # Intitialisation :
    all_el <- list()
    new_targets <- list()

    # For the first level of depth :
    all_el[[1]] <- list()
    new_targets[[1]] <- list()
    all_el[[1]] <- edgelist_of(target)
    new_targets[[1]] <- unlist(all_el[[1]]["TO"], use.names=FALSE)

    # Collect data for the second level of depth and the following :
    if (iteration > 1) {
        for (a in (2:iteration)){
            all_el[[a]] <- list()
            new_targets[[a]] <- list()
            for (i in (1:length(new_targets[[a-1]]))){
                all_el[[a]][[i]] <- list()
                all_el[[a]][[i]] <- edgelist_of(new_targets[[a-1]][i])
                new_targets[[a]] <- c(unlist(new_targets[[a]]),unlist(all_el[[a]][[i]]["TO"],use.names=FALSE))
            }
        }
    }
    
    # Now we have to return the data :

    # The edgelist

    # For the first iteration :
    df <- data.frame(all_el[[1]])

    # For the second and other iterations :
    if (iteration > 1) {
        for (i in (2:length(all_el))){
            for (j in (1:length(all_el[[i]]))){
                df <- rbind(df,all_el[[i]][[j]], row.names = FALSE)
            }
        }
    }

    end_time <- Sys.time()
    time_taken <- end_time - start_time
    print(time_taken)

    return(df)
}

    # DOES NOT WORK : 
    # N3 <- network_from_webpage1(target, iteration = 3)
    # Error in data.frame(href, url) : 
    # arguments imply differing number of rows: 0, 1
    # class(N3)
    # dim(N3)


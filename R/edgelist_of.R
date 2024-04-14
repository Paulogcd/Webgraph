
#' Edgelist_of function
#' 
#' This function allows to get an edgelist of the links of a certain webpage. The edge list also includes a column of source, that takes E if external page, and I if it is an internal page.
#' 
#' @param target The target file you are targeting (Usually a web adress).
#' @param target_title Logical value set by default as FALSE. If set to TRUE, it will get the edgelist with the title of the target instead of its url.
#' @param links_title Logical value set by default as FALSE. If set to TRUE, it will return the edgelist with the title of all links. If set to TRUE, it can take a long time for large datasets.
#' @param full_URL A logical value indicating if you want to return the full URL adresses of the links.
#' @param origin A logical value indicating if you want to return the origin of the link (external/internal) in the final dataframe.
#' @return Returns a dataframe object with the information of the edgelist of the connections of the targeted webpage. 
#' 
#' @export 
#' 
#' @examples 
#' 
#' target = "https://en.wikipedia.org/wiki/R_(programming_language)"
#' el <- edgelist_of(target)
#' summary (el)

edgelist_of = function (target,
    target_title = F,
    full_URL = T,
    links_title = F, 
    origin = F)
{
    start_time <- Sys.time()

    d <- links_of(target, full_URL =  full_URL, titles = links_title, origin = origin)

    # If we do not want the name of the base node as a title, we repeat the target name in the edgelist :
    if (target_title == FALSE) {
        FROM <- rep(target, nrow(d))
    }
    # If we want the name of the base node as a title, we repeat the scrapped name in the edgelist :
    if (target_title == TRUE)
    {
        title <- title_of(target)
        FROM <- rep(title, nrow(d))
    }

    # If we want the list of the full URLs :
    if (full_URL == TRUE)
    {
        TO <- unlist(d["url"])
        names(TO) <- "TO"
    }
    if (full_URL == FALSE)
    {
        TO <- unlist(d["href"])
        names(TO) <- "TO"
    }

    # If we want the list of web pages to be shown without their title 
    if (links_title == F)
    {
        # If you want to include the origin of the links
        if (origin == T){
            source <- d["source"]
                end_time <- Sys.time()
                time_taken <- end_time - start_time
                message("Time for edgelist_of ", target,": ", time_taken, " seconds.")
            return(data.frame(FROM,TO,source))
        }
        # If you do not want to include the origin of the links
        if (origin == F){
                end_time <- Sys.time()
                time_taken <- end_time - start_time
                message("Time for edgelist_of ", target,": ", time_taken, " seconds.")
            return(data.frame(FROM,TO))
        }
    }
    # If we want the list of web pages to be shown with their title 
    if (links_title == T)
    {
        titles_of_links <- c()
        for (i in (1:length(TO))) {
            titles_of_links[i] <- title_of(TO[i])
        }
        # If you want to include the origin of the links
        if (origin == T){
            source <- d["source"]
                end_time <- Sys.time()
                time_taken <- end_time - start_time
                message("Time for edgelist_of ", target,": ", time_taken, " seconds.")
            return(data.frame(FROM,TO,titles_of_links, source))
        }
        # If you do not want to include the origin of the links
        if (origin == F){
                end_time <- Sys.time()
                time_taken <- end_time - start_time
                message("Time for edgelist_of ", target,": ", time_taken, " seconds.")
            return(data.frame(FROM,TO,titles_of_links))
        }
    }
}



#' Title_of function : This function gets the title of a certain webpage.
#' 
#' @param target The target file you are targeting (Usually a web adress).
#' @param collapse A logical that indicates if you want to merge titles in case of the webpage having more than one title element.
#' @export 
#' 
#' @return The vector of titles. 
#' 
#' @importFrom rvest html_text
#' @importFrom rvest html_elements
#' @importFrom rvest read_html
#' 
#' @examples 
#' target <- "https://en.wikipedia.org/wiki/R_(programming_language)"
#' title_of(target)

# This function gives the title of a webpage.
title_of = function (target, collapse = T) {
    title <- tryCatch( 
        html_text(html_elements(read_html(target), "title")),
        error = function(e){NA})

        if (length(title)!=1) {
            if (collapse == T) {
                warning(paste0("Warning : title length is more than one for '",target,"'.Since collapse is TRUE, titles were merged"))
                title <- paste(title, collapse=' ')
            }
            else if (collapse == F) {
                warning(paste0("Warning : title length is more than one for '",target,"'.Since collapse is FALSE, titles were ignored and set to NA."))
                title <- NA
            }
        }
        return(title)
}
### NOTE !!! : The function title_of does not take into account the encoding, which might be a problem in the future.

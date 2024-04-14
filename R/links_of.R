
#' Links_of function
#' 
#' This function gets the links of a certain webpage.
#' 
#' @param target The target file you are targeting (Usually a web adress).
#' @param full_URL A logical value indicating if you want to return the full URL adresses of the links.
#' @param titles A logical value indicating if you want to return the titles of the links of the target webpage.
#' @param origin A logical value indicating if you want to return the origin of the link (external/internal) in the final dataframe.
#' @return Returns a dataframe of the links of the targeted webpage. 
#' 
#' @export 
#' 
#' @return A dataframe with the links of a certain webpage, its source nature (external or internal), and some additional informaiton.
#' 
#' @importFrom rvest html_attr
#' @importFrom stringr str_starts
#' @importFrom stringr str_ends
#' 
#' @examples 
#' target = "https://en.wikipedia.org/wiki/R_(programming_language)"
#' links_of(target)
#' 

links_of = function(target, full_URL = T, titles = F, origin = F) {
    
    # If we want the titles, we need the list of full URLs :
    if (titles == T) {full_URL <- T}

    # Get the HTML code of the target as a xml_document and xml_node document.
    html_page <- tryCatch(read_html(target), error = function(e) {NA})
    if (!is.na(html_page))
    {
        a_elements <- html_elements(html_page, "a") # Select the HTML elements with <a> balises
        href <- as.vector(html_attr(a_elements, name = "href")) # Select the hyperlink of the <a> balises
        source <- c()
        pattern <- c("http://","https://","www.")
        
        # We build the source vector :
        for (i in (seq_along(href)))
        {
            # If it is an external page, set the source to "E"
            # We defin an external link as beginning with "http://" or similar terms.
            if (isTRUE(str_starts(href[i], paste(pattern, collapse = "|"))))
            {
                source[i] <- "E"
            }
            # If it is an page of the same site, set the source to "I"
            else { 
                source[i] <- "I"
            }
        }

        # If we want the full url for the internal links :
        if (full_URL == T) {
            # We create an empty vector
            url <- c()
            # For all elements of the href list :
            for (i in (seq_along(source))) {
                # If it's an external link, we just put the link in the url vector :
                if (source[i] == "E") {
                    url[i] <- href[i]
                }
                # If it's an internal link, we put the url of the target before the scrapped href value :
                if (source[i] == "I") {
                    # We first check if both the target and the href starts and end with an "/" : 
                    if(!is.na(target) & !is.na(href[i])){
                        if (str_ends(target, "/") & str_starts(href[i],"/")) {
                                reduced_href <- sub('.', '', href[i])
                                tryCatch(url[i] <- paste0(target,reduced_href), error = function(e) {NA})
                        }
                        else {
                        tryCatch(url[i] <- paste0(target,href[i]), error = function(e) {NA})
                        }
                    }
                    else if (is.na(target) | is.na(href[i])){
                        url[i] <- NA
                    }
                    else {
                        url[i] <- NA
                    }
                }
            }
        }
        # Thus, in the end, we have the url vector filled with all the complete links

        # To check if they have the same length :
        if (length(href) != length(url)) {
            warning("The number of hrefs is not the same as the number of urls for ", target)
        }

        # Plus, if we want the titles of the pages : 
        if (titles == T) {
            titles <- c()
            for (i in (seq_along(url))){
                titles[i] <- title_of(url[i])
            }
            if (origin == T) {
                df <- data.frame(href, source, url, titles)
            }
            if (origin == F){
                df <- data.frame(href, url, titles)
            }
            return (df)
        }

        # If we don't want the titles : 
        else if (titles == F){
            if (origin == T) {
                df <- data.frame(href, source, url)
            }
            if (origin == F){
                df <- data.frame(href, url) # This gives an error with network_from_webpage1 at iteration = 3 ; 
                # Error in data.frame(href, url) : arguments imply differing number of rows: 0, 1
            }
            return (df)
        }

        if (origin == T) {
            df <- data.frame(href, source)
        }
        if (origin == F){
            df <- data.frame(href)
        }
        return (df)
        }
    else {return(NA)}
}


# now this other version of the function should be faster, because 
# it checks if it already has gotten the links of a webpage, and 
# only runs new pages.

# network_from_webpage2 <- function (target, iteration) {
#     start_time <- Sys.time()
# 
#     # Intitialisation :
#     all_el <- list()
#     new_targets <- list()
# 
#     # For the first level of depth :
#     all_el[[1]] <- list()
#     new_targets[[1]] <- list()
# 
#     #Starting from here, the function differs :
#     all_el[[1]] <- unique(edgelist_of(target))
#     new_targets[[1]] <- unlist(unique(all_el[[1]]["TO"], use.names=FALSE))
# 
#     if (iteration > 1) {
#         for (a in (2:iteration)) {
#             all_el[[a]] <- list()
#             new_targets[[a]] <- list()
#             for (i in (1:length(new_targets[[a-1]]))){
#                 all_el[[a]][[i]] <- list()
#                 all_el[[a]][[i]] <- edgelist_of(new_targets[[a-1]][i])
#                 new_targets[[a]] <- c(unlist(new_targets[[a]]),unlist(all_el[[a]][[i]]["TO"],use.names=FALSE))
#             }
#         }
#     }
# }


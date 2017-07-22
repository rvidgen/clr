# # Utility function
#
# topCites <- function(impact_list, n){
#   impact_list$articles_df %>%
#     select(Id, NumberCites) %>%
#     top_n(n = n, wt = NumberCites) %>%
#     arrange(desc(NumberCites))
# }
#
#
# topCites <- function(impact_list, n)
# {
#   UseMethod("topCites",impact_list)
# }
#
# topCites.default <- function(impact_list, n)
# {
#   # print("You screwed up. I do not know how to handle this object.")
#   return(impact_list)
# }
#
#
# topCites.impactCLR <- function(impact_list, n)
# {
#   top_n_cites <- impact_list$articles_df %>%
#     select(Id, NumberCites) %>%
#     top_n(n = n, wt = NumberCites) %>%
#     arrange(desc(NumberCites))
#
#   return(top_n_cites)
# }

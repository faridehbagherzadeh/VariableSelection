#' @title Selecting the appropriate sets of variables in diagnostic/prognostic tests with binary outcome
#' @description variable.selection is the core function of the package which identifies the appropriate sets of predictors of a binary outcome. Based on the dependency of variable selection methods on the classification method, they are categorized into Filter and Wrapper approaches. Based on the product of variable selection, filters are categorized into Ranker and Subset Selector methods. Rankers rank the variables based on their quality while Subset Selectors generate a subset of good quality variables. Wrappers are all Subset Selectors.
#' Based on the methodology of the selected method, you may consider specifying additional parameters. However, all the parameters have default values so that the researchers not interested in or familiar with methodology are able to implement lots of variable selection methods on their own dataset without being involved in the complexity of methods by using a single command (see details).
#'
#' \strong{Ranker methods:}
#' "correlation", "information", "relief", "OneR", "RF"
#'
#' \strong{Subset Selector methods:}
#'  "CFS", "consistency", "wrapper"
#'
#' @export
variable.selection <- function(...) {
    UseMethod("variable.selection")
}


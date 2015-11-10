#' @name variable.selection.default
#' @rdname variable.selection
#' @param input a character vector with the name of input variables to select from.
#' @param target a character string with the name of the outcome that distinguishes nondiseased from diseased individuals. Only applies for the method "variable.selection.default".
#' @param data a data frame containing all needed variables.
#' @param methods
#' "correlation" (Ranker; Based on a correlation measure to compute the relation of each individual variable with the outcome.);
#' "information" (Ranker; Based on a concept called entropy which is a measure  of  uncertainty  or  unpredictability.);
#' "relief" (Ranker; Based on how well the values of each variable distinguish between neighbor subjects; It takes advantage of a nearest-neighbor procedure.);
#' "OneR" (Ranker; "One Rule"; Based on the performance measure of the one-level  decision trees obtained for each individual variable in the data. Ranking of the variables is based on the fact that variables.);
#' "RF" (Ranker; Based on the simple idea that if a variable is not important for prediction of a particular outcome, relocating its values randomly among the instances will not change the performance of the prediction model; the average performance of the random forst trees are used.);
#' "CFS" (Subset Selector; Based on the idea  that a  good  variable subset is one that contains variables uncorrelated with each other while being highly correlated with the outcome.);
#' "consistency" (Subset Selector; Based  on  the  idea  that  a  dataset  containing  only the selected variables must be consistent, i.e. two subjects with the same predictors must belong to the same outcome.);
#' "wrapper" (Subset Selector; Based on the performance measure  of  the diagnostic/prognostic classification) (see details).
#' @param control output of the \code{control.selection} function.
#' @param trace a logical value. If TRUE, information on progress is shown.
#' The default is FALSE.
#' @param ... further arguments.
#' @return Returns an object of class "optimal.cutpoints" with the following components:
#' \itemize{
#' \item{"Selection"}{
#'  a list of items where each item corresponds to a selected varaiable subset, names of this list commonly consist of three componenets seperated by ".". The first component shows the method of variable selection, the second one shows the measure used in the corresponding method and the third one shows the search method.
#'  The firs componenet may be:
#' \strong{"correlation" ,"information" , "relief", "OneR" , "RF" , "CFS", "consistency"} showing the filter method of variable selecction
#' \strong{"logistic"} showing the model used in wrapper variable selection
#'  The second componenet may be:
#'  \strong{"Chi2", "CramerV"} for "orrelation" method
#'  \strong{"IG", "GR", "SU"} for "information" method
#'  \strong{"acc"} for "OneR" method
#'  \strong{"acc", "imp"} for "RF" method
#'  \strong{"IG", "GR", "SU", "Chi2", "CramerV"} for "CFS" method
#'  \strong{"aic"} for  logistic "wrapper" method
#'  The third componenet may be:
#'  \strong{"BR", "FR"} for ranker methods
#'  \strong{"SF", "SB", "BF", "HC"} for subset selector methods
#'  in each of these names there are either "subset" if the method is subset selector or
#'  both "subset" and "weights" if the method is ranker
#' }
#' \item{"frequency"}{a numeric vector containing the number of times each variable was selected by variable selection methods.}
#' \item{"percentage"}{a numeric vector containing the percentage each variable was selected by variable selection methods.}
#' \item{"ranker"}{a character vector containing the names of the implemented Ranker variable selection methods.}
#' \item{"subsetSelector"}{a character vector containing the names of the implemented Subset Selector variable selection methods.}
#' \item{"methods"}{a character vector containing the names of the implemented variable selection methods.}
#' \item{"input"}{a character vector with the name of input variables to select from.}
#' \item{"target"}{target a character string with the name of the outcome that distinguishes nondiseased from diseased individuals.}
#' \item{"call"}{the matched call.}
#' }
#'  @examples
#'  library(VariableSelection)
#'  data(tlgs)
#'
#'  ###########################################
#'  # Variable Selection with the deafault method (all methods with the default parameters):
#'  object <- variable.selection (input=names(tlgs)[-1],
#'  target=names(tlgs)[1], data=tlgs)

#'  print(object)
#'
#' @author Farideh Bagherzadeh-Khiabani.
#' @export
variable.selection.default <-
  function(input, target, data, methods = c("correlation", "information",
    "consistency", "relief", "OneR", "RF", "CFS", "wrapper"),
    control = control.selection(), trace = FALSE,...) {
    if (missing(input) || is.null(input)) {
        stop("'input' argument required.", call. = FALSE)
    }
    if (missing(target) || is.null(target)) {
        stop("'target' argument required.", call. = FALSE)
    }
    if (missing(data) || is.null(data)) {
        stop("'data' argument required.", call. = FALSE)
    }
    if (any(!(methods %in%
              c("correlation", "information", "consistency",
                "relief", "OneR", "RF", "CFS", "wrapper")))) {
        stop("You have entered an invalid method.", call. = FALSE)
    }
    if (!all(c(input, target) %in% names(data))) {
        stop("Not all needed variables are supplied in 'data'.", call. = FALSE)
    }
    if (length(unique(na.omit(data[, target]))) != 2) {
        stop("target variable must be binary", call. = FALSE)
    }
    if (is.logical(trace) == FALSE) {
        stop("'trace' must be a logical-type argument.", call. = FALSE)
    }
    # Missing Data handling
    data <- na.omit(data[, c(input, target)])
    res <- vector("list", 1)
    rNames <- sNames <- vector()
    names(res) <- "Selection"
    res[["Selection"]] <- list()
    ranker <- c("correlation", "information", "OneR", "relief", "RF")
    for (m in methods) {
        object <-
          eval(parse(text = paste("function.", m, sep = "")))(
            input = input, target = target,
            data = data, control = control, trace = trace, ...)
        ifelse(m %in% ranker, rNames <- c(rNames, names(object)),
               sNames <- c(sNames, names(object)))
        res[["Selection"]] <- c(res[["Selection"]], object)
        if (trace) {
          cat("----------------------\n")
          cat("The variables selected by", names(object), "are", paste(object[[names(object)]][["subset"]], collapse=", "))
          cat("\n--------------------\n")
        }
    }
    variables <- sapply(names(res$Selection),
                        function(m) c(res[["Selection"]][[m]][["subset"]]))
    res$frequency <- sort(table(unlist(variables)), decreasing = TRUE)
    res$percentage <- res$frequency / length(names(res[["Selection"]])) * 100
    res$ranker <- rNames
    res$subsetSelector <- sNames
    res$methods <- methods
    res$input <- input
    res$target <- target
    res$call <- match.call()
    class(res) <- "variable.selection"
    res
}

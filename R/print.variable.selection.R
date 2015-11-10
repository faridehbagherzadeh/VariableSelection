#' @name print.variable.selection
#' @title Print method for and object of class variable.selection
#' @description A short summary is printed with: the call to the variable.selection() function;
#' Then On the basis of a variable.selection object, two/three heading are currently available:
#' (1) "The selected variable(s)" shows the selected variables according to each method;
#' (2) "Weights for variables" emereges if there is any ranker methods selected,  showing the weight of each variable in each method.
#' and (3)  "Frequency of Selection" shows the frequency of the selection of each variable was selected across all methods.
#' @rdname print
#' @param x an object of class variable.selection as produced by \code{variable.selection()}.
#' @param ... further arguments passed to method print.default.
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
print.variable.selection <-
  function(x, ...) {
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n",
                           collapse = "\n"), "\n", sep = "")
    cat("\n---------------------")
    cat("\nSelected Variable(s):\n")
    cat("---------------------\n")
    for (m in names(x$Selection)) {
        cat(paste("\n", "Method:", m, "\n"))
        variables <- x[["Selection"]][[m]][["subset"]]
        cat("")
        print(variables)
    }
    if (length(x$ranker) > 0 ) {
      cat("\n----------------------")
      cat("\nWeights for Variables:\n")
      cat("----------------------\n")
      for (m in names(x$Selection)) {
          weights <- x[["Selection"]][[m]][["weights"]]
          if (!is.null(weights)) {
              cat(paste("\n", "Method:", m, "\n"))
              print(weights)
          }
      }
    }
    cat("\n-----------------------")
    cat("\nFrequency of Selection:\n")
    cat("-----------------------\n")
    print(x$percentage)
}

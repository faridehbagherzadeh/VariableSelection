#' @name variable.selection.formula
#' @rdname variable.selection
#' @param
#' formul a formula (method "variable.selection.formula" is called). It must be an object
#' of class "formula". Right side of ~ must contain the name of the variable that
#' distinguishes diseased from non-diseased individuals, and left side of ~ must contain
#' the name of the diagnostic/prognostic test variables.
#' @export
variable.selection.formula <- function(formul, data, methods = c("correlation", "information", "consistency",
    "relief", "OneR", "RF", "CFS", "wrapper"), control = control.selection(), trace = FALSE, ...) {
    if (missing(formul)) {
        stop("'formul' argument required.", call. = FALSE)
    }
    target <- all.vars(formul)[attr(terms(formul), "response")]
    input <- attr(terms(formul), "term.labels")
    if (length(target) != 1 | length(input) < 1) {
        stop("Invalid formula. Please correct", call. = FALSE)
    }
    res <- variable.selection.default(input = input, target = target, data = data, methods = methods,
        control = control, trace = trace, ...)
    res$call <- match.call()
    res
}

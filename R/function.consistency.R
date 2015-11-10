function.consistency <- function(input, target, data,
                                 control = control.selection(), trace = FALSE, ...) {

    consistency.evaluator <- function(subset, target, data, control) {
        res <- list()

        if (!all(is.na(subset))) {
            attr_data <- new_data[, subset, drop = FALSE]
            hashvec <- as.factor(apply(attr_data, 1, digest::digest))
            result <- aggregate(new_data[[target]], list(hash = hashvec),
                                function(classes) {
                return(max(as.vector(table(classes))))
            })
            Liu <- sum(result[[dim(result)[2]]]) / dim(attr_data)[1]
            res$perf <- Liu

        } else res$perf <- -Inf
        res$which.optimal <- which.max

        return(res)
    }

    new_data <- discretize.data(input, target, data, control = control, ...)
    res <- list()
    for (s in control$subset.search) {
        for (i in control$consistency.measure) {
            x <- eval(parse(text = s))(input, target,
                                       data, control = control,
                                       eval.fun = consistency.evaluator, trace = trace)
            res[["consistency"]][[i]][[s]] <- list(subset = x$subset)
        }
    }
    return(unlist(unlist(res, recursive = FALSE), recursive = FALSE))
}

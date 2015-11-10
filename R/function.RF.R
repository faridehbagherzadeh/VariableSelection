function.RF <- function(input, target, data, control = control.selection(), trace = FALSE, ...) {
    new_data <- discretize.data(input, target, data, control = control, ...)

    no_na <- rep(TRUE, dim(new_data)[1])
    for (i in 1:dim(new_data)[2]) {
        no_na <- no_na & complete.cases(new_data[, i])
    }
    new_data <- new_data[no_na, , drop = FALSE]
    res <- list()
    forest <- randomForest::randomForest(new_data[input], new_data[, target], ntree = control$RF.ntree,
        keep.forest = FALSE, importance = TRUE)
    for (s in control$ranker.search) {
        for (i in control$RF.measure) {
            type <- if (i == "acc") {
                1
            } else if (i == "imp")
                2
            weights <- as.vector(randomForest::importance(forest, type = type))
            names(weights) <- input
            weights <- sort(weights, decreasing = TRUE)
            x <- ranker.search(weights, target, data, control = within(control, ranker.search <- s), trace = trace)
            res[["RF"]][[i]][[s]] <- list(weights = weights, subset = x$subset)
        }
    }
    return(unlist(unlist(res, recursive = FALSE), recursive = FALSE))
}

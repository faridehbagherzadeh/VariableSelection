function.OneR <- function(input, target, data,
                          control = control.selection(), trace = FALSE, ...) {
    new_data <- discretize.data(input, target, data, control = control, ...)
    res <- list()
    OneRres <- data.frame(sapply(input, OneR.evaluator, c = target, data = new_data), row.names="acc")
    names(OneRres) <- setdiff(names(new_data), target)
    for (s in control$ranker.search) {
        for (j in control$OneR.measure) {
            weights <- unlist(OneRres[j, ])
            weights <- sort(weights, decreasing = TRUE)
            x <- ranker.search(weights, target, data,
                               control = within(control, ranker.search <- s), trace = trace)
            res[["OneR"]][[j]][[s]] <-
              list(weights = weights, subset = x$subset)
        }
    }
    return(unlist(unlist(res, recursive = FALSE), recursive = FALSE))
}

OneR.evaluator <- function(v, c, data, control = control.selection()) {
    vec <- factor(data[, v])
    accs <- sapply(levels(vec), function(val) {
        return(max(table(data[data[, v] == val, c])))
    })
    acc <- sum(accs) / nrow(data)
    imp <- entropy::entropy(table(data[, c(v, c)], useNA = "always"))
    # return(list(acc = acc, imp = imp))
    return(list(acc = acc))
}

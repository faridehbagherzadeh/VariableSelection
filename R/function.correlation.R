function.correlation <- function(input, target, data,
                                 control = control.selection(), trace = FALSE, ...) {
    new_data <- discretize.data(input, target, data, control = control, ...)
    res <- list()
    cor <- sapply(input, correlation.evaluator, v2 = target, data = new_data,
                  control = control)
    for (s in control$ranker.search) {
        for (i in control$correlation.measure) {
            weights <- unlist(cor[i, ])
            weights <- sort(weights, decreasing = TRUE)
            x <- ranker.search(weights, target, data,
                               control = within(control, ranker.search <- s), trace = trace)
            res[["correlation"]][[i]][[s]] <- list(weights = weights, subset = x$subset)
        }
    }
    return(unlist(unlist(res, recursive = FALSE), recursive = FALSE))
}

correlation.evaluator <- function(v1, v2, data, control = control.selection()) {
    cont <- table(data[, v1], data[, v2])
    row_sums <- apply(cont, 1, sum)
    col_sums <- apply(cont, 2, sum)
    all_sum <- sum(col_sums)
    expected_matrix <- t(as.matrix(col_sums) %*% t(as.matrix(row_sums))) /
      all_sum
    Chi2 <- sum((cont - expected_matrix) ^ 2 / expected_matrix)
    CramerV <- ifelse(Chi2 == 0 || length(col_sums) < 2 ||
                        length(row_sums) < 2, 0, sqrt(Chi2 / (all_sum *
        min(length(col_sums) - 1, length(row_sums) - 1))))
    return(list(Chi2 = Chi2, CramerV = CramerV))
}

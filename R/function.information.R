function.information <- function(input, target, data,
                                 control = control.selection(), trace = FALSE, ...) {
    new_data <- discretize.data(input, target, data, control = control, ...)
    res <- list()
    info <- sapply(input, information.evaluator, c = target, data = new_data,
                   control = control)
    for (s in control$ranker.search) {
        for (j in control$information.measure) {
            weights <- unlist(info[j, ])
            weights <- sort(weights, decreasing = TRUE)
            x <- ranker.search(weights, target, data,
                               control = within(control, ranker.search <- s), trace = trace)
            res[["information"]][[j]][[s]] <-
              list(weights = weights, subset = x$subset)
        }
    }
    return(unlist(unlist(res, recursive = FALSE), recursive = FALSE))
}

information.evaluator <- function(v, c, data, control = control.selection()) {
    V.ent <- entropy::entropy(table(data[, v]), useNA = "always")
    C.ent <- entropy::entropy(table(data[, c]), useNA = "always")
    joint.ent <- entropy::entropy(table(data[, c(v, c)], useNA = "always"))
    IG <- V.ent + C.ent - joint.ent
    GR <- IG / V.ent
    GR[IG == 0] <- 0
    SU <- 2 * IG / (V.ent + C.ent)
    SU[IG == 0] <- 0
    return(list(IG = IG, GR = GR, SU = SU))
}

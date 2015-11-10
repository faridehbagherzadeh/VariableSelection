function.CFS <- function(input, target, data, control = control.selection(), trace = FALSE, ...) {
    ff_cor_mat <- lower.tri(matrix(, length(input), length(input)))
    rownames(ff_cor_mat) <- colnames(ff_cor_mat) <- input
    fc_cor_mat <- matrix(rep(NA, length(input)), nrow = length(input), ncol = 1,
                         dimnames = list(input, target))
    new_data <- discretize.data(input, target, data, control = control, ...)
    matList <- list()
    for (i in control$CFS.measure) {
        for (v in input) {
            if (i %in% c("IG", "GR", "SU")) {
                evaluator <- information.evaluator
            } else if (i %in% c("Chi2", "CramerV")) {
                evaluator <- correlation.evaluator
            }
            fc_cor_mat[v, target] <-
              evaluator(v, target,
                        data = new_data, control =
                          control.selection(information.measure = i))[[i]]
            for (w in input) {
                ff_cor_mat[v, w] <-
                  ifelse(
                    ff_cor_mat[v, w],
                    evaluator(v, w, data = new_data,
                              control =
                                control.selection(information.measure = i))[[i]]
                    , NA)
            }
        }
        matList[[i]] <- list(fc_cor_mat = fc_cor_mat, ff_cor_mat = ff_cor_mat)
    }

    cfs.evaluator <- function(subset, target, data, control) {
        res <- list()

        if (!all(is.na(subset))) {
            k <- length(subset)
            fc_cor <- sum(fc_cor_mat[subset, target], na.rm = TRUE) /
              length(subset)
            ff_cor <- sum(ff_cor_mat[subset, subset],
                          na.rm = TRUE) / (length(subset) * length(subset) -
                length(subset))
            res$perf <- ifelse(k == 1, fc_cor, k * fc_cor / sqrt(k + k * (k - 1) *
                                                                 ff_cor))

        } else res$perf <- -Inf
        res$which.optimal <- which.max

        return(res)
    }

    res <- list()
    for (s in control$subset.search) {
        for (j in control$CFS.measure) {
            fc_cor_mat <- matList[[j]][["fc_cor_mat"]]
            ff_cor_mat <- matList[[j]][["ff_cor_mat"]]
            x <- eval(parse(text = s))(input, target, data,
                                       control = control,
                                       eval.fun = cfs.evaluator, trace = trace)
            res[["CFS"]][[j]][[s]] <- list(subset = x$subset)
        }
    }
    return(unlist(unlist(res, recursive = FALSE), recursive = FALSE))
}


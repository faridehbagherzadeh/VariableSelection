discretize.data <- function(input, target, data, control, ...) {
    temp <- data[, sapply(data, is.numeric), drop = FALSE]
    if (control$disc.method %in% c("mdlp", "chi2", "chiM", "disc.Topdown",
                                   "extendChi2", "modChi2")) {
        temp[] <- eval(
          parse(text = paste(
            "discretization::",
            control$disc.method, sep = "")))(data.frame(temp,
            data[target]))$Disc.data[-(ncol(temp)+1)]
    }
    if (control$disc.method == "EB") {
        if (control$bins < 1)
            stop("Number of bins too small")

        temp[] <- lapply(temp, cut, breaks = control$bins)
    }
    if (control$disc.method == "EF") {
        if (control$bins < 1)
            stop("Number of bins too small")

        temp[] <- lapply(temp, function(x) {
            id <- round(c(1, (1:(control$bins - 1)) *
                            (nrow(data) / control$bins), nrow(data)))
            breaks <- sort(x)[id]
            if (sum(duplicated(breaks)) > 0)
                stop("n is too large.")
            return(cut(x, breaks, include.lowest = TRUE))
        })
    }
    data[, colnames(temp)] <- lapply(temp, as.factor)
    return(data[, c(input, target)])
}

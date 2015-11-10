ranker.search <- function(weights, target, data, control, trace=FALSE) {
    if (length(weights) == 0)
        return(character(0))
    evaluation.criteria <- numeric()
    models <- list()
    sorted_names <- names(weights)[order(weights, decreasing = TRUE)]
    if (control$ranker.search == "BR") {
        for (i in 1:length(weights)) {
            subset <- sorted_names[1:i]
            model <- eval(
              parse(
                text = paste("model.",
                             control$model, sep = "")))(subset, target, data)
            evaluation.criteria[i] <-
              eval(parse(text =
                           paste("model$", control$model.evaluation, sep = "")))
            models[[i]] <- model
            if (trace) {
              cat("-----","Step ", i)
              cat("\nSubset:\n",paste(subset[i], collapse=", "))
              cat("\n\nPerformance:\n", evaluation.criteria[i], "\n\n")
            }
        }
        if (control$model.evaluation == "aic")
            k <- which(evaluation.criteria == min(evaluation.criteria))
        res <- list(k = k, subset = sorted_names[1:k],
                    optimal.criterion = evaluation.criteria[k],
                    optimal.model = models[[k]],
                    evaluation.criteria = evaluation.criteria,
                    models = models)
    }

    if (control$ranker.search == "FR") {
        if (control$ranker.k < 1)
            stop("k too small")
        if (control$ranker.k > length(weights))
            control$ranker.k <- length(weights)
        subset <- sorted_names[1:control$ranker.k]
        model <-
          eval(parse(
            text = paste(
              "model.", control$model, sep = "")))(subset, target, data)
        optimal.criterion <-
          eval(
            parse(text = paste("model$", control$model.evaluation, sep = "")))
        k <- control$ranker.k
        res <-
          list(k = k, subset = sorted_names[1:k],
               optimal.criterion = optimal.criterion, optimal.model = model)
    }
    return(res)
}

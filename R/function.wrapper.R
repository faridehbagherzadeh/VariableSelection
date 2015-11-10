function.wrapper <-
  function(input, target, data, control = control.selection(), trace = FALSE, ...) {
    wrapper.evaluator <- function(subset, target, data, control) {
        res <- list()
        if (!all(is.na(subset))) {
            model <- eval(parse(text = paste(
              "model.", control$model, sep = "")))(input = subset,
                target = target, data = data)
            res$perf <- eval(parse(text = paste(
              "model$", control$model.evaluation, sep = "")))
        } else res$perf <- Inf
        if (control$model.evaluation == "aic") {
            res$which.optimal <- which.min
        }

        return(res)
    }

    res <- list()
    for (s in control$subset.search) {
        x <- eval(parse(text = s))(
          input, target, data, control = control, eval.fun = wrapper.evaluator, trace = trace)
        res[[control$model]][[control$model.evaluation]][[s]] <- list(subset = x$subset)
    }
    return(unlist(unlist(res, recursive = FALSE), recursive = FALSE))
}

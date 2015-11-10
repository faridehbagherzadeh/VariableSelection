model.logistic <- function(input, target, data, ...) {
    model <- glm(as.formula(paste(target, "~.")), data[, c(input, target)],
                 family = binomial(link = "logit"), na.action = na.exclude, ...)
    return(model = model)
}

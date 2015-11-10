
#' @name plot.variable.selection
#' @title Plot method for and object of class variable.selection
#' @description On the basis of a variable.selection object, two/three plots are currently available:
#' (1) a plot showinh the variables selected in each method ;
#' (2) a plot showing the  frequency of the selection of each variable  across all methods;
#' and, if there is any ranker methods selected, (3) a plot showing the weight of each variable in each method. The square related to each variable-method will be darker if the  variable has a high weight in this method and it will be white if it has a low weight. The intensity of color shows the weight of the variable.
#' @rdname plot
#' @param x an object of class variable.selection as produced by \code{variable.selection()}.
#' @param which a numeric vector with the required plots. By default, all the possible options are plotted.
#' @param col color
#' @param ... further arguments passed to method plot.default.
#'  @examples
#'  library(VariableSelection)
#'  data(tlgs)
#'
#'  ###########################################
#'  # Variable Selection with the deafault method (all methods with the default parameters):
#'  object <- variable.selection (input=names(tlgs)[-1],
#'  target=names(tlgs)[1], data=tlgs)

#'  plot(object)
#'
#' @author Farideh Bagherzadeh-Khiabani.
#' @export
plot.variable.selection <- function(x, which = c(1L, 2L, 3L), col="black", ...) {

      if (!is.numeric(which) || any(which < 1) || any(which > 3))
        stop("'which' must be in 1:3")
    show <- rep(FALSE, 3)
    show[which] <- TRUE
    n.plots <- 0
    SVList <- lapply(names(x$Selection),
                     function(m) c(x[["Selection"]][[m]][["subset"]]))
    names(SVList) <- names(x$Selection)
    frequency <- x[["frequency"]]
    if (show[1L]) {
        if (n.plots > 0) {
            readline("Press return for next page....")
        }
        n <- length(x$input)
        p <- length(SVList)
        # define rectangles
        xl <- (1:n) - 0.5
        xr <- (1:n) + 0.5
        yb <- (1:p) - 0.5
        yt <- (1:p) + 0.5
        rects <- merge(data.frame(xl, xr), data.frame(yb, yt))
        cols <- matrix(nrow = n, ncol = p)
        dimnames(cols) <- list(x$input, names(x[["Selection"]]))
        for (m in names(x[["Selection"]])) for (v in x$input)
          cols[v, m] <- ifelse(v %in% SVList[[m]], col, "white")
        cols <- as.vector(cols)

        xlim <- c(0.5, n + 0.5)
        ylim <- c(0.5, p + 0.5)
        plot.new()
        plot.window(xlim = xlim, ylim = ylim, ...)
        rect(rects$xl, rects$yb, rects$xr, rects$yt, col = cols)
        axis(1, at = 1:n, labels = x$input, las = 3, cex.axis = 0.7, ...)
        axis(2, at = 1:p, labels = names(x[["Selection"]]),
             las = 1, cex.axis = 0.7, ...)
        n.plots <- n.plots + 1
    }
    if (show[2L]) {
        if (n.plots > 0) {
            readline("Press return for next page....")
        }
        barplot(frequency, las = 2, col=col, ...)
        n.plots <- n.plots + 1
    }
    if (show[3L]) {
      if (length(x$ranker) > 0 ) {
          if (n.plots > 0) {
              readline("Press return for next page....")
          }
            n <- length(x$input)
          p <- length(x$ranker)
          # define rectangles
          xl <- (1:n) - 0.5
          xr <- (1:n) + 0.5
          yb <- (1:p) - 0.5
          yt <- (1:p) + 0.5
          rects <- merge(data.frame(xl, xr), data.frame(yb, yt))
          cols <- matrix(nrow = n, ncol = p)
          dimnames(cols) <- list(x$input, x$ranker)
          for (m in x$ranker) {
              range <- x[["Selection"]][[m]][["weights"]]
              range <- unlist(range)
              range <- (range - min(range)) / (max(range) - min(range))
              cols[names(range), m] <- rgb(colorRamp(c("white", col=col))(range)/255) # gray(1 - range)
          }
          xlim <- c(0.5, n + 0.5)
          ylim <- c(0.5, p + 0.5)
          plot.new()
          plot.window(xlim = xlim, ylim = ylim, ...)
          rect(rects$xl, rects$yb, rects$xr, rects$yt, col = cols)
          axis(1, at = 1:n, labels = x$input, las = 3, cex.axis = 0.7, ...)
          axis(2, at = 1:p, labels = x$ranker, las = 1, cex.axis = 0.7, ...)
          n.plots <- n.plots + 1
      }
    }
}

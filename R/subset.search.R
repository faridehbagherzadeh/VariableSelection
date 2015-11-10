SF <- function(input, target, data, control= control.selection(), trace=FALSE, eval.fun) {
  return(subset.search(input, target, data, control, trace, eval.fun, initial.subset="empty", direction="forward", backtracks=0))
}

SB <- function(input, target, data, control= control.selection(), trace=FALSE, eval.fun) {
  return(subset.search(input, target, data, control, trace, eval.fun, initial.subset="full", direction="backward", backtracks=0))
}

# BF <- function(input, target, data, control= control.selection(), eval.fun) {
#   return(subset.search(input, target, data, control, eval.fun, initial.subset="full", direction="backward", backtracks=control$search.backtracks))
# }

BF <- function(input, target, data, control= control.selection(), trace=FALSE, eval.fun) {
  return(subset.search(input, target, data, control, trace, eval.fun, initial.subset="empty", direction="forward", backtracks=control$search.backtracks))
}

HC <- function(input, target, data, control= control.selection(), trace=FALSE, eval.fun) {
  return(subset.search(input, target, data, control, trace, eval.fun, initial.subset="random", direction="both", backtracks=0))
}

print.step <- function(step) {
  cat("\nSubset:\n",paste(step$subset, collapse=", "))
  cat("\n\nPerformance:\n", step$eval, "\n\n")
}


subset.search <- function(input, target, data, control=control.selection(), trace=FALSE, eval.fun, initial.subset, direction, backtracks) {

  if(length(input) == 0)
    stop("input not specified")

  eval.fun = match.fun(eval.fun)
  res <- list()

  candidates <- best <- local.best <- initial <- list()
  stepInd  <- 0
  backtrackInd  <- 0
  match.arg(initial.subset, c("empty", "full", "random"))
  initial$subset <-
    if (initial.subset == "empty") NA else #
      if (initial.subset == "full")  input else
        if (initial.subset == "random")  input[which(sample(c(TRUE, FALSE), length(input), replace = TRUE))] # init.idx <- rep(0, length(attributes)); while (sum(init.idx)==0) init.idx <- sample(c(0,1), length(attributes), replace = TRUE)
  # if (initial.subset %in% input) initial$subset

  temp <- eval.fun(subset=initial$subset, target=target, data=data, control=control)
  which.optimal <- temp$which.optimal
  initial$eval <- temp$perf

  evaluated <- backtrackList <- bestList <- best <- local.best <- initial

  if (trace) {
    if (!all(is.na(initial$subset))) {
      cat("-----","Step ", stepInd)
      print.step(initial)
    }
  }

  generate.candidates <- function(input, subset, direction) {
    match.arg(direction, c("forward", "backward", "both"))
    res1 <- res2 <- NULL
    if (direction == "forward" || direction == "both")
      if (length(input)>length(subset)) res1 <- lapply(setdiff(input, subset), function(a) c(subset, a)[!is.na(c(subset, a))]) else res1 <- list()
    if (direction == "backward" || direction == "both")
      if (length(subset)>1) res2 <- lapply(1:length(subset), function(i) subset[-i]) else res2 <- list()
    return(union(res1, res2))
  }

  backtracking <- FALSE
  repeat {
    if (FALSE) {
      print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      cat("stepInd", stepInd, "\n")
      cat("backtrackInd", backtrackInd, "\n")
      print("====================")
      print("local.best")
      print(local.best$subset)
      print("====================")
      print("best")
      print(best$subset)
      print("====================")
      print("bestList")
      print(bestList$subset)
      print("====================")
      print("backtrackList")
      print(backtrackList$subset)
    }
    stepInd <- stepInd + 1
    if (!backtracking) {
      candidates$subset <- generate.candidates(input=input, subset=best$subset, direction=direction)
    } else {
      candidates$subset <- setdiff(generate.candidates(input=input, subset=best$subset, direction=direction),backtrackList$subset)


      backtracking <- FALSE
    }
    if(is.null(candidates$subset))
      break()
    # lapply(candidates$subset,setequal,best$subset)
    candidates$eval <- rep(list(NULL), length(candidates$subset))

    if (direction == "both" | backtracks > 0) {
      for (i in seq_along(candidates$subset))
        for (j in seq_along(evaluated$subset))
          if (setequal( candidates$subset[[i]],evaluated$subset[[j]])) candidates$eval[[i]] <- evaluated$eval[[j]]
    }

    ind <- sapply(candidates$eval, is.null)
    l <- lapply(candidates$subset[ind], eval.fun, target=target, data=data, control=control)
    candidates$eval[ind] <-  lapply(l,"[[", "perf")
    new <- lapply(candidates, function(x) x[ind])
    evaluated <- mapply(c, evaluated, new, SIMPLIFY=FALSE)

    bestIdx <- which.optimal(unlist(candidates$eval))

    local.best$subset <- candidates$subset[[bestIdx]]
    local.best$eval <- candidates$eval[[bestIdx]]

    star <- rep("", length(candidates$eval))
    star[bestIdx] <- "*"

    if (trace) {
      cat("-----","Step ", stepInd, "\n")
      print(format(data.frame(Candidates=sapply(candidates[[1]],  paste, collapse=", "), Performance=unlist(candidates[[2]]), star=star), justify= "left"))
      # cat ("Press [enter] to continue")
      # line <- readline()
      # print(format(data.frame(Candidates=sapply(bestList[[1]],  paste, collapse=", "), Performance=unlist(bestList[[2]])), justify= "left"))
    }

    if(best$eval != local.best$eval & (which.optimal(c(local.best$eval, best$eval)) == 1)) {
      if (trace) {
        cat("\nBest at this step:")
        print.step(local.best)
      }
      best <- local.best
      bestList$subset[length(bestList$subset) + 1] <- list(best$subset)
      bestList$eval[length(bestList$eval) + 1] <- list(best$eval)
      # cat(length(bestList$subset), "\n") ##
    } else if (backtracks > 0) {

      backtracking <- TRUE
      backtracks <- backtracks - 1
      backtrackInd <- backtrackInd + 1
      backtrackList$subset[backtrackInd] <- list(best$subset)
      backtrackList$eval[backtrackInd] <- list(best$eval)
      bestList <- lapply(bestList, function(x) x[-length(x)])
      # cat(length(bestList$subset), "\n")
      best <- lapply(bestList, "[[", length(bestList$subset))
      if (trace) {
        cat("\nThere was no improvement at this step. Thus the process backtracks and undo the last best selected subset. Thus, It continues from the following subset and select the next best subset instead:")
        print.step(best) #
        cat("\n-----", "Backtrack ", backtrackInd)
        # cat ("Press [enter] to continue")
        # line <- readline()
      }
    } else {
      if (trace) {
        cat("\nThere was no improvement at this step. Thus the process halts.")
      }
      break()
    }
  }

  if (backtrackInd > 0) {
    best <- lapply(backtrackList, "[[", which.optimal(unlist(backtrackList$eval)))
  }

  if (trace) {
    cat("\n*************************\n")
    cat("\nThe Best Selected Subset:\n")
    print.step(best)
  }

  res$evaluated <- evaluated
  res$subset <- best$subset
  return(res)
}

# object <- subset.search(input = input, target = target, data = dataset[train, ], initial.subset="empty", direction="forward", eval.fun=wrapper.evaluator, backtracks = 100)
# object <- SF(input = input, target = target, data=dataset[train, ], control= control.selection(), eval.fun=wrapper.evaluator)
# object <- SB(input = input, target = target, data=dataset[train, ], control= control.selection(), eval.fun=wrapper.evaluator)
# BF(input = input, target = target, data=dataset[train, ], control= control.selection(), eval.fun=wrapper.evaluator)
# object <- HC(input = input, target = target, data=dataset[train, ], control= control.selection(), eval.fun=wrapper.evaluator)



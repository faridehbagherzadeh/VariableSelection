function.relief <- function(input, target, data, control = control.selection(), trace = FALSE, ...){
  field_distance <- function(input_idx, instance1, instance2) {
        value1 <- instance1[1, input_idx]
        value2 <- instance2[1, input_idx]
        if (is.factor(value1) && is.factor(value2)) {
            if (is.na(value1) && is.na(value2)) {
                return(1 - sum(p_val_in_class[[input_idx]]
                               [, instance1[1, target]] *
                                 p_val_in_class[[
                                   input_idx]][, instance2[1, target]]))
            } else if (is.na(value1) || is.na(value2)) {
                if (is.na(value1)) {
                  known_value <- value2
                  unknown_class <- instance1[1, target]
                } else {
                  known_value <- value1
                  unknown_class <- instance2[1, target]
                }
                return(1 - p_val_in_class[[input_idx]][
                  known_value, unknown_class])
            } else if (value1 == value2) {
                return(0)
            } else {
                # if(value1 != value2)
                return(1)
            }
        } else if (is.numeric(value1) && is.numeric(value2)) {
            if (is.na(value1) && is.na(value2)) {
                return(1)
            } else if (is.na(value1)) {
                return(max(value2, 1 - value2))
            } else if (is.na(value2)) {
                return(max(value1, 1 - value1))
            } else {
                return(abs(value1 - value2))
            }
        } else {
            stop("Unsupported value type")
        }
    }

    instance_distance <- function(instance1, instance2) {

        result <- sapply(input, function(i) {
            return(field_distance(i, instance1, instance2))
        })
        res <- sum(result ^ 2)
        if (is.na(res)) {
            stop("Internal error. Distance NA.")
        }
        return(res)
    }
    # uses parent.env
    find_neighbours <- function(instance_idx) {
        instance <- new_data[instance_idx, , drop = FALSE]
        # for every other instance
        for (current_idx in 1:instances_count) {
            if (instance_idx == current_idx)
                (next)()
            current_instance <- new_data[current_idx, , drop = FALSE]
            if (is.na(current_instance[, target]))
                (next)()
            dist <- instance_distance(instance, current_instance)
            class_no <- 1
            if (nn_stored_count[class_no] < control$Relief.nneighbour) {
                nn_stored_count[class_no] <<- nn_stored_count[class_no] + 1
                n_array[class_no, nn_stored_count[class_no], ] <<-
                  c(dist, current_idx)
            } else {
                max_idx <- which.max(n_array[class_no, , 1])
                max_value <- n_array[class_no, max_idx, 1]
                if (dist < max_value) {
                  n_array[class_no, max_idx, ] <<- c(dist, current_idx)
                }
            }
        }
    }
    # uses parent.env
    update_weights <- function(instance_idx) {
        instance <- new_data[instance_idx, , drop = FALSE]
        instance_class <- instance[1, target]
        instance_class_no <- which(classes == instance_class)
        # for each attribute
        for (input_idx in 1:attributes_count) {
            # nearest hits
            hits_sum <- 0
            if (nn_stored_count[instance_class_no] > 0) {
                hits_sum <- sum(sapply(1:nn_stored_count[
                  instance_class_no], function(n_idx) {
                  n_instance_idx <- n_array[instance_class_no, n_idx, 2]
                  n_instance <- new_data[n_instance_idx, , drop = FALSE]
                  return(field_distance(input_idx, instance, n_instance))
                }))
                hits_sum <- hits_sum / nn_stored_count[instance_class_no]
            }
            # nearest misses
            misses_sum <- 0
            if (class_count > 1) {
                misses_sum <- sum(sapply((1:class_count)[-instance_class_no], function(class_no) {
                  class_misses_sum <- 0
                  if (nn_stored_count[class_no] > 0) {
                    class_misses_sum <- sum(sapply(1:nn_stored_count[class_no], function(n_idx) {
                      n_instance_idx <- n_array[class_no, n_idx, 2]
                      n_instance <- new_data[, input][n_instance_idx, , drop = FALSE]
                      return(field_distance(input_idx, instance, n_instance))
                    }))
                    class_misses_sum <-
                      class_misses_sum *
                      class_prob[class_no] / nn_stored_count[class_no]
                  }
                  return(class_misses_sum)
                }))
                misses_sum <- misses_sum/(1 - class_prob[instance_class_no])
            }
            weights[input_idx] <<- weights[input_idx] - hits_sum + misses_sum
        }
    }
    # uses parent.env
    new_data <- data[, c(input, target)]
    new_data <- normalize.min.max(new_data)
    class_count <- NULL
    class_prob <- NULL
    classes <- NULL
    p_val_in_class <- NULL
    weights <- NULL
    n_array <- NULL
    nn_stored_count <- NULL
    sample_instances_idx <- NULL
    instances_count <- dim(new_data)[1]
    attributes_count <- length(input)
    if (control$Relief.nneighbour < 1) {
        control$Relief.nneighbour <- 1
        warning(paste("Assumed: control$Relief.nneighbour = ",
                      control$Relief.nneighbour))
    }
    if (control$Relief.nsample < 1) {
        control$Relief.nsample <- 1
        warning(paste("Assumed: control$Relief.nsample = ",
                      control$Relief.nsample))
        sample_instances_idx <- sample(1:instances_count, 1)
    } else if (control$Relief.nsample > instances_count) {
        warning(paste("Assumed: control$Relief.nsample = ",
                      control$Relief.nsample))
        control$Relief.nsample <- instances_count
        sample_instances_idx <- 1:instances_count
    } else {
        sample_instances_idx <- sort(sample(
          1:instances_count, control$Relief.nsample, replace = TRUE))
    }
    tab <- table(new_data[[target]])
    class_prob <- tab / sum(tab)
    classes <- names(class_prob)
    class_count <- length(classes)
    p_val_in_class <- lapply(new_data[input], function(vec) {
        tab <- table(vec, new_data[[target]])
        return(apply(tab, 2, function(x) {
            s <- sum(x)
            if (s == 0) return(x) else return(x / s)
        }))
    })
    n_array <- array(0, c(class_count, control$Relief.nneighbour, 2))
    nn_stored_count <- array(0, class_count)
    weights <- rep(0, attributes_count)
    sapply(sample_instances_idx, function(current_instance_idx) {
        current_instance <- new_data[current_instance_idx, , drop = FALSE]
        if (is.na(current_instance[[target]]))
            return(NULL)
        nn_stored_count[] <<- 0
        n_array[] <<- Inf
        find_neighbours(current_instance_idx)
        update_weights(current_instance_idx)
    })
    res <- list()
    weights <- weights / control$Relief.nsample
    names(weights) <- input
    weights <- sort(weights, decreasing = TRUE)

    for (s in control$ranker.search) {
        x <- ranker.search(weights, target, data,
                           control = within(control, ranker.search <- s), trace = trace)
        res[["relief"]][[s]] <- list(weights = weights, subset = x$subset)
    }

    return(unlist(res, recursive = FALSE))
}

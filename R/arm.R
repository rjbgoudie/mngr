#' Build factorial experiment
#' @param ... passed to expand.grid
#' @export
arms_factorial <- function(...){
  arms_list <- list(...)
  assign("arms_list", arms_list, envir = task_env)
}

arms_all <- function(task, include_shared = TRUE){
  task_obj <- task_get(task, exists = TRUE)

  arms_list <- task_env$arms_list
  share <- task_obj$getShared()
  which_arms_shared <- names(arms_list) %in% share
  arms_unshared <- do.call("expand.grid", arms_list[!which_arms_shared])
  arms_unshared <- lapply(seq_len(nrow(arms_unshared)), function(i){
    as.list(arms_unshared[i,, drop = FALSE])
  })
  if (include_shared){
    if (sum(which_arms_shared) > 0){
      arms_shared <- arms_list[which_arms_shared]
      values <- lapply(seq_along(arms_unshared), function(x) arms_shared)
      arms_unshared <- mapply(append, arms_unshared, values, SIMPLIFY = FALSE)
    }
    arms_unshared
  } else {
    arms_unshared
  }
}

#' Unique name for arm
arm_name <- function(){
  arms <- arms_all(.task, include_shared = TRUE)

  arm <- arms[[.arm]]
  o <- order(names(arm))
  arm <- arm[o]
  arm_values <- sapply(arm, paste, collapse = "-")
  arm_names <- names(arm)
  paste(arm_names, arm_values, sep = "--",  collapse = "__")
}

arm_name_shared <- function(){
  arms <- arms_all(.task, include_shared = TRUE)

  arm <- arms[[.arm]]
  o <- order(names(arm))
  arm <- arm[o]
  grid <- expand.grid(arm)
  apply(grid, 1, function(arm){
    arm_values <- sapply(arm, paste, collapse = ",")
    arm_names <- names(arm)
    paste(arm_names, arm_values, sep = "--",  collapse = "__")
  })
}

#' @export
`%share%` <- function(taskname, share){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$add_shared(share)
  invisible(taskname)
}

#' @export
read_rds_shared <- function(...){
  all_arms <- arm_name_shared()
  all_paths <- sapply(all_arms, function(x){
    rds_file(..., arm = x)
  })
  lapply(all_paths, readRDS)
}

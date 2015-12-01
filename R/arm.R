#' Build factorial experiment
#' @param ... passed to expand.grid
#' @export
arms_factorial <- function(...){
  arms_list <- list(...)
  assign("arms_list", arms_list, envir = task_env)
}

arms_all <- function(task, include_shared = TRUE){
  id <- task_find_id(task, exists = TRUE)
  task_obj <- task_env$tasklist[[id]]

  arms_list <- task_env$arms_list
  share <- task_obj$getShared()
  which_arms_shared <- names(arms_list) %in% share
  arms_unshared <- do.call("expand.grid", arms_list[!which_arms_shared])
  arms_unshared <- apply(arms_unshared, 1, as.list)
  if (include_shared){
    arms_shared <- arms_list[which_arms_shared]
    values <- lapply(seq_along(arms_unshared), function(x) arms_shared)
    arms_unshared <- mapply(append, arms_unshared, values, SIMPLIFY = FALSE)
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
  arm_values <- sapply(arm, paste, collapse = ",")
  arm_names <- names(arm)
  paste(.arm,
        paste(arm_names, arm_values, sep = ":",  collapse = ";"),
        sep = "-")
}

#' @export
`%share%` <- function(taskname, share){
  stopifnot(inherits(taskname, "character"))
  task_id <- task_find_id(taskname, exists = TRUE)
  task_env$tasklist[[task_id]]$add_shared(share)
  invisible(taskname)
}

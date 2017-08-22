#' Build factorial experiment
#' @param ... passed to expand.grid
#' @export
arms_factorial <- function(...){
  arms_list <- list(...)
  assign("arms_list", arms_list, envir = task_env)
}

arms_all <- function(task, expand_split = TRUE){
  task_obj <- task_get(task, exists = TRUE)
  task_obj$arms(expand_split = expand_split)
}

#' Unique name for arm
arm_name <- function(expand_split = TRUE){
  arms <- arms_all(.task, expand_split = expand_split)

  arm <- arms[[.arm]]
  o <- order(names(arm))
  arm <- arm[o]
  arm_values <- sapply(arm, paste, collapse = "-")
  arm_names <- names(arm)
  paste(arm_names, arm_values, sep = "--",  collapse = "__")
}

arm_name_merge <- function(){
  arms <- arms_all(.task)

  arm <- arms[[.arm]]
  o <- order(names(arm))
  arm <- arm[o]
  grid <- expand.grid(sapply(arm, as.character))
  apply(grid, 1, function(arm){
    arm_values <- sapply(arm, paste, collapse = ",")
    arm_names <- names(arm)
    paste(arm_names, arm_values, sep = "--",  collapse = "__")
  })
}

#' @export
`%merge%` <- function(taskname, merge){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$add_merge(merge)
  invisible(taskname)
}

#' @export
`%split%` <- function(taskname, split){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$add_split(split)
  invisible(taskname)
}

#' @export
read_rds_merge <- function(...){
  all_arms <- arm_name_merge()
  all_paths <- sapply(all_arms, function(x){
    rds_file(..., arm = x)
  })
  lapply(all_paths, readRDS)
}

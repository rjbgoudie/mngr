#' Build factorial arms
#'
#' Adds arms of ALL combinations of the supplied variables as arms
#'
#' @param ... A number of parameters
#' @examples
#' # will run 4 arms, corresponding to each combination
#' arms_factorial(seed = c(21, 42), type = c("fast", "slow"))
#' @export
arms_factorial <- function(...){
  arms_list <- list(...)
  assign("arms_list", arms_list, envir = task_env)
}

#' Unique name for arm
#'
#' Create a character name for a particular arm
#'
#' NOTE THIS SEEMS TO DUPLICATE taskarm_name??
#'
#' @return A character vector of length 1, the arm_name
arm_id <- function(task, splitting = FALSE){
  task_obj <- task_get(task, exists = TRUE)
  task_obj$arm_ids(splitting = splitting)[.arm]
}

#' Arm IDs for the prerequisite tasks
#'
#' @param task A task name
#' @return
#' A list of arm ids of the current arm of the supplied task
prereq_arm_ids <- function(task){
  task_obj <- task_get(task, exists = TRUE)
  prereq_ids <- task_obj$prereq_ids(id = "arm", throttle = FALSE)
  unique(prereq_ids[[.arm]])
}

#' Merge arms
#'
#' @param taskname A task name
#' @param merge The arm name to merge across
#' @export
`%merge%` <- function(taskname, merge){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$add_merge(merge)
  invisible(taskname)
}

#' Split arms
#'
#' @param taskname A task name
#' @param split The arm name to split across
#' @export
`%split%` <- function(taskname, split){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$add_split(split)
  invisible(taskname)
}

#' Load and merge all RDS files
#'
#' When a task %merge% a particular arm, then you may wish to load all the
#' output from the unmerged task.
#'
#' All files are loaded, and joined into a list
#' @param ... Passed to rds_file
#' @return A list, each component of which corresponds to a unmerged arm
#' @export
read_rds_merge <- function(...){
  all_arms <- prereq_arm_ids(.task)
  all_paths <- sapply(all_arms, function(x){
    rds_file(..., arm = x)
  })
  paths_exist <- file.exists(all_paths)
  if (!all(paths_exist)){
    message("The following paths don't exist:", all_paths[!paths_exist])
  }
  lapply(all_paths, readRDS)
}

expand_grid <- function(...){
  expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}

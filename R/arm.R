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

#' All arms of a task
#'
#' If expand_split = TRUE, then split arms are treated as standard arms
#' If expand_split = FALSE, then split arms are repeated nrow(expand.grid)
#' times
#'
#' @param task A task name
#' @param expand_split Determine the handling of split arms
#' @return A list of all arms for the task
arms_all <- function(task, expand_split = TRUE){
  task_obj <- task_get(task, exists = TRUE)
  task_obj$arms(expand_split = expand_split)
}

#' Unique name for arm
#'
#' Create a character name for a particular arm
#'
#' If expand_split = TRUE, then split arms are treated as standard arms
#' If expand_split = FALSE, then split arms are repeated nrow(expand.grid)
#' times
#'
#' NOTE THIS SEEMS TO DUPLICATE taskarm_name??
#'
#' @param expand_split Determine the handling of split arms
#' @return A character vector of length 1, the arm_name
arm_name <- function(expand_split = TRUE){
  arms <- arms_all(.task, expand_split = expand_split)

  arm <- arms[[.arm]]
  o <- order(names(arm))
  arm <- arm[o]
  arm_values <- sapply(arm, paste, collapse = "-")
  arm_values_long <- nchar(arm_values) > 10
  arm_values[arm_values_long] <- sapply(arm_values[arm_values_long],
                                        digest::digest, algo = "xxhash32")
  arm_names <- names(arm)
  paste(arm_names, arm_values, sep = "--",  collapse = "__")
}

#' The arm names prior to merging
#'
#' @return A list of arm names
arm_name_merge <- function(){
  arms <- arms_all(.task)

  arm <- arms[[.arm]]
  o <- order(names(arm))
  arm <- arm[o]
  arml <- lapply(arm, as.character)
  grid <- do.call("expand.grid", arml)
  apply(grid, 1, function(arm){
    arm_values <- sapply(arm, paste, collapse = ",")
    arm_names <- names(arm)
    paste(arm_names, arm_values, sep = "--",  collapse = "__")
  })
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
  all_arms <- arm_name_merge()
  all_paths <- sapply(all_arms, function(x){
    rds_file(..., arm = x)
  })
  lapply(all_paths, readRDS)
}

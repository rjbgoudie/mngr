#' Set the memory requirements for a task
#'
#' @param taskname A task
#' @param memory An integer number of megabytes that the task will need
#' @return Invisibly returns the taskname
#' @export
`%memory%` <- function(taskname, memory){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$set_properties(memory = memory)
  invisible(taskname)
}

#' Set the number of cores for a task
#'
#' @param taskname A task
#' @param cores An integer number of cores needed by the task
#' @export
`%cores%` <- function(taskname, cores){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$set_properties(cores = cores)
  invisible(taskname)
}

#' Set the number of hours for a task (walltime)
#'
#' @param taskname A task
#' @param hours A number of hours (walltime) that the task will need
#' @export
`%hours%` <- function(taskname, hours){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$set_properties(hours = hours)
  invisible(taskname)
}

#' Throttle the number of tasks to run simultaneously
#'
#' @param taskname A task
#' @param throttle The maximum number of task to run simultaneously
#' @export
`%throttle%` <- function(taskname, throttle){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$set_properties(throttle = throttle)
  invisible(taskname)
}

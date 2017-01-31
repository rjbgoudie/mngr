#' @export
`%memory%` <- function(taskname, memory){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$set_properties(memory = memory)
  invisible(taskname)
}

#' @export
`%cores%` <- function(taskname, cores){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$set_properties(cores = cores)
  invisible(taskname)
}

#' @export
`%hours%` <- function(taskname, hours){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$set_properties(hours = hours)
  invisible(taskname)
}

#' @export
`%throttle%` <- function(taskname, throttle){
  stopifnot(inherits(taskname, "character"))
  task_obj <- task_get(taskname, exists = TRUE)
  task_obj$set_properties(throttle = throttle)
  invisible(taskname)
}

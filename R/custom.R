#' @export
`%memory%` <- function(taskname, memory){
  stopifnot(inherits(taskname, "character"))
  task_id <- task_find_id(taskname, exists = TRUE)
  task_env$tasklist[[task_id]]$set_properties(memory = memory)
  invisible(taskname)
}

#' @export
`%cores%` <- function(taskname, cores){
  stopifnot(inherits(taskname, "character"))
  task_id <- task_find_id(taskname, exists = TRUE)
  task_env$tasklist[[task_id]]$set_properties(cores = cores)
  invisible(taskname)
}

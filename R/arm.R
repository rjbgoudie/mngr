#' Build factorial experiment
#' @param ... passed to expand.grid
#' @export
arms_factorial <- function(...){
  arms <- expand.grid(...)
  current <- task_env$config
  current$array <- nrow(arms)
  assign("config", current, envir = task_env)
  assign("arms", arms, envir = task_env)
}

#' Unique name for arm
arm_name <- function(){
  arm <- task_env$arms[.arm, ]
  o <- order(names(arm))
  paste(.arm,
        paste(names(arm)[o], arm[o],  sep = ":",  collapse = ","),
        sep = "-")
}

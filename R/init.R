#' @export
startup <- function(){
  if (!interactive()){
    args <- commandArgs(TRUE)
    eval.parent(parse(text = paste(".job <- ", args[[1]])))
    cat("Time:", format(Sys.time(), "%a %b %d %H:%M:%S %Y"), "\n")
    cat("Working directory:", getwd(), "\n")
    cat("Job:", .job, "\n")
    if (args[[2]] != "undefined"){
      eval.parent(parse(text = paste(".arm <- ", args[[2]])))
    }
    else {
      eval.parent(parse(text = paste(".arm <- 1")))
    }
    cat("Arm:", .arm, "\n")
  }
  else {
    message("INTERACTIVE MODE: assuming .arm = 1\n")
    assign(x = ".arm", value = 1, envir = parent.frame(1))
  }
  mngrfile <- find_mngrfile(getwd())
  source(mngrfile)
  arm <- task_env$arms[.arm, ]
  attach(arm)
  cat("Arm values\n")
  unlist(arm)
}

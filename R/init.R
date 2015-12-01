#' @export
startup <- function(){
  if (!interactive()){
    args <- commandArgs(TRUE)
    eval.parent(parse(text = paste(".job <- ", args[[1]])))
    cat("Time:", format(Sys.time(), "%a %b %d %H:%M:%S %Y"), "\n")
    cat("Working directory:", getwd(), "\n")
    cat("Job:", .job, "\n")
    if (args[[2]] != "undefined"){
      eval.parent(parse(text = paste0(".task <- \"", args[[2]], "\"")))
    }
    else {
      eval.parent(parse(text = paste(".task <- \"default\"")))
    }
    cat("Task:", .task, "\n")
    if (args[[3]] != "undefined"){
      eval.parent(parse(text = paste(".arm <- ", args[[3]])))
    }
    else {
      eval.parent(parse(text = paste(".arm <- 1")))
    }
    cat("Arm:", .arm, "\n")
  }
  else {
    if (exists(".arm", envir = parent.frame(1))){
      .arm <- get(".arm", envir = parent.frame(1))
    } else {
      .arm <- 1
    }
    assign(x = ".arm", value = .arm, envir = parent.frame(1))
    message("INTERACTIVE MODE: .arm = ", .arm, "\n")
  }
  mngrfile <- find_mngrfile(getwd())
  source(mngrfile)
  arms <- arms_all(.task, include_shared = TRUE)
  arm <- arms[[.arm]]
  attach(arm)
  cat("Arm values\n")
  unlist(arm)
}

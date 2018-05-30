#' Startup mngr
#'
#' This function set up the .task, .job and .arm variables for non-interactive
#' jobs, and loads the Mngrfile, so that the arm values are available
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
  task_obj <- task_get(.task, exists = TRUE)
  arms <- task_obj$arms_to_invoke()
  if (.arm > nrow(arms)){
    stop("Can't find arm ", .arm, "; there are only ", nrow(arms), " arms")
  }
  arm <- lapply(arms[.arm, ], unlist, recursive = FALSE)
  attach(arm)
  cat("\nArm values\n")
  cat(paste(pretty_print_arm_values(arm), collapse = "\n"), "\n\n")
}

#' Print the arm values prettily
#' @param x A set of arm values
pretty_print_arm_values <- function(x){
  capture.output(str(x,
                     give.length = FALSE,
                     give.attr = FALSE,
                     no.list = TRUE,
                     comp.str = "",
                     indent.str = ""))
}

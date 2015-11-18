#' @export
rjb_init <- function(){
  if (!interactive()){
    args <- commandArgs(TRUE)
    eval.parent(parse(text = paste("jid <- '", args[[1]], "'")))
    cat("Time:", format(Sys.time(), "%a %b %d %H:%M:%S %Y"), "\n")
    cat("wd:", getwd(), "\n")
    cat("jobID:", jobID, "\n")
    if (args[[2]] != "undefined"){
      eval.parent(parse(text = paste("jobName <- ", shQuote(as.character(args[[2]])))))
    }
    else {
      eval.parent(parse(text = paste("jobName <- 'Untitled'")))
    }
    cat("jobName:", jobName, "\n")
    if (args[[3]] != "undefined"){
      eval.parent(parse(text = paste("taskID <- ", args[[3]])))
    }
    else {
      eval.parent(parse(text = paste("taskID <- 1")))
    }
    cat("taskID:", taskID, "\n")
    if (length(args) > 3) {
      for (i in 4:length(args)){
        ai <- args[[i]]
        if (isTRUE(grepl("=", ai))){
          eval.parent(parse(text = paste(ai)))
        }
        else {
          settrue <- " <- T"
          eval.parent(parse(text = paste(ai, settrue)))
        }
      }
      cat("More arguments loaded..\n")
    }
  }
  else {
    warning("Not in BATCH mode - not loading arguments\n")
  }
}

#' Count warnings in file
#'
#' @param log the log file
count_rout_warnings <- function(log){
  cmd <- paste('grep -c -i "^Warning"', log)
  system(cmd, intern = TRUE)
}

#' Count errors in file
#'
#' @param log the log file
count_rout_errors <- function(log){
  cmd <- paste('grep -c -i "^Error"', log)
  system(cmd, intern = TRUE)
}

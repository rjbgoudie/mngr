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

#' Parse .Rout files to a data frame
#'
#' @param paths Character vector of paths to Rout files
parse_rout_files <- function(paths){
  warnings <- unname(sapply(paths, count_rout_warnings))
  errors <- unname(sapply(paths, count_rout_errors))
  warnings <- ifelse(warnings == "1",
                     paste0("\x1b[43m\x1b[30m", warnings, "w\x1b[0m"),
                     "")
  errors <- ifelse(errors == "1", paste0("\x1b[41m", errors, "e\x1b[0m"), "")

  final <- unname(sapply(paths, file_last_line))

  basenames <- basename(paths)
  data.frame(names = basenames, w = warnings, e = errors, f = final)
}

run_time <- function(path){
  final <- file_last_line(path)
  m <- regexec("1;42;30m([^h]+)h ([^m]+)m ([^s]+)s", final)
  if (m[[1]][1] != -1){
    as.numeric(regmatches(final, m)[[1]][2:4])
  } else {
    NULL
  }
}

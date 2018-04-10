#' Count warnings in file
#'
#' @param log the log file
count_rout_warnings <- function(log){
  matches <- grep("^Warning", readLines(log, warn = FALSE))
  length(matches)
}

#' Count errors in file
#'
#' @param log the log file
count_rout_errors <- function(log){
  matches <- grep("^Error", readLines(log, warn = FALSE))
  length(matches)
}

#' Parse .Rout files to a data frame
#'
#' @param paths Character vector of paths to Rout files
#'
#' A data.frame with the following columns:
#'
#' jid, jobname, warnings, errors, internal_run_seconds, final
#'
#' plus
#'
#' arm___xyz columns for each arm value
parse_rout_files <- function(paths, type = "rout"){
  warnings <- unname(sapply(paths, count_rout_warnings))
  errors <- unname(sapply(paths, count_rout_errors))
  final <- unname(sapply(paths, file_last_line))

  # get basename, then remove ".Rout" from the end
  basenames <- basename(paths)
  filenames <- substr(basenames, 1, nchar(basenames) - 5)

  x <- data.frame(name = filenames,
                  warnings = warnings,
                  errors = errors,
                  final = final,
                  stringsAsFactors = FALSE)
  if (nrow(x) > 0){
    x <- x %>%
      do(parse_name(., type = type))
    x <- all_run_times(x)
    x$success <- success(x)
    x <- x %>%
      mutate(final = if_else(success == TRUE, "", final)) %>%
      select(jobname, jid, everything()) %>%
      select(-matches("final"), final)
  } else {
    warning("No logs found")
    NULL
  }
  x
}

#' Parse the run time of a file
#'
#' Assumes the runtime appears in green on the final line of the R output
#' @param path A path to an Rout log file
run_time <- function(path){
  final <- file_last_line(path)
  m <- regexec("1;42;30m([^h]+)h ([^m]+)m ([^s]+)s", final)
  if (m[[1]][1] != -1){
    as.numeric(regmatches(final, m)[[1]][2:4])
  } else {
    NULL
  }
}

#' Determine whether R job ran successfully
#'
#' @param x A job row?
success <- function(x){
  grepl("[0-9]+h [0-9]+m [0-9]+s", x$final)
}

#' Parse all run times
#'
#' @param x A data.frame?
all_run_times <- function(x){
  m <- regexec("1;42;30m([^h]+)h ([^m]+)m ([^s]+)s", x$final)
  d <- as.data.frame(t(sapply(regmatches(x$final, m), "[", 2:4)),
                     stringsAsFactors = FALSE)
  d <- d %>%
    rename(h = V1, m = V2, s = V3) %>%
    mutate_all(as.numeric) %>%
    transmute(internal_run_seconds = 360 * h + 60 * m + s)
  cbind(x, d)
}

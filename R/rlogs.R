#' Count warnings in file
#'
#' @param log the log file
count_rout_warnings <- function(log){
  cmd <- paste('grep -c -i "^Warning"', log)
  suppressWarnings(system(cmd, intern = TRUE))
}

#' Count errors in file
#'
#' @param log the log file
count_rout_errors <- function(log){
  cmd <- paste('grep -c -i "^Error"', log)
  suppressWarnings(system(cmd, intern = TRUE))
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

run_time <- function(path){
  final <- file_last_line(path)
  m <- regexec("1;42;30m([^h]+)h ([^m]+)m ([^s]+)s", final)
  if (m[[1]][1] != -1){
    as.numeric(regmatches(final, m)[[1]][2:4])
  } else {
    NULL
  }
}

success <- function(x){
  grepl("[0-9]+h [0-9]+m [0-9]+s", x$final)
}

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

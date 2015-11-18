#' @export
monitor <- function(){
  suppressWarnings({
    suppressMessages({
      library(ductr)
      args <- commandArgs(TRUE)
      sp <- strsplit(args[[1]], " ")[[1]]
      my_jobs <- system(paste("squeue --jobs",  sp[1]),
                        intern = TRUE,
                        ignore.stderr = TRUE)
      jids <- strsplit(sp[1], ",")[[1]]
      paths <- Sys.glob(paste0(sp[2], "/", jids, "*.Rout"))
      basenames <- basename(paths)
      warnings <- unname(sapply(paths, ductr:::count_rout_warnings))
      errors <- unname(sapply(paths, ductr:::count_rout_errors))
      warnings <- ifelse(warnings == "1",
                         paste0("\x1b[43m\x1b[30m", warnings, "w\x1b[0m"),
                         "")
      errors <- ifelse(errors == "1", paste0("\x1b[41m", errors, "e\x1b[0m"), "")
      last_line <- function(x){
        system(paste("tail -n 1 ", x, " | sed '2d' $1"), intern = TRUE)
      }
      final <- unname(sapply(paths, last_line))

      if (length(attributes(my_jobs)$status) == 0){
        jobs <- read.table(text = my_jobs, header = TRUE)
      } else {
        jobs <- "Job does not exist"
      }
    })
  })

  out <- data.frame(names = basenames, w = warnings, e = errors, f = final)
  if (nrow(jobs) > 0){
    print(jobs)
  }

  for (i in seq_len(nrow(out))){
    cat(as.matrix(out)[i, ])
    cat("\n")
  }

  if (sum(errors == "1") > 0){
    cat("\u274c  Note errors")
  }
  if (sum(warnings == "1") > 0){
    cat("\u274c  Note warnings")
  }
}

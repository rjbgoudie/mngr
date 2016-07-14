#' Evaluate expression with working directory
#'
#' @param dir path to directory in which to evaluate expression
#' @param expr an expression
with_dir <- function(dir,  expr){
  old_wd <- getwd()
  setwd(dir)
  on.exit(setwd(old_wd))
  evalq(expr)
}

#' Manipuate a path
#'
#' uses sub for grep replacement
#'
#' @param pattern character string containing a regular expression
#' @param replacement a replacement for matched pattern
#' @param dir a directory to tack on the end of the path
#' @export
sub_path <- function(pattern, replacement, dir){
  function(path){
    path <- normalizePath(path, mustWork = FALSE)
    out <- sub(pattern = pattern,
               replacement = replacement,
               x = path)
    file.path(out, dir)
  }
}

#' Find relative path to dir from start
#'
#' @param dir the directory to find a path to
#' @param start the starting location
#' @param file.sep path separator
#' @param up_one platform specific up one level notation
#' @param same platform specific same directory
rel_path <- function(dir,
                     start,
                     file.sep = .Platform$file.sep,
                     up_one = "..",
                     same = "."){
  split_path <- function(path){
    strsplit(path, split = file.sep, fixed = TRUE)[[1]]
  }
  dirs <- split_path(normalizePath(dir, mustWork = F))
  starts <- split_path(normalizePath(start, mustWork = F))
  dirs_length <- length(dirs)
  starts_length <- length(starts)
  common_length <- min(starts_length, dirs_length)
  common_range <- seq_len(common_length)

  dirs_tail <- dirs_length - common_length
  starts_tail <- starts_length - common_length
  dirs_tail_range <- seq_len(dirs_tail) + common_length
  starts_tail_range <- seq_len(starts_tail) + common_length

  i <- match(TRUE,
             starts[common_range] != dirs[common_range],
             nomatch = NA) - 1

  if (is.na(i)){
    # no difference in the common range
    ups <- rep(up_one, times = starts_tail)
    tails <- dirs[dirs_tail_range]
  } else {
    # difference in the common range
    how_many_up <- (common_length - i) + starts_tail
    ups <- rep(up_one, times = starts_tail)

    tails_range <- seq_len(common_length - i) + i
    tails <- c(dirs[c(tails_range, dirs_tail_range)])
  }
  out <- do.call("file.path", as.list(c(ups, tails)))
  if (length(out) == 0){
    same
  } else {
    out
  }
}

#' Ensure directory exists
#'
#' @param dir path to the directory
ensure_exists <- function(dir){
  for (x in dir){
    if (!file.exists(x)){
      dir.create(x,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
  }
}

#' Evalutate system command in directory
#'
#' Wrapper around system to execute a system command in a different
#' directory to the current one
#'
#' @param command a system call
#' @param dir path to directory
#' @param ... passed to system()
system_in_dir <- function(command, dir, ...){
  system(command = paste0("(cd ", dir, "; ", command, ")"), ...)
}

#' Get the final line of a file
#'
#' Internally uses system calls to tail and sed
#'
#' @param path Path to the file
file_last_line <- function(path){
  out <- system(paste("tail -n 1 ", path, " | sed '2d' $1"), intern = TRUE)
  # if the last line is very long, R splits it into pieces
  out <- paste(out, collapse = "")

  is_multiline <- grepl("\r", out, fixed = TRUE)
  if (is_multiline){
    pieces <- strsplit(out, "\r", fixed = T)[[1]]
    tail(pieces, 1)
  } else {
    out
  }
}

#' Concatenate and print a data.frame
#'
#' Output a data.frame object using cat. This is useful so that raw bash
#' calls (for colours etc) are printed correctly
#'
#' @param x A data frame
cat_df <- function(x){
  for (i in seq_len(nrow(x))){
    cat(as.matrix(x)[i, ])
    cat("\n")
  }
}

`%||%` <- function(a,  b) if (is.null(a)) b else a

debug_msg <- function(debug, ...){
  if (debug){
    message(...)
  }
}

not.null <- function(x){
  !is.null(x)
}

set_terminal_width <- function(){
  width <- as.numeric(system("tput cols", intern = T))
  options(width = width)
}

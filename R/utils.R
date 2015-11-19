with_dir <- function(dir,  expr){
  old_wd <- getwd()
  setwd(dir)
  on.exit(setwd(old_wd))
  evalq(expr)
}

# uses sub for grep replacement
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

ensure_exists <- function(directories){
  for (dir in directories){
    if (!file.exists(dir)){
      dir.create(dir,
                 recursive = TRUE,
                 showWarnings = FALSE)
    }
  }
}

system_in_dir <- function(command, dir, ...){
  system(command = paste0("(cd ", dir, "; ", command, ")"), ...)
}

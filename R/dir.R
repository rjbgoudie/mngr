#' Check if dir is inside git work tree
#'
#' @param dir A directory
is_git_dir <- function(dir = getwd()){
  command <- "git rev-parse --is-inside-work-tree"
  out <- NA
  out <- suppressWarnings({
    try(system_in_dir(command, dir = dir, intern = TRUE, ignore.stderr = TRUE),
        silent = T)
  })
  if (length(out) > 0 && !is.na(out) && out == "true"){
    TRUE
  } else {
    FALSE
  }
}

#' Get the toplevel git directory
#'
#' @param A directory
git_toplevel_dir <- function(dir = getwd()){
  command <- "git rev-parse --show-toplevel"
  system_in_dir(command, dir = dir, intern = TRUE)
}

#' Check if dir is a run directory
#'
#' @param dir A directory
is_run_dir <- function(dir = getwd()){
  home <- Sys.getenv("HOME")
  pattern <- paste0("^", home, "/run")
  grepl(pattern, dir)
}

#' Git toplevel directory of the run directory
#'
#' Return the path to the git toplevel (ie the root directory) of the
#' corresponding run directory for the supplied directory.
#' If the supplied dir is a run directory, then the git toplevel of that dir is
#' returned.
#'
#' @param dir A directory
run_git_toplevel_dir <- function(dir = getwd()){
  git_toplevel <- git_toplevel_dir(dir = dir)
  if (!is_run_dir(dir)){
    git_abbrev_ref <- git_abbrev_ref(dir = dir, base_only = TRUE)
    home <- Sys.getenv("HOME")
    git_toplevel_from_home <- rel_path(dir = git_toplevel, start = home)
    paste0(home, "/run/", git_toplevel_from_home, "/", git_abbrev_ref)
  } else {
    git_toplevel
  }
}

#' Corresponding run directory
#'
#' Return the path to the corresponding run directory for the supplied
#' directory. If the supplied dir is a run directory, then the supplied
#' directory is returned.
#'
#' @param dir A directory
run_dir <- function(dir = getwd()){
  git_toplevel <- git_toplevel_dir(dir = dir)
  dir_from_git_toplevel <- rel_path(dir = dir, start = git_toplevel)
  run_git_toplevel_dir <- run_git_toplevel_dir()
  paste0(run_git_toplevel_dir, "/", dir_from_git_toplevel)
}

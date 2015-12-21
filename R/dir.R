#' Check if dir is inside git work tree
#'
#' Uses git rev-parse to test whether the supplied directory is a git work
#' tree
#'
#' @param dir A directory
is_inside_git_work_tree <- function(dir = getwd()){
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
#' @param dir A directory
#' @param check Logical, if TRUE no test for whether dir is inside a git work
#' tree
git_toplevel_dir <- function(dir = getwd(), check = TRUE){
  if (check){
    stopifnot(is_inside_git_work_tree(dir))
  }

  command <- "git rev-parse --show-toplevel"
  system_in_dir(command, dir = dir, intern = TRUE)
}

#' Check if dir is a run directory
#'
#' Simply checks whether dir start with $HOME/run
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
#' @param check Logical, if FALSE no test for whether dir is inside a git work
#' tree
run_git_toplevel_dir <- function(dir = getwd(), check = TRUE){
  if (check){
    stopifnot(is_inside_git_work_tree(dir))
  }

  git_toplevel <- git_toplevel_dir(dir = dir, check = FALSE)
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
#' @param check Logical, if FALSE no test for whether dir is inside a git work
#' tree
run_dir <- function(dir = getwd(), check = TRUE){
  if (check){
    stopifnot(is_inside_git_work_tree(dir))
  }

  git_toplevel <- git_toplevel_dir(dir = dir, check = FALSE)
  dir_from_git_toplevel <- rel_path(dir = dir, start = git_toplevel)
  run_git_toplevel_dir <- run_git_toplevel_dir(check = FALSE)
  paste0(run_git_toplevel_dir, "/", dir_from_git_toplevel)
}

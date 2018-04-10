#' Check if dir is inside git work tree
#'
#' Uses git rev-parse to test whether the supplied directory is a git work
#' tree.
#'
#' @param dir A directory, by default the current working directory
#' @return A logical indicating whether dir is inside a git work tree. Returns
#'   FALSE if there are errors or warnings when running rev-parse.
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
#' @param dir A directory, by default the current working directory
#' @param check Logical, if TRUE no test for whether dir is inside a git work
#' tree
#' @return The path to the toplevel git directory
git_toplevel_dir <- function(dir = getwd(), check = TRUE){
  if (check){
    stopifnot(is_inside_git_work_tree(dir))
  }

  command <- "git rev-parse --show-toplevel"
  system_in_dir(command, dir = dir, intern = TRUE)
}

#' Check if dir is a run directory
#'
#' simply checks whether dir start with $HOME/run
#'
#' @param dir A directory
#' @return A logical vector indicating whether the directory starts with
#' $HOME/run
is_run_dir <- function(dir = getwd()){
  pattern <- paste0("run")
  grepl(pattern, dir)
}

#' Git toplevel directory of the run directory
#'
#' Return the path to the git toplevel (ie the root directory) of the
#' corresponding run directory for the supplied directory.
#'
#' The path: $HOME/foo/gittoplevel/folder
#' is converted to: $HOME/run/foo/gittoplevel
#'
#' If the supplied dir is a run directory, then the git toplevel of that dir is
#' returned.
#'
#' @param dir A directory
#' @param check Logical, if FALSE no test for whether dir is inside a git work
#' tree
#' @return Path to the git toplevel for the run directory corresponding to dir
run_git_toplevel_dir <- function(dir = getwd(), check = TRUE){
  if (check){
    stopifnot(is_inside_git_work_tree(dir))
  }

  run_path_fun <- getOption("mngr_run_path")
  run_path <- run_path_fun(normalizePath(dir, winslash = "/"))

  git_toplevel <- git_toplevel_dir(dir = dir, check = FALSE)
  if (!is_run_dir(dir)){
    git_abbrev_ref <- git_abbrev_ref(dir = dir, base_only = TRUE)
    git_toplevel_name <- basename(git_toplevel)

    normalizePath(file.path(run_path, git_toplevel_name, git_abbrev_ref))
  } else {
    normalizePath(git_toplevel)
  }
}

#' Corresponding run directory
#'
#' Converts a directory to the corresponding run directory. For example, the
#' path $HOME/foo/gittoplevel/folder is converted to
#' $HOME/run/foo/gittoplevel/folder
#'
#' @param dir A directory
#' @param check Logical, if FALSE no test for whether dir is inside a git work
#' tree
#' @return Return the path to the corresponding run directory for the supplied
#' directory. If the supplied dir is a run directory, then the supplied
#' directory is returned.
run_dir <- function(dir = getwd(), check = TRUE){
  if (check){
    stopifnot(is_inside_git_work_tree(dir))
  }

  git_toplevel <- git_toplevel_dir(dir = dir, check = FALSE)
  dir_from_git_toplevel <- rel_path(dir = dir, start = git_toplevel)
  run_git_toplevel_dir <- run_git_toplevel_dir(check = FALSE)
  file.path(run_git_toplevel_dir, dir_from_git_toplevel)
}

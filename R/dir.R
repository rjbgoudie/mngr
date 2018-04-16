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
dir_git_toplevel <- function(dir = getwd(), check = TRUE){
  if (check){
    stopifnot(is_inside_git_work_tree(dir))
  }

  command <- "git rev-parse --show-toplevel"
  system_in_dir(command, dir = dir, intern = TRUE)
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
dir_run_branch_toplevel <- function(dir = getwd(), check = TRUE){
  if (check){
    stopifnot(is_inside_git_work_tree(dir))
  }
  dir <- fs::path_tidy(dir)
  dir_git_toplevel <- dir_git_toplevel(dir = dir, check = FALSE)

  # Convert git_toplevel path to rundir equivalent
  dir_run_toplevel_fun <- mngr_option_dir_run()
  dir_run_toplevel <- dir_run_toplevel(dir_git_toplevel)

  if (run_path == dir_git_toplevel){
    # if run_path_fun returns input, then assume we are already in rundir
    dir_git_toplevel
  } else {
    git_abbrev_ref <- git_abbrev_ref(dir = dir, base_only = TRUE)
    fs::path(dir_run_toplevel, git_abbrev_ref)
  }
}

#' Corresponding run directory
#'
#' If passed ~/analyses/project/folder/, where ~/analyses/project is the git
#' toplevel folder, will return
#'
#' mngr_option_run_path("$GIT_TOPLEVEL")/$GIT_BRANCH/folder/
#'
#' If mngr_option_run_path($GIT_TOPLEVEL)=$GIT_TOPLEVEL, then just returns what
#' was supplied
#'
#' @param dir A directory
#' @param check Logical, if FALSE no test for whether dir is inside a git work
#' tree
#' @return Return the path to the corresponding run directory for the supplied
#' directory. If the supplied dir is a run directory, then the supplied
#' directory is returned.
dir_run_branch <- function(dir = getwd(), check = TRUE){
  if (check){
    stopifnot(is_inside_git_work_tree(dir))
  }

  git_toplevel <- dir_git_toplevel(dir = dir, check = FALSE)
  dir_from_git_toplevel <- fs::path_rel(path = dir, start = git_toplevel)
  dir_run_branch_toplevel <- dir_run_branch_toplevel(dir = dir, check = FALSE)
  file.path(dir_run_branch_toplevel, dir_from_git_toplevel)
}

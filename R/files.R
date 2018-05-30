#' Build a file path
#'
#' @param file file name
#' @param arm make arm-specific
#' @param directory directory name
#' @param extension file extension
#' @param splitting A logical, should the split arm names be combined in a
#'   single part
#' @return A character vector of length 1, the file path
#' @export
file_path <- function(file,
                      arm = arm_id(.task, splitting = splitting),
                      directory = "etc",
                      extension = "",
                      splitting = FALSE){
  base <- mngr_option_dir_output()(dir_run_branch())
  dir <- fs::path(base, directory, arm)
  fs::dir_create(dir)
  fs::path(dir, file, ext = extension)
}

#' path to rds file
#'
#' @param ... passsed to file_path
#' @return A character vector of length 1, the file path
#' @export
rds_file <- function(...){
  file_path(..., directory = "rds", extension = "rds")
}

#' path to pdf file
#'
#' @param ... passsed to file_path
#' @return A character vector of length 1, the file path
#' @export
pdf_file <- function(...){
  file_path(..., directory = "plots", extension = "pdf")
}

#' path to tiff file
#'
#' @param ... passsed to file_path
#' @return A character vector of length 1, the file path
#' @export
tiff_file <- function(...){
  file_path(..., directory = "plots", extension = "tiff")
}

#' path to png file
#'
#' @param ... passsed to file_path
#' @return A character vector of length 1, the file path
#' @export
png_file <- function(...){
  file_path(..., directory = "plots", extension = "png")
}

#' save file
#' @param file file name
#' @param ... passed to default save method
#' @export
save <- function(..., file){
  warning("Please use saveRDS instead")
  saveRDS(..., file = file)
}

#' Save an R object to a rds file
#'
#' Wraps the default \code{\link{saveRDS}} function to allow for handling
#' softlinking, and using tempfile saving.
#'
#' If the option \code{mngr_use_tempfile} is TRUE, then files are first saved
#' to a tempfile, generated using \code{\link{tempfile}}, and then copied
#' to the final destination. This makes saving much faster on some very slow
#' filesystems.
#'
#' @param file A filename file name
#' @param ... passed to default \code{\link{saveRDS}}
#' @export
saveRDS <- function(..., file){
  if (isTRUE(mngr_option_use_tempfile())){
    temp_file <- tempfile()
    base:::saveRDS(..., file = temp_file)
    file.rename(from = temp_file,
                to = file)
  } else {
    unlink(file)
    base:::saveRDS(..., file = file)
  }
}

#' pdf device
#' @param file file name
#' @param ... passed to default pdf
#' @export
pdf <- function(file, ...){
  unlink(file)
  grDevices:::pdf(file = file, ...)
}

#' tiff device
#' @param file file name
#' @param ... passed to default tiff
#' @export
tiff <- function(file, ...){
  unlink(file)
  grDevices:::tiff(file = file, ...)
}

#' png device
#' @param file file name
#' @param ... passed to default png
#' @export
png <- function(file, ...){
  unlink(file)
  grDevices:::png(file = file, ...)
}

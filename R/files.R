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
                      arm = arm_name(expand_split = !splitting),
                      directory = "etc",
                      extension = "",
                      splitting = FALSE){
  output_fun <- task_env$config$output
  np <- normalizePath(".", winslash = "/")
  pre <- output_fun(run_dir())

  directory <- ifelse(directory == "", "", paste0(directory, "/"))
  arm <- paste0(arm, "/")
  extension <- ifelse(extension == "", "", paste0(".", extension))

  path_directory <- file.path(pre, directory, arm)
  ensure_exists(path_directory)
  file.path(path_directory, paste0(file, extension))
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

#' save rds file
#' @param file file name
#' @param ... passed to default save
#' @export
saveRDS <- function(..., file){
  if (isTRUE(getOption("mngr_use_tempfile"))){
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

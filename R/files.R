#' Build a file path
#'
#' @param file file name
#' @param arm make arm-specific
#' @param directory directory name
#' @param extension file extension
#' @export
file_path <- function(file,
                      arm = arm_name(),
                      directory = "etc",
                      extension = ""){
  output_fun <- task_env$config$output
  np <- normalizePath(".")
  pre <- output_fun(run_dir())

  directory <- ifelse(directory == "", "", paste0(directory, "/"))
  arm <- paste0(arm, "/")
  extension <- ifelse(extension == "", "", paste0(".", extension))

  path_directory <- paste0(pre, directory, arm)
  ensure_exists(path_directory)
  paste0(path_directory, file, extension)
}

#' path to rds file
#'
#' @param ... passsed to file_path
#' @export
rds_file <- function(...){
  file_path(..., directory = "rds", extension = "rds")
}

#' path to pdf file
#'
#' @param ... passsed to file_path
#' @export
pdf_file <- function(...){
  file_path(..., directory = "plots", extension = "pdf")
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
  unlink(file)
  base:::saveRDS(..., file = file)

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

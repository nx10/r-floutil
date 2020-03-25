#' Initialize project directory
#'
#' @param dir Project directory
#'
#' @export
init <- function(dir = getwd()) {
  touch_dir('data', dir)
  touch_dir('literature', dir)
  touch_dir('plot', dir)
  touch_script('main.r', dir, 'source(\'data_load.r\')\nlibrary(ggplot2)\nsource(\'utils.r\')')
  touch_script('utils.r', dir, '# helper <- function(x) { }')
  touch_script('data_load.r', dir, '# readxl::read_excel(\'data/data.xlsx\')\n# readr::read_csv(\'data/data.csv\')')
  writeLines(paste0('Initialized project in: \'',dir,'\''))
}

#' Create folder if it does not exist.
#'
#' @param foldername Name of the folder
#' @param dir Directory to create the folder in
#'
#' @export
touch_dir <- function(foldername, dir = getwd()) {
  dir.create(file.path(dir, foldername), showWarnings = FALSE)
}

#' Create folder if it does not exist.
#'
#' @param filename Name of the file
#' @param content File content
#' @param dir Directory to create the file in
#'
#' @export
touch_file <- function(filename, dir = getwd(), content = '') {
  if (!file.exists(filename)) {
    writeLines(
      text = content,
      con = file.path(dir, filename))
  }
}

#' Create script file if it does not exist.
#'
#' @param filename Name of the script file
#' @param content Script content
#' @param dir Directory to create the script in
#'
#' @export
touch_script <- function(filename, dir = getwd(), content = '') {
  if (!endsWith(tolower(filename), '.r')) {
    filename <- paste0(filename, '.r')
  }
  touch_file(filename, dir,
             paste0('# ',filename,'\n# ',Sys.time(),'\n\n',content,'\n'))
}

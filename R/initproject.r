#' Initialize project directory
#'
#' @param dir Project directory
#'
#' @export
init <- function(dir = getwd()) {
  touch_dir("data", dir)
  touch_dir("literature", dir)
  touch_dir("plot", dir)
  touch_script(
    "main.r",
    dir,
    "source(\"data_load.r\")\nlibrary(ggplot2)\nsource(\"utils.r\")"
  )
  touch_script("utils.r", dir, "# helper <- function(x) { }")
  touch_script(
    "data_load.r",
    dir,
    "# readxl::read_excel(\"data/data.xlsx\")\n# readr::read_csv(\"data/data.csv\")"
  )
  touch_file(
    ".gitignore",
    dir,
    paste0(
      get_gitignore(),
      "\n\n# Project structure\nplot/\nliterature/\ndata/\ndist/"
    )
  )
  writeLines(paste0("Initialized project in: \"", dir, "\""))
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
touch_file <- function(filename,
                       dir = getwd(),
                       content = "") {
  if (!file.exists(filename)) {
    writeLines(text = content,
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
touch_script <- function(filename,
                         dir = getwd(),
                         content = "") {
  if (!endsWith(tolower(filename), ".r")) {
    filename <- paste0(filename, ".r")
  }
  touch_file(filename,
             dir,
             paste0("# ", filename, "\n# ", Sys.time(), "\n\n", content, "\n"))
}

#' Returns basic gitignore contents.
#'
#' @return Contents of R.gitignore
#'
get_gitignore <- function() {
  gp <- system.file("extdata", "R.gitignore", package = "floutil")
  paste(readLines(gp, warn = FALSE), collapse = "\n")
}

#' dots in string are replaced with \\\\. to make them regex save.
#'
#' @param s String
#'
#' @return String
dotrepl <- function(s) {
  gsub(".", "\\.", s, fixed = T)
}

#' Get list of files to include in distribution package.
#'
#' Note: Due to limitations of the "zip" package, all files in subfolders will be included and can not be filtered/ignored.
#'
#' @param path Path
#' @param ignore_start Ignore paths that start with this
#' @param ignore_end Ignore paths that end with this (e.g. c(".pdf",".docx") would ignore pdfs and word documents)
#' @param ignore_end Ignore paths that are exactly this (e.g. c("plot") would exclude plot folder)
#' @param dotreplace dots in ignore list are replaced with \\\\. to make them regex save.
#'
#' @return list of files to include in distribution package
#'
get_dist_files <- function(path = getwd(),
                           ignore_start = c(),
                           ignore_end = c(),
                           ignore_full = c(), dotreplace = T) {
  ignore_end_default = dotrepl(c(
      ".Rproj",
      ".Rhistory",
      ".Rapp.history",
      ".Ruserdata",
      ".utf8.md",
      ".knit.md",
      ".tmp",
      ".Rcheck"
    ))
  ignore_start_default = dotrepl(c("~"))
  ignore_full_default = dotrepl(c("dist",".RData",".git",".Rproj.user",".Rbuildignore",".vscode",".Renviron"))

  if (dotreplace) {
    ignore_start <- dotrepl(ignore_start)
    ignore_end <- dotrepl(ignore_end)
    ignore_full <- dotrepl(ignore_full)
  }

  ss <-
    paste0(c(ignore_start_default, ignore_start), collapse = "|")
  ee <-
    paste0(c(ignore_end_default, ignore_end), collapse = "|")
  ff <-
    paste0(c(ignore_full_default, ignore_full), collapse = "|")

  fl <-
    dir(path = path, recursive = F, all.files = T,no.. = T) # the zip package cant handyle subfolders unless they are included completely

  fl[!grepl(paste0("^(",ff,")$|^(", ss, ")|(", ee, ")$"), fl)]
}

#' Build distribution package
#'
#' @param path Path
#'
#' @export
build <- function(path = getwd(), ...) {
  touch_dir("dist", path)
  dn  <-
    paste0("dist/",
           basename(getwd()),
           "_",
           format(Sys.time(), "%y%m%d%H%M"),
           ".zip")
  writeLines(paste0("Creating \'",dn,"\', include:"))
  fn <- get_dist_files(path = path, ...)
  writeLines(paste0("* ",paste0(fn,ifelse(dir.exists(fn), "/*",""))))
  zip::zipr(
    dn,
    fn,
    compression_level = 9
  )
}

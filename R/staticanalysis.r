

file_parse_xml <- function(file) {
    xml2::read_xml(xmlparsedata::xml_parse_data(parse(
        text = readLines(file, warn = FALSE),
        keep.source = TRUE
    )))
}

find_deps <- function(xml) {
    # symbol import: library(ggplot2)
    xp_symbol <-
        "//expr[expr/SYMBOL_FUNCTION_CALL[text() = 'library']]/expr/SYMBOL/text()"
    res_symbol <- paste(xml2::xml_find_all(xml, xp_symbol))
    # string import: library("ggplot2")
    xp_str = "//expr[expr/SYMBOL_FUNCTION_CALL[text() = 'library']]/expr/STR_CONST/text()"
    res_str <- paste(xml2::xml_find_all(xml, xp_str))
    res_str <- substr(res_str, 2, nchar(res_str) - 1) # remove quotes
    # scoped import ggplot2::fun
    xp_scope = "//SYMBOL_PACKAGE/text()"
    res_scope <- paste(xml2::xml_find_all(xml, xp_scope))
    unique(c(res_symbol, res_str, res_scope))
}

find_sources <- function(xml) {
    # string import: source("script.r")
    xp_str = "//expr[expr/SYMBOL_FUNCTION_CALL[text() = 'source']]/expr/STR_CONST/text()"
    res_str <- paste(xml2::xml_find_all(xml, xp_str))
    res_str <- substr(res_str, 2, nchar(res_str) - 1) # remove quotes
    c(res_str)
}

detect_sources <- function(files) {
    deps = c()
    for (file in files) {
        cat(paste0("Analyzing: ", file, "\n"))
        sou <- find_sources(file_parse_xml(file))
        new_sou <- sou[!(sou %in% deps) & !(sou %in% files)]
        deps <- c(deps, new_sou, detect_sources(new_sou))    
    }
    deps
}

#' Lists all packages that get imported by an R script.
#' Detects packages that are imported using library(package)
#' as well as with package::foo
#'
#' @param recursive Detect R scripts that are called using
#' source(script.R) and includes them in the analysis
#'
#' @return Character vector of imported package names
#' @export
detect_dependencies <- function(files, recursive = TRUE) {
    deps = c()
    if (recursive)
        files <- c(files, detect_sources(files))
    for (file in files) {
        xml <- file_parse_xml(file)
        deps <- c(deps, find_deps(xml))    
    }
    unique(deps)
}

#' Lists all packages that get imported by an R script.
#' Detects packages that are imported using library(package)
#' as well as with package::foo
#'
#' @param recursive Detect R scripts that are called using
#' source(script.R) and includes them in the analysis
#'
#' @return data.frame with attached version information for each package
#' @export
report_dependencies <- function(file, recursive = TRUE) {
    do.call(rbind, lapply(detect_dependencies(file, recursive), function(x) {
        list(package = x,
             version = paste(packageVersion(x)))
    }))
}


#cat(paste(file_parse_xml("R/testi.R")))

# function call analysis

find_functions <- function(xml) {
    xp = "/exprlist/expr[LEFT_ASSIGN and expr/FUNCTION]/expr/SYMBOL/text()"
    paste(xml2::xml_find_all(xml, xp))
}

find_function_calls <- function(xml) {
    xp = "//SYMBOL_FUNCTION_CALL/text()"
    paste(xml2::xml_find_all(xml, xp))
}

find_duplicate_functions <- function(xml) {
    funs <- find_functions(xml)
    funs[duplicated(funs)]
}

detect_functions <- function(files, recursive = TRUE) {
    funs <- c()
    if (recursive)
        files <- c(files, detect_sources(files))
    for (file in files) {
        xml <- file_parse_xml(file)
        funs <- c(funs, find_functions(xml))
    }
    funs
}

#' @export
detect_duplicate_functions <- function(files, recursive = TRUE) {
    funs <- detect_functions(files, recursive)
    funs[duplicated(funs)]
}

# todo: also detect calls via sapply/lapply/etc
# todo: also detect functions that get assigned to another variable/parameter (same thing?)
detect_function_calls <- function(files, recursive = TRUE) {
    funs <- c()
    if (recursive)
        files <- c(files, detect_sources(files))
    for (file in files) {
        xml <- file_parse_xml(file)
        funs <- c(funs, find_function_calls(xml))
    }
    funs
}

#' @export
detect_unused_functions <- function(files, recursive = TRUE) {
    if (recursive)
        files <- c(files, detect_sources(files))
    print(files)
    funs <- detect_functions(files, recursive = FALSE)
    fun_calls <- detect_function_calls(files, recursive = FALSE)
    funs[!(funs %in% fun_calls)]
}


#report_dependencies(c("main.r","report.r"))
#detect_unused_functions(c("main.r","report.r"))
#detect_duplicate_functions(c("main.r","report.r"))
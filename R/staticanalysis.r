

file_parse_xml <- function(file) {
    xml2::read_xml(xmlparsedata::xml_parse_data(parse(
        text = readLines(file),
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

#' Lists all packages that get imported by an R script.
#' Detects packages that are imported using library(package)
#' as well as with package::foo
#'
#' @param recursive Detect R scripts that are called using
#' source(script.R) and includes them in the analysis
#'
#' @return Character vector of imported package names
#' @export
detect_dependencies <- function(file, recursive = TRUE) {
    cat(paste0("Analyzing: ", file, "\n"))
    xml <- file_parse_xml(file)
    deps <- find_deps(xml)
    if (recursive) {
        for (so in find_sources(xml)) {
            deps <- c(deps, detect_dependencies(so))
        }
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

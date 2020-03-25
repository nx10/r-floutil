

#' Compare two ID-vectors. Shows duplicates and elements that don't match.
#'
#' @param a Vector 1
#' @param b Vector 2
#' @param re (Optional) return results as a list? For example to use in unit testing.
#'
#' @return if re is set to true a list is returned.
#' @export
#'
#' @examples matchinfo(c("a","b","c"), c("a","b","d","d"))
matchinfo <- function(a,b,re=F) {

  writeLines(paste0("------------ MATCHINFO ------------"))

  acount <- length(a)
  bcount <- length(b)
  writeLines(paste0("A = ",deparse(substitute(a)),", B = ",deparse(substitute(b))))
  writeLines(paste0("A count: ",acount,", B count: ",bcount))

  # in group douplicates
  adupes <- a[which(duplicated(a))]
  bdupes <- b[which(duplicated(b))]
  if (length(adupes) + length(bdupes) > 0) {
    writeLines(paste0("* WARNING: Duplicates found!"))
    if (length(adupes) > 0) {
      writeLines(paste0("A duplicates (",length(adupes),"): \"",paste(adupes,collapse=", "),"\""))
    }
    if (length(bdupes) > 0) {
      writeLines(paste0("B duplicates (",length(bdupes),"): \"",paste(bdupes,collapse=", "),"\""))
    }
  } else {
    writeLines(paste0("* No duplicates found."))
  }

  # inter group matching
  anotb <- a[which(!(a %in% b))]
  bnota <- b[which(!(b %in% a))]
  if (length(anotb)+length(bnota) > 0) {
    writeLines(paste0("* WARNING: Groups do not match!"))
    if (length(anotb) > 0) {
      writeLines(paste0("A elements not in B (",length(anotb),"): \"",paste(anotb,collapse=", "),"\""))
    }
    if (length(bnota) > 0) {
      writeLines(paste0("B elements not in A (",length(bnota),"): \"",paste(bnota,collapse=", "),"\""))
    }
  } else {
    writeLines(paste0("* Groups match."))
  }

  writeLines(paste0("----------------------------------"))

  if (re) { return(list(adupes=adupes,bdupes=bdupes,anotb=anotb,bnota=bnota)) }
}

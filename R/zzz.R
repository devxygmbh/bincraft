utils::globalVariables(c(".", "OS_type", "Package", "%nin%"))

#' Not in operator
#'
#' Returns TRUE for elements not in a set.
#' @param x Vector or NULL: the values to be matched
#' @param table Vector or NULL: the values to be matched against
#' @return A logical vector, indicating if each element of x is NOT in table
#' @usage x \%nin\% table
#' @keywords internal
#' @export
`%nin%` <- function(x, table) match(x, table, nomatch = 0L) <= 0L

#' Is it a 'nothing' value?
#'
#' @param x (`any`).
#' @return (`logical(1)`).\cr
#' `TRUE` if:
#' * `length(x) == 0`
#' * the value is `NA`
#' * the value is `NULL`
#' * the value is `NaN`
is.nothing <- function(x) {
    length(x) == 0 || all(is.na(x)) || all(is.null(x)) || all(is.nan(x))
}

#' @todo test
#' @todo document
is.nothing <- function(x) {
    length(x) == 0 || all(is.na(x)) || all(is.null(x)) || all(is.nan(x))
}

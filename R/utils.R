#' @todo test
#' @todo document
is.nothing <- function(x) {
    length(x) == 0 || all(is.na(x)) || all(is.null(x)) || all(is.nan(x))
}

#' @todo test
#' @todo document
#' @todo document no trailing space on NULL
get_class_value_line_str <- function(name, value) {
    indent <- function(lines) {
        indented_lines <-
            paste0("\t", lines) |>
            paste(collapse = "\n")
        sprintf("[\n%s\n]", indented_lines)
    }

    class_line <- paste(class(value), collapse = ", ")

    value_line <- format(value)
    if (nchar(value_line) > 0) {
        value_line <-
            value_line |>
            strsplit("\n", fixed = TRUE) |>
            unlist(recursive = TRUE)
    }

    if (length(value_line) > 1) {
        value_line <- indent(value_line)
    }

    if (nchar(value_line) > 0) {
        value_line <- paste0(" ", value_line)
    }

    sprintf("%s (%s):%s", name, class_line, value_line)
}

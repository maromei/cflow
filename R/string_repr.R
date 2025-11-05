
#' @todo test
#' @todo document
pretty_print_list <- function(x) {
    value_strs <- lapply(x, get_class_value_line_str, name = "")
    paste0(names(x), value_strs)
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
    if (is.data.frame(value)) {
        value_line <- capture.output(print(value))
    } else if (is.list(value)) {
        value_line <- cflow:::pretty_print_list(value)
    }
    value_line <- paste(value_line, collapse = "\n")

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

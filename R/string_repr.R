
#' Pretty Print List objects.
#'
#' @param x (`list(n)`).
#' @return (`character(n)`).\cr
#' A string where each entry of the list will correspond to a single line,
#' formatted via [`get_class_value_line_str`].
#'
#' @examples
#'
#' list_ <- list(a = "a", b = c(1, 2, 3))
#'
#' pretty_print(list_) |> print()
#' # Output:
#' # [1] "a (character): a"
#' # [2] "b (numeric): [\n    1\n    2\n    3\n]"
#'
#' pretty_print_list(list_) |>
#'     paste(collapse = "\n") |>
#'     cat()
#' # Output:
#' # a (character): a
#' # b (numeric): [
#' #     1
#' #     2
#' #     3
#' # ]
pretty_print_list <- function(x) {
    value_strs <- lapply(x, cflow:::get_class_value_line_str)

    names_ <- names(x)
    is_not_an_empty_name <- names_ != ""
    names_[is_not_an_empty_name] <- paste0(names_[is_not_an_empty_name], " ")

    paste0(names_, value_strs)
}

#' Generate a string for pretty printing a value with its content.
#'
#' Properties:
#' * `NULL`, or `length = 0` values, will have no trailing whitespace.
#' * If `name = NULL`, no leading whitespace will be present,
#'   with `name = ""`, however, it will.
#'
#' @param value (`any`).
#' @param name (`character(1)` or `NULL`).
#' @return (`character(1)`).\cr
#' A string of the format `<NAME> (<CLASS>): <VALUE>`.
#' If the string representation of the `value` has more than one line,
#' it will be wrapped in `[...]` and indented. This also works recursively.
#' See examples.
#'
#' @examples
#'
#' get_class_value_line_str(1L)
#' # Output: "(integer): 1"
#'
#' get_class_value_line_str(NULL)
#' # Output: "(NULL): NULL"
#'
#' get_class_value_line_str("Some Value", name = "Name")
#' # Output: "Name (character): Some Value"
#'
#' list_ <- list(a = "a", b = c(1, 2, 3))
#' get_class_value_line_str(list_, name = "Some List")
#' # Output:
#' # Some List (list): [
#' #     a (character): a
#' #     b (numeric): [
#' #         1
#' #         2
#' #         3
#' #     ]
#' # ]
get_class_value_line_str <- function(value, name = NULL) {
    indent <- function(lines) {
        indented_lines <-
            paste0("    ", lines) |>
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

    if (is.null(name)) {
        name <- ""
    } else {
        name <- paste0(name, " ")
    }

    sprintf("%s(%s):%s", name, class_line, value_line)
}

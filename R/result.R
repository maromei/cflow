#' Result - Control Flow constrcut for a Value or Error
#'
#' @description
#' The idea is to bring a control flow construct used in the `Rust` programming
#' language to R. The idea is to not always call [`stop()`] if an error occurs,
#' or return a magic number indicating the special error state, but instead
#' signal it via this class.
#'
#' To construct a [`Result`] object, use the [`Ok`] and [`Err`] functions,
#' as these handle the creation of the Object without producing an inconsistent
#' internal state. The [`Result`] class is still exported by this packages,
#' since it is a central piece of functionality, but technically it should
#' never be required to call / modify it directory.
#'
#' @examples
#'
#' # Construct an Ok Value
#' result <- Ok(1)
#'
#' # Construct an Error value
#' result <- Err("Some Error Mesage", "Optional Error type")
#'
#' @export
Result <- R6::R6Class(
    "Result",

    public = list(
        #' @description
        #' Construct a [`Result`] value.
        #' @param value (`any`).
        #' @param is_error (`any`).
        #' @param error_type (`any`).
        initialize = function(value, is_error, error_type = NULL) {
            private$.value <- value
            private$.is_error <- is_error
            private$.error_type <- error_type
        },

        #' @description
        #' Get the [`Result`] value without any checks for the current state.
        #' @return (`any`)\cr
        #' Value or error message of the result.
        get = function() private$.value,

        #' @description
        #' Call [`stop()`] with an error message or simply return the value.
        #' @return (`any`).
        get_or_stop = function() {
            if (!self$is_error()) {
                return(self$get())
            }
            self$get_formatted_error_message |>
                stop()
        },

        #' @description
        #' Check if the [`Result`] is an error.
        #' @return (`logical(1)`).
        is_err = function() private$.is_error,

        #' @description
        #' Check if the [`Result`] is ok.
        #' @return (`logical(1)`).
        is_ok = function() !self$is_err(),

        #' @description
        #' Treats the saved `value` as an error message, and constructs a
        #' message to display.
        #' @return (`character(1)`).
        get_formatted_error_message = function() {
            paste("Result value unwrapped to an error of type '%s':\n%s") |>
                sprintf(toString(self$error_type), self$get())
        },

        #' Apply a function to the contained [`Ok`] value, or leave
        #' the [`Err`] as is.
        #'
        #' @param rhs (`expression`)\cr
        #' An expression where the wrapped [`Ok`] [`Result`] value will be
        #' passed as the first argument to the expression.
        #'
        #' @return ([`Result`]).\cr
        #' The evaluated `rhs` call wrapped in a `Result` object.
        #' If `rhs(...)` returns a [`Result`] itself, it will not be
        #' wrapped in another `Ok()` call. [`Results`][`Result`] will not
        #' be nested. An [`Err`] [`Result`] will just be returned as is.
        #'
        #' @examples
        #'
        #' f <- function(x, y, z) c(x, y, z)
        #'
        #' Ok(1)$then(f(2, 3))
        #' # Output: Ok(c(1, 2, 3))
        #'
        #' Err("Some Error Message")$then(f(2, 3))
        #' # Output: Err("Some Error Message")
        #'
        #' Ok(2)$then(f(x = 1, 3))
        #' # Output: Ok(c(1, 2, 3))
        #'
        #' f_err <- function(x) Err(x)
        #' Ok(1)$then(f_err())
        #' # Output: Err(1)
        #'
        #' f_ok <- function(x) Ok(x)
        #' Ok(1)$then(f_ok())
        #' # Output: Ok(1)
        #'
        #' @export
        then = function(rhs) {
            rhs <- substitute(rhs)
            cflow:::then_with_symbols(self, rhs)
        },

        get_str_repr = function() {
            state_str <- "Ok"
            if (self$is_err()) {
                state_str <- "Err"
            }

            value_line <- cflow:::get_class_value_line_str(
                "Value",
                self$get()
            )
            error_type_line <- cflow:::get_class_value_line_str(
                "Error Type",
                self$error_type
            )

            sprintf(
                "Result: %s\n%s\n%s\n",
                state_str,
                value_line,
                error_type_line
            )
        },

        print = function(...) {
            cat(self$get_str_repr())
        }
    ),
    active = list(
        #' @field error_type (`any`).
        error_type = function() private$.error_type
    ),
    private = list(
        # @field .value (`any`)\cr
        # Internal / private representation of the value.
        .value = NULL,

        # @field .is_error (`any`)\cr
        # Internal / private representation of whether the object represents an
        # error. Should be set on initialization. There is no other way to
        # detect whether an error is saved, other than checking for an
        # `.error_type`. However, that may be [`NULL`].
        .is_error = NULL,

        # @field .error_type (`any`)\cr
        # Internal / private representation of the error type.
        # It may be [NULL] and does not need to be specified.
        .error_type = NULL
    )
)

#' Construct an `Ok` [`Result`] value.
#'
#' @param ok_value (`any`).
#' @return ([`Result`]).
#'
#' @export
Ok <- function(ok_value) {
    cflow::Result$new(
        value = ok_value,
        is_error = FALSE
    )
}

#' Construct an `Err` [`Result`] value.
#'
#' @param error_value (`any`).
#' @param error_type (`any`).
#' @return ([`Result`]).
#'
#' @export
Err <- function(error_value, error_type = NULL) {
    cflow::Result$new(
        value = error_value,
        is_error = TRUE,
        error_type = error_type
    )
}

#' Is the object a [`Result`]?
#' @param x (`any`)\cr
#' Object to check.
#' @return (`logical(1)`).
#' @export
is.Result <- function(x) {
    R6::is.R6(x) && class(x)[1] == cflow::Result$classname
}

#' @todo write docs
then_with_symbols <- function(lhs, rhs) {
    if (!cflow::is.Result(lhs)) {
        stop(sprintf("'%s' is not a 'cflow::Result' object.", toString(lhs)))
    }

    if (!is.call(rhs)) {
        stop(sprintf("'%s' is not a function call", toString(rhs)))
    }

    if (lhs$is_err()) {
        return(lhs)
    }

    lhs <- lhs$get()

    # the following will generate a list
    # > list(function_name, arg1, arg2, arg3, ...)
    rhs_list <- as.list(rhs)

    max_len <- max(2, length(rhs_list))
    arg_list_seq <- seq(2, max_len)
    # unlist here will trigger NULL to be returned if the length
    # max_len = 2, meaning only a single argument is present in the list
    rhs_args <- unlist(
        rhs_list[arg_list_seq],
        recursive = FALSE,
        use.names = TRUE
    )
    rhs_list <- c(rhs_list[[1]], lhs, rhs_args)

    rhs <- as.call(rhs_list)

    evaluated_call <- tryCatch(
        eval(rhs),
        error = function(e) cflow::Err(toString(e))
    )

    # This check will be true if:
    #  * The `eval(rhs)` resulted in an error, which triggers the `error`
    #    function in the `tryCatch()` statement above.
    #  * The `eval(rhs)` call evaluates to a [`Result`] itself.
    #
    # In both cases it is fine to return early. This avoids nesting
    # [`Result`] values.
    if (cflow::is.Result(evaluated_call)) {
        return(evaluated_call)
    }

    cflow::Ok(evaluated_call)
}

#' @todo write docs
#' @export
`%then%` <- function(lhs, rhs) {
    rhs <- substitute(rhs)
    cflow:::then_with_symbols(lhs, rhs)
}

#' @todo write docs
#' @todo tests
#' @export
`==.Result` <- function(lhs, rhs) {
    lhs_str <- lhs$get_str_repr()
    rhs_str <- rhs$get_str_repr()

    all(lhs_str == rhs_str)
}

#' @todo write docs
#' @todo test
#' @todo test, that it can be used with library(include.only=c("Ok", "Err"))
#'  (It works, but there should be an explicit test case for it)
#' @export
toString.Result <- function(x) {
    x$get_str_repr()
}

#' @todo write docs
#' @export
format.Result <- function(x) {
    x$get_str_repr()
}

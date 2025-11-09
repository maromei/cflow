#' Result - Control Flow constrcut for a Value or Error
#'
#' @description
#' `Result` brings a control flow construct used in the `Rust` programming
#' language to R. The idea is to not always call [`stop()`] if an error occurs,
#' or return a magic number indicating the special error state, but instead
#' signal it via this class.
#'
#' To construct a [`Result`] object, use the [`Ok`] and [`Err`] functions,
#' as these handle the creation of the [`Result.Ok`] and [`Result.Err`]
#' Objects without producing an inconsistent
#' internal state. Generally, the [`Result`] class should not
#' be used directly. The important implementations are found in the
#' [`Result.Ok`] and [`Result.Err`] subclasses. Though not enforeced on a
#' language level, this class should be thought of as a virtual one.
#' It is still exported by this packages,
#' since it is a central piece of functionality.
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
        initialize = function(value) {
            private$.value <- value
        },

        #' @description
        #' Get the [`Result`] value without any checks for the current state.
        #' @return (`any`)\cr
        #' Value or error message of the result.
        get = function() private$.value,

        #' @description
        #' Call [`stop()`] with an error message or simply return the value.
        #'
        #' Calling this function on the base [`Result`] class will cause
        #' an error. The method should be implemented in [`Result.Ok`] and
        #' [`Result.Err`] sub-classes.
        #' @return (`any`).
        get_or_stop = function() {
            paste(
                "Implementation should be done by subclasses",
                "'Result.Ok' and 'Result.Err'.",
            ) |>
                stop()
        },

        #' @description
        #' Check if the [`Result`] is an error.
        #' See also [`is.Err()`].
        #' @return (`logical(1)`).
        is_err = function() cflow::is.Err(self),

        #' @description
        #' Check if the [`Result`] is ok.
        #' See also [`is.Ok()`].
        #' @return (`logical(1)`).
        is_ok = function() cflow::is.Ok(self),

        #' @description
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
        then = function(rhs) {
            .env <- parent.frame(n = 1)
            rhs <- substitute(rhs)
            cflow:::then_with_symbols(self, rhs, .env)
        },

        #' @description
        #' Produce a string representation of the [`Result`] object.
        #'
        #' Calling this function will result in an error, as the sub-classes
        #' are supposed to implement it.
        get_str_repr = function() {
            paste(
                "Implementation should be done by subclasses",
                "'Result.Ok' and 'Result.Err'.",
            ) |>
                stop()
        },

        #' @description
        #' Print the [`Result`] objects string representation
        #'
        #' @param ... Unused additional parameters required to define the
        #' `print()` function in an `R6` class.
        print = function(...) {
            cat(self$get_str_repr())
        }
    ),
    private = list(
        # @field .value (`any`)\cr
        # Internal / private representation of the value.
        .value = NULL
    )
)

#' Result.Ok - Signal the successfull capture of a variable
#'
#' @description
#' This `R6` class represents the 'success' portion of the
#' [`Result`] control flow construct.
#'
#' Internally it just saves a single value, and provides some methods
#' for pretty printing the construct around it.
#'
#' While instances of this class could be created via `Result.Ok::new(...)`,
#' the recommendation is to use [`Ok()`] instead, as it has a cleaner interface.
#'
#' @examples
#'
#' # Constructs a simple Ok value
#' Ok(1)
#'
#' # The 'then' method (and the '%then%' operator) can be used
#' # to pipe the wrapped Ok value to another function, whose output
#' # will also be wrapped.
#'
#' f <- function(x, y) c(x, y)
#' Ok(1)$then(f(2))
#' # Output: Ok(c(1, 2))
#' Ok(1) %then% f(2)
#' # Output: Ok(c(1, 2))
#'
#' @name Result.Ok
#' @export
Result.Ok <- R6::R6Class(
    "Ok",
    inherit = Result,
    public = list(
        #' @description
        #' Returns the value.
        #'
        #' Overriding the base class [`Result$get_or_stop()`][`Result`]
        #' method for simplicity.
        #' @return (`any`).
        get_or_stop = function() sefl$get(),

        #' @description
        #' Produce a string representation of the [`Result.Ok`] object.
        #'
        #' @return (`character(1)`).
        get_str_repr = function() {
            value_line <- cflow:::get_class_value_line_str("Value", self$get())
            sprintf("Result: Ok\n%s\n", value_line)
        }
    )
)

#' Result.Err - Signal the unsuccessfull capture of a variable
#'
#' @description
#' This `R6` class represents the 'failure' portion of the
#' [`Result`] control flow construct.
#'
#' Internally an error message is saved, with an optional error type,
#' to differentiate different sources of errors.
#'
#' While instances of this class could be created via `Result.Err::new(...)`,
#' the recommendation is to use [`Err()`] instead,
#' as it has a cleaner interface.
#'
#' @examples
#'
#' # Constructs a simple Err value
#' Err("Some Error Message", "Some optional Error Type")
#'
#' # The 'then' method (and the '%then%' operator) can be used
#' # to pipe a Result object to another function. In contrast to an
#' # Ok type, the error type will be returned as is, without evaluation of
#' # the function.
#'
#' f <- function(x, y) c(x, y)
#' Err("error")$then(f(2))
#' # Output: Err("error")
#' Err("error") %then% f(2)
#' # Output: Err("error")
#'
#' @name Result.Err
#' @export
Result.Err <- R6::R6Class(
    "Err",
    inherit = Result,
    public = list(
        #' @description
        #' Construct a [`Result`] value.
        #' @param value (`any`).\cr
        #' Will be used as the error message here.
        #' @param error_type (`any`).\cr
        #' Some object representing the type of an error.
        #' Can be `NULL`.
        initialize = function(value, error_type = NULL) {
            private$.value <- value
            private$.error_type <- error_type
        },

        #' @description
        #' Calls [`stop()`] with an error message.
        #'
        #' Overriding the base class [`Result$get_or_stop()`][`Result`]
        #' method for simplicity.
        #' @return (`any`).
        get_or_stop = function() {
            self$get_formatted_error_message |>
                stop()
        },

        #' @description
        #' Treats the saved `value` as an error message, and constructs a
        #' message to display.
        #'
        #' @return (`character(1)`).
        get_formatted_error_message = function() {
            paste("Result value unwrapped to an error of type '%s':\n%s") |>
                sprintf(toString(self$error_type), self$get())
        },

        #' @description
        #' Produce a string representation of the [`Result.Ok`] object.
        #'
        #' @return (`character(1)`).
        get_str_repr = function() {
            value_line <- cflow:::get_class_value_line_str(
                "Message",
                self$get()
            )
            error_type_line <- cflow:::get_class_value_line_str(
                "Error Type",
                self$error_type
            )

            sprintf(
                "Result: Err\n%s\n%s\n",
                value_line,
                error_type_line
            )
        }
    ),
    active = list(
        #' @field error_type (`any`).
        error_type = function() private$.error_type
    ),
    private = list(
        # @field .error_type (`any`)\cr
        # Internal / private representation of the error type.
        # It may be [NULL] and does not need to be specified.
        .error_type = NULL
    )
)

#' Construct a [`Result.Ok`] value.
#'
#' See [`Result.Ok`] for more detail.
#'
#' @param ok_value (`any`).\cr
#' Value to wrap.
#' @return ([`Result.Ok`]).
#'
#' @export
Ok <- function(ok_value) {
    cflow::Result.Ok$new(value = ok_value)
}

#' Construct an [`Result.Err`] value.
#'
#' See [`Result.Err`] for more detail.
#'
#' @param error_value (`any`).\cr
#' Error message to wrap.
#' @param error_type (`any`).\cr
#' Optional error type
#' @return ([`Result.Err`]).
#'
#' @export
Err <- function(error_value, error_type = NULL) {
    cflow::Result.Err$new(
        value = error_value,
        error_type = error_type
    )
}

#' Is the object a [`Result`]?
#' @param x (`any`)\cr
#' Object to check.
#' @return (`logical(1)`).
#'
#' @name is.Result
#' @export
is.Result <- function(x) {
    R6::is.R6(x) && inherits(x, cflow::Result$classname)
}

#' Is the value a [`Result.Ok`] object?
#'
#' @param x (`any`)\cr
#' Object to check.
#'
#' @return (`logical(1)`)
#'
#' @name is.Ok
#' @export
is.Ok <- function(x) {
    class_ <- class(x)
    expected_class <- c(
        cflow::Result.Ok$classname,
        cflow::Result$classname,
        "R6"
    )
    if (length(class_) != length(expected_class)) {
        return(FALSE)
    }

    all(class_ == expected_class)
}

#' Is the value a [`Result.Err`] object?
#'
#' @param x (`any`)\cr
#' Object to check.
#'
#' @return (`logical(1)`)
#'
#' @name is.Err
#' @export
is.Err <- function(x) {
    class_ <- class(x)
    expected_class <- c(
        cflow::Result.Err$classname,
        cflow::Result$classname,
        "R6"
    )
    if (length(class_) != length(expected_class)) {
        return(FALSE)
    }

    all(class_ == expected_class)
}

#' Pass a Result value to an unevaluated function call.
#'
#' This function implements the core logic to the
#' \code{\link{\%then\%}} operator.
#' The main assumption here is that `rhs` is assumed to be a symbolic
#' function call. Both the \code{\link{\%then\%}} operator and the
#' [`Result%then()`][`Result`] function, simply [`substitute`] the function
#' call and pass it to this function for evaluation.
#'
#' @param lhs ([`Result(1)`][`Result`]).\cr
#' The Result object. If it is a [`Result.Ok`] value, it will be passed
#' as the first argument to `rhs`, while a [`Result.Err`] value will be returned
#' as is.
#' @param rhs (`symbol(1)` with `mode(rhs) == "call"`).\cr
#' Unevaluated function call.
#' @param .env (`environment(1)`).\cr
#' The environment to evaluate `rhs` in.
#'
#' @return ([`Result(1)`][`Result`]).\cr
#' The Result of the function call wrapped in a [`Result`] object.
#' If the function call already returns a result, no further wrapping will
#' be done.
then_with_symbols <- function(lhs, rhs, .env = parent.frame()) {
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
        eval(rhs, envir = .env),
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

#' Pass a wrapped Result value to a function call or return the Err as is.
#'
#' @param lhs ([`Result(1)`][`Result`]).
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
#' Ok(1) %then% f(2, 3)
#' # Output: Ok(c(1, 2, 3))
#'
#' Err("Some Error Message") %then% f(2, 3)
#' # Output: Err("Some Error Message")
#'
#' Ok(2) %then% f(x = 1, 3)
#' # Output: Ok(c(1, 2, 3))
#'
#' f_err <- function(x) Err(x)
#' Ok(1) %then% f_err()
#' # Output: Err(1)
#'
#' f_ok <- function(x) Ok(x)
#' Ok(1) %then% f_ok()
#' # Output: Ok(1)
#'
#' @name %then%
#' @export
`%then%` <- function(lhs, rhs) {
    .env <- parent.frame(n = 1)
    rhs <- substitute(rhs)
    cflow:::then_with_symbols(lhs, rhs, .env)
}

#' Compare [`Result`] values for equality
#'
#' Will throw an error if either `lhs` or `rhs` is not a [`Result`] type.
#' See [`is.Result()`].
#'
#' @param lhs ([`Result`]).
#' @param rhs ([`Result`]).
#' @return (`logical(1)`).
#'
#' @export
`==.Result` <- function(lhs, rhs) {
    if (!cflow::is.Result(lhs) || !cflow::is.Result(rhs)) {
        lhs_class_str <- paste(class(lhs), collapse = ", ")
        rhs_class_str <- paste(class(rhs), collapse = ", ")
        sprintf(
            "Cannot compare class '%s' and '%s'",
            lhs_class_str,
            rhs_class_str
        ) |>
            stop()
    }

    lhs_str <- lhs$get_str_repr()
    rhs_str <- rhs$get_str_repr()

    if (length(lhs_str) != length(rhs_str)) {
        return(FALSE)
    }

    all(lhs_str == rhs_str)
}

#' Compare [`Result`] values for inequality
#'
#' @param lhs ([`Result`]).
#' @param rhs ([`Result`]).
#' @return (`logical(1)`).
#'
#' @export
`!=.Result` <- function(lhs, rhs) {
    !(lhs == rhs)
}

#' Simple [`toString`] method for [`Result`]
#'
#' See [`Result.Ok$get_str_repr`][`Result.Ok`] and
#' [`Result.Err$get_str_repr`][`Result.Err`].
#'
#' @return (`character(1)`).
#'
#' @export
toString.Result <- function(x) {
    x$get_str_repr()
}

#' Simple [`format`] method for [`Result`]
#'
#' See [`Result.Ok$get_str_repr`][`Result.Ok`] and
#' [`Result.Err$get_str_repr`][`Result.Err`].
#'
#' @return (`character(1)`).
#'
#' @export
format.Result <- function(x) {
    x$get_str_repr()
}

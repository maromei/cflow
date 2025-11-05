library(cflow, include.only = c(Result, Ok, `%then%`))

testthat::test_that("'then' pipe and function, works with Result Ok LHS", {
    f <- function(x, y) c(x, y)
    result <- Ok(1)

    pipe_result <- result %then% f(2)
    fun_result <- result$then(f(2))

    expectation <- Ok(c(1, 2))
    testthat::expect_equal(pipe_result, expectation)
    testthat::expect_equal(fun_result, expectation)
})

testthat::test_that("'then' pipe and function, works with Result Err LHS", {
    f <- function(x, y) c(x, y)

    simple_then_test <- function(result, expectation) {
        pipe_result <- result %then% f("another")
        fun_result <- result$then(f("another"))

        testthat::expect_equal(pipe_result, expectation)
        testthat::expect_equal(fun_result, expectation)
    }

    result <- Err("Some Error message")
    expectation <- Err("Some Error message")

    simple_then_test(result, expectation)

    result <- Err("Some Error message", error_type = "some_error_type")
    expectation <- Err("Some Error message", error_type = "some_error_type")

    simple_then_test(result, expectation)
})

testthat::test_that("'%then%' works with LHS evaluating to Result Ok", {
    f <- function(x, y) c(x, y)
    lhs_func <- function() Ok(1)

    pipe_result <- lhs_func() %then% f(2)

    expectation <- Ok(c(1, 2))
    testthat::expect_equal(pipe_result, expectation)
})

testthat::test_that(
    "'%then%' returns an error if RHS does not accept any arguments",
    {
        f_no_args <- function() c(1, 2)

        expectation <- Err("Error in f_no_args(1): unused argument (1)")

        result <- Ok(1) %then% f_no_args()
        testthat::expect_equal(result, expectation)

        result <- Ok(1)$then(f_no_args())
        testthat::expect_equal(result, expectation)
    }
)

testthat::test_that(
    "'%then%' returns an error if too many arguments are supplied",
    {
        f_single_arg <- function(a) c(a, 2)

        expectation <- Err("Error in f_single_arg(1, 2): unused argument (2)")

        result <- Ok(1) %then% f_single_arg(1)
        testthat::expect_equal(result, expectation)

        result <- Ok(1)$then(f_single_arg(1))
        testthat::expect_equal(result, expectation)
    }
)


#' @todo continue tests
NULL

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

testthat::test_that("'%then%' works with LHS evaluating to Result Ok", {
    f <- function(x, y) c(x, y)
    lhs_func <- function() Ok(1)

    pipe_result <- lhs_func() %then% f(2)

    expectation <- Ok(c(1, 2))
    testthat::expect_equal(pipe_result, expectation)
})


#' @todo continue tests
NULL

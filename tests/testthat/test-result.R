#' Test the construction and basic usage of the [`Result`] object.
#'
#' The `then` function and special operator `%then%` are tested in `test-then.R`
NULL

testthat::test_that("Result construction via Ok() works.", {
    value1 <- 1
    value2 <- 2

    result1 <- cflow::Ok(value1)
    result2 <- cflow::Ok(value2)

    testthat::expect_equal(result1$get(), value1)
    testthat::expect_equal(result2$get(), value2)

    testthat::expect_false(result1$is_err())
    testthat::expect_false(result2$is_err())

    testthat::expect_null(result1$error_type)
    testthat::expect_null(result2$error_type)
})

testthat::test_that("Result construction via Err() works.", {
    value1 <- 1
    value2 <- 2

    error_type2 <- "Some custom error type."

    result1 <- cflow::Err(value1)
    result2 <- cflow::Err(value2, error_type2)

    testthat::expect_equal(result1$get(), value1)
    testthat::expect_equal(result2$get(), value2)

    testthat::expect_true(result1$is_err())
    testthat::expect_true(result2$is_err())

    testthat::expect_null(result1$error_type)
    testthat::expect_equal(result2$error_type, error_type2)
})

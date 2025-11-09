#' Tests specifically for the [`is.nothing()`] function.
NULL

testthat::test_that("is.nothing() works.", {

    testthat::expect_true(is.nothing(NULL))
    testthat::expect_true(is.nothing(NA))
    testthat::expect_true(is.nothing(NaN))
    testthat::expect_true(is.nothing(character(0)))
    testthat::expect_true(is.nothing(logical(0)))
    testthat::expect_true(is.nothing(integer(0)))
    testthat::expect_true(is.nothing(numeric(0)))

    testthat::expect_false(is.nothing(c(NA, 1)))
    testthat::expect_false(is.nothing(c(NaN, 1)))

    testthat::expect_false(is.nothing(""))
    testthat::expect_false(is.nothing(0))
    testthat::expect_false(is.nothing(Inf))

})

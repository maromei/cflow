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

testthat::test_that("The == operator works.", {
    testthat::expect_true(Ok(1) == Ok(1))

    testthat::expect_false(Ok(1) == Ok(2))
    testthat::expect_false(Ok(2) == Ok(1))

    testthat::expect_false(Ok(1) == Err(1))
    testthat::expect_false(Err(1) == Ok(1))

    testthat::expect_true(Err(1) == Err(1))

    testthat::expect_false(Err(2) == Err(1))
    testthat::expect_false(Err(1) == Err(2))

    testthat::expect_false(Err(1, "some type") == Err(1))
    testthat::expect_false(Err(1) == Err(1, "some type"))

    zero_len_objects <- list(NULL, NA, NaN, character(0))
    for (obj1 in zero_len_objects) {
        for (obj2 in zero_len_objects) {
            str_repr <- paste("Compare", format(obj1), format(obj2))

            if (identical(obj1, obj2)) {
                testthat::expect_true(Ok(obj1) == Ok(obj2), str_repr)
                testthat::expect_true(Err(obj1) == Err(obj2), str_repr)
                next
            }

            testthat::expect_false(Ok(obj1) == Ok(obj2), str_repr)
            testthat::expect_false(Ok(obj2) == Ok(obj1), str_repr)

            testthat::expect_false(Err(obj1) == Err(obj2), str_repr)
            testthat::expect_false(Err(obj2) == Err(obj1), str_repr)
        }
    }

    testthat::expect_error(
        1 == Ok(1),
        regexp = "Cannot compare class",
        fixed = TRUE
    )
    testthat::expect_error(
        Ok(1) == 1,
        regexp = "Cannot compare class",
        fixed = TRUE
    )
    testthat::expect_error(
        Err(1) == 1,
        regexp = "Cannot compare class",
        fixed = TRUE
    )
    testthat::expect_error(
        1 == Err(1),
        regexp = "Cannot compare class",
        fixed = TRUE
    )
})

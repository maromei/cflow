#' Test the `==` and `!=` operator of [`Result`] objects.
NULL

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


testthat::test_that("The != operator works.", {
    testthat::expect_false(Ok(1) != Ok(1))

    testthat::expect_true(Ok(1) != Ok(2))
    testthat::expect_true(Ok(2) != Ok(1))

    testthat::expect_true(Ok(1) != Err(1))
    testthat::expect_true(Err(1) != Ok(1))

    testthat::expect_false(Err(1) != Err(1))

    testthat::expect_true(Err(2) != Err(1))
    testthat::expect_true(Err(1) != Err(2))

    testthat::expect_true(Err(1, "some type") != Err(1))
    testthat::expect_true(Err(1) != Err(1, "some type"))

    zero_len_objects <- list(NULL, NA, NaN, character(0))
    for (obj1 in zero_len_objects) {
        for (obj2 in zero_len_objects) {
            str_repr <- paste("Compare", format(obj1), format(obj2))

            if (identical(obj1, obj2)) {
                testthat::expect_false(Ok(obj1) != Ok(obj2), str_repr)
                testthat::expect_false(Err(obj1) != Err(obj2), str_repr)
                next
            }

            testthat::expect_true(Ok(obj1) != Ok(obj2), str_repr)
            testthat::expect_true(Ok(obj2) != Ok(obj1), str_repr)

            testthat::expect_true(Err(obj1) != Err(obj2), str_repr)
            testthat::expect_true(Err(obj2) != Err(obj1), str_repr)
        }
    }

    testthat::expect_error(
        1 != Ok(1),
        regexp = "Cannot compare class",
        fixed = TRUE
    )
    testthat::expect_error(
        Ok(1) != 1,
        regexp = "Cannot compare class",
        fixed = TRUE
    )
    testthat::expect_error(
        Err(1) != 1,
        regexp = "Cannot compare class",
        fixed = TRUE
    )
    testthat::expect_error(
        1 != Err(1),
        regexp = "Cannot compare class",
        fixed = TRUE
    )
})

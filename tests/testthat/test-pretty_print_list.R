#' Tests specifically for the [`pretty_print_list()`] function.
NULL

testthat::test_that("pretty_print_list() works.", {
    list_ <- list(a = "a", b = c(1, 2, 3))
    formatted_list <- cflow::pretty_print_list(list_)
    expected <- c(
        "a (character): a",
        "b (numeric): [\n    1\n    2\n    3\n]"
    )
    testthat::expect_equal(formatted_list, expected)
})

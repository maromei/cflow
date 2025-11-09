#' Test specifically for the [`get_class_value_line_str()`] function.
NULL

# fmt: skip
testthat::test_that(
    "get_class_value_line_str() has no trailing whitespace on NULL",
    {
        testthat::expect_equal(
            get_class_value_line_str(NULL, name = "Value"),
            "Value (NULL): NULL"
        )
    }
)

# fmt: skip
testthat::test_that(
    "get_class_value_line_str() has no whitespace at the start for NULL names.",
    {
        testthat::expect_equal(get_class_value_line_str(NULL), "(NULL): NULL")
        testthat::expect_equal(get_class_value_line_str(1L), "(integer): 1")
    }
)

# fmt: skip
testthat::test_that(
    "get_class_value_line_str() collapse classes with lenght() > 1",
    {
        testthat::expect_equal(
            get_class_value_line_str(Ok(1L)),
            "(Ok, Result, R6): [\n    Result: Ok\n    Value (integer): 1\n]"
        )
    }
)

testthat::test_that("get_class_value_line_str() recursively indents", {
    list_ <- list(a = "a", b = c(1, 2, 3))
    formatted_line <- get_class_value_line_str(list_, name = "Some List")
    expectation <- paste(
        "Some List (list): [",
        "    a (character): a",
        "    b (numeric): [",
        "        1",
        "        2",
        "        3",
        "    ]",
        "]",
        sep = "\n"
    )
    testthat::expect_equal(formatted_line, expectation)
})


# cflow

`cflow` aims to bring control flow constructs `Result` and
`Option` found in the Rust programming language to R.
This is coupled with tools to have stricter type checking on runtime
via `assert` like funtions, based on the
[`checkmate`](https://mllg.github.io/checkmate/) package.

## Installation

Currently the package is not available on CRAN.
It can be installed via GitHub.

```r
remotes::install_github("maromei/cflow@0.0.1")
```

## Example

### Results

`Results` come in 2 flavors: `Ok` and `Err`.
They are used to wrap a value, or signal an error, which can be passed to the
caller, without halting program execution.

Creation of these values can be easily done using the following functions:

```r
ok_value <- Ok(1)
error_value <- Err("Some Error Mesage", "Optional Error Type")
```

The pipe operator `%then%`, or the `Result$then()` method can also be used
to have functions act on a wrapped `Ok` value, or simply return the error.

```r
f <- function(x, y) c(x, y)

Ok(1)$then(f(2))
# Output: Ok(c(1, 2))
Ok(1) %then% f(2)
# Output: Ok(c(1, 2))

Err("error")$then(f(2))
# Output: Err("error")
Err("error") %then% f(2)
# Output: Err("error")
```

An example of using this to propagate errors for readings content form files:

```r
read_a_file <- function(file_path) {
    result <- tryCatch(
        readLines(file_path),
        error = function(e) Err(e, "file_error")
    )

    if (!is.Err(result)) {
        result <- Ok(result)
    }

    result
}

file_content <- read_a_file("path/to/file")
print(file_content)

# Result: Err
# Message (simpleError, error, condition): [
#     message (character): cannot open the connection
#     call (call): file(con, "r")
# ]
# Error Type (character): file_error

file_content <- read_a_file("NEWS.md")
print(file_content)

# Result: Ok
# Value (character): [
#     # cflow 0.0.1
#
#     * Initial CRAN submission.
# ]

# Here you can now do some operations, even if the function call
# failed. Check for the type with `is.Err()` or `is.Ok()`.

# You can still hault the program execution if you wish.
file_content <- file_content$get_or_stop()
```

## Feature Status

- `Result`
    - The object is implemented in its most basic form
- `Option`
    - open
- Assert interface
    - open

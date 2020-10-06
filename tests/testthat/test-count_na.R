## Create example data:
x <- c(1, 2, NA, 4, 5, NA)

## Tests on results:
test_that("count_na works well", {
    expect_equal(count_na(x), 2)
})
test_that("count_nonmissing works well", {
    expect_equal(count_nonmissing(x), 4)
})

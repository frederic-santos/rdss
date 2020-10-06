## Create example data:
dat <- matrix(c(1, NA, 1, 1, NA, 1,
                1, 1, 1, 1, 1, 1,
                NA, NA, 1, 1, 1, NA,
                1, 1, NA, 1, 1, 1),
              ncol = 4)
res_ind3 <- dat[-2, , drop = FALSE]
res_ind08 <- dat[4, , drop = FALSE]
res_var4 <- matrix(c(1, NA, 1, 1, NA, 1,
                     1, 1, 1, 1, 1, 1,
                     1, 1, NA, 1, 1, 1),
                   ncol = 3)
res_var56 <- matrix(c(1, 1, 1, 1, 1, 1,
                     1, 1, NA, 1, 1, 1),
                   ncol = 2)

## Tests on arguments:
test_that("Error messages are triggered", {
    expect_error(remove_na(dat, which = "var"),
                 "Please provide a value for prop_min or n_min.")
    expect_error(remove_na(dat, which = "var", prop_min = 3),
                 "Incorrect value for prop_min.")
    expect_error(remove_na(dat, which = "var", prop_min = -1),
                 "Incorrect value for prop_min.")
    expect_error(remove_na(dat, which = "var", n_min = -1),
                 "Incorrect value for n_min.")
    expect_error(remove_na(dat, which = "var", n_min = 500),
                 "Incorrect value for n_min.")
    expect_message(remove_na(dat, which = "var", n_min = 5,
                             prop_min = 0.5),
                 "n_min has been used instead of prop_min.")
})

## Tests on results:
test_that("Results are ok for individuals", {
    expect_equal(remove_na(dat, which = "ind", n_min = 3),
                 res_ind3)
    expect_equal(remove_na(dat, which = "ind", prop_min = 0.8),
                 res_ind08)
})
test_that("Results are ok for variables", {
    expect_equal(remove_na(dat, which = "var", prop_min = 5/6),
                 res_var56)
    expect_equal(remove_na(dat, which = "var", n_min = 4),
                 res_var4)
})

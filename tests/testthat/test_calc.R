test_that("calc_max_age() applies the Hamel-Cope formula", {
  expect_equal(calc_max_age(c(0.2, 0.4)), c(27, 13.5))
})

test_that("calc_max_age() preserves names", {
  mortality <- c(female = 0.2, male = 0.4)

  expect_named(calc_max_age(mortality), names(mortality))
})

test_that("calc_max_age() rejects non-numeric input", {
  expect_error(calc_max_age("0.2"), "must be numeric")
})

test_that("calc_max_age() rejects array input", {
  expect_error(calc_max_age(matrix(0.2)), "must be a vector")
})

test_that("calc_max_age() rejects empty input", {
  expect_error(calc_max_age(numeric()), "must not be empty")
})

test_that("calc_max_age() rejects non-finite input", {
  expect_error(calc_max_age(c(0.2, NA_real_)), "finite values")
  expect_error(calc_max_age(Inf), "finite values")
})

test_that("calc_max_age() rejects non-positive input", {
  expect_error(calc_max_age(0), "greater than 0")
  expect_error(calc_max_age(-0.2), "greater than 0")
})
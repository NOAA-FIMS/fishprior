test_that("make_informative_mortality_prior() errors when given a vector", {
  expect_error(
    make_informative_mortality_prior(c(log(20), log(25))),
    "must be a single value"
  )
})

test_that("make_informative_mortality_prior() errors for non-numeric input", {
  expect_error(
    make_informative_mortality_prior("twenty"),
    "must be numeric"
  )
})

test_that("make_informative_mortality_prior() errors for negative log age", {
  # log(0.5) < 0 — maximum age below 1 year is implausible
  expect_error(
    make_informative_mortality_prior(-0.5),
    "must be greater than 0"
  )
})

test_that("make_informative_mortality_prior() returns a lognormal prior", {
  p <- make_informative_mortality_prior(log(25))

  expect_s4_class(p, "prior")
  expect_equal(get_distribution(p), "lognormal")
  expect_equal(get_type(p), "informative")
})

test_that("make_informative_mortality_prior() uses Hamel-Cope formula", {
  max_age <- 25
  p <- make_informative_mortality_prior(log(max_age))
  params <- get_parameters(p)

  expected_mean <- log(5.40 / max_age) # = log(median M)
  expected_sd <- 0.31

  expect_equal(params[["mean_log"]], expected_mean, tolerance = 1e-10)
  expect_equal(params[["sd_log"]], expected_sd, tolerance = 1e-10)
})

test_that("make_informative_mortality_prior() scales correctly with max age", {
  # Older fish → lower M
  p_young <- make_informative_mortality_prior(log(10))
  p_old <- make_informative_mortality_prior(log(50))

  expect_gt(
    get_parameters(p_young)[["mean_log"]],
    get_parameters(p_old)[["mean_log"]]
  )
})

test_that("make_diffuse_mortality_prior() errors when given a vector", {
  expect_error(
    make_diffuse_mortality_prior(c(-1.5, -1.2), 0.3),
    "must be a single value"
  )
})

test_that("make_diffuse_mortality_prior() errors for non-numeric input", {
  expect_error(
    make_diffuse_mortality_prior("low", 0.3),
    "must be numeric"
  )
})

test_that("make_diffuse_mortality_prior() returns a lognormal diffuse prior", {
  p <- make_diffuse_mortality_prior(-1.5, 0.3)

  expect_s4_class(p, "prior")
  expect_equal(get_distribution(p), "lognormal")
  expect_equal(get_type(p), "diffuse")
})

test_that("make_diffuse_mortality_prior() stores supplied parameters", {
  p <- make_diffuse_mortality_prior(-1.5, 0.3)
  params <- get_parameters(p)

  expect_equal(params[["mean_log"]], -1.5)
  expect_equal(params[["sd_log"]], 0.3)
})

test_that("make_diffuse_mortality_prior() sets trait to natural mortality", {
  p <- make_diffuse_mortality_prior(-1.5, 0.3)
  expect_equal(get_trait(p), "natural mortality")
})

test_that("make_mortality_prior() accepts 'diffuse' type without error", {
  # No error means arg_match passed
  expect_no_error(
    make_mortality_prior(data = NULL, type = "diffuse")
  )
})

test_that("make_mortality_prior() accepts 'informative' type without error", {
  expect_no_error(
    make_mortality_prior(data = NULL, type = "informative")
  )
})

test_that("make_mortality_prior() defaults to 'diffuse'", {
  # arg_match should not throw when called with the default
  expect_no_error(make_mortality_prior(data = NULL))
})

test_that("make_mortality_prior() errors on an invalid type", {
  expect_error(
    make_mortality_prior(data = NULL, type = "uniform"),
    class = "rlang_error"
  )
})

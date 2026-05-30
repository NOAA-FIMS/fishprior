# minimal data frame that matches what make_growth_coefficient_prior expects.
# Note: the function reads data[["name"]] (not "Species") and filters on
# trait == "log(growth_coefficient)", which is the translated name produced by
# translate_trait_names("K").
make_growth_data <- function(name = "Gadus morhua",
                              mean_normal = 0.20,
                              sd_normal   = 0.05) {
  data.frame(
    name        = name,
    trait       = "log(growth_coefficient)",
    mean_normal = mean_normal,
    sd_normal   = sd_normal,
    mean        = log(mean_normal),
    sd          = 0.10,
    stringsAsFactors = FALSE
  )
}

test_that("make_growth_coefficient_prior() errors when data has more than one group", {
  data <- rbind(
    make_growth_data("Gadus morhua"),
    make_growth_data("Anoplopoma fimbria")
  )
  expect_error(
    make_growth_coefficient_prior(data),
    "works with just one group"
  )
})

test_that("make_growth_coefficient_prior() errors for diffuse type", {
  expect_error(
    make_growth_coefficient_prior(make_growth_data(), type = "diffuse"),
    "No.*prior available for the growth coefficient"
  )
})

test_that("make_growth_coefficient_prior() error message suggests informative", {
  expect_error(
    make_growth_coefficient_prior(make_growth_data(), type = "diffuse"),
    "informative"
  )
})

test_that("make_growth_coefficient_prior() errors on an invalid type string", {
  expect_error(
    make_growth_coefficient_prior(make_growth_data(), type = "bayes"),
    class = "rlang_error"
  )
})

test_that("make_growth_coefficient_prior() returns a prior S4 object", {
  p <- make_growth_coefficient_prior(make_growth_data())
  expect_s4_class(p, "prior")
})

test_that("make_growth_coefficient_prior() returns a normal distribution prior", {
  p <- make_growth_coefficient_prior(make_growth_data())
  expect_equal(get_distribution(p), "normal")
})

test_that("make_growth_coefficient_prior() sets type to informative", {
  p <- make_growth_coefficient_prior(make_growth_data())
  expect_equal(get_type(p), "informative")
})

test_that("make_growth_coefficient_prior() sets trait slot", {
  p <- make_growth_coefficient_prior(make_growth_data())
  expect_equal(get_trait(p), "growth_coefficient")
})

test_that("make_growth_coefficient_prior() sets group from data name column", {
  p <- make_growth_coefficient_prior(make_growth_data("Anoplopoma fimbria"))
  expect_equal(get_group(p), "Anoplopoma fimbria")
})

test_that("make_growth_coefficient_prior() stores mean and sd from data", {
  data   <- make_growth_data(mean_normal = 0.20, sd_normal = 0.05)
  p      <- make_growth_coefficient_prior(data)
  params <- get_parameters(p)

  expect_equal(params[["mean"]], 0.20)
  expect_equal(params[["sd"]],   0.05)
})

test_that("make_growth_coefficient_prior() stores the filtered data in the prior", {
  data <- make_growth_data()
  p    <- make_growth_coefficient_prior(data)

  expect_s3_class(get_data(p), "data.frame")
  expect_true(nrow(get_data(p)) >= 1)
  expect_true(all(get_data(p)[["trait"]] == "log(growth_coefficient)"))
})

test_that("make_growth_coefficient_prior() default type is informative", {
  # Should not error and should return an informative prior
  p <- make_growth_coefficient_prior(make_growth_data())
  expect_equal(get_type(p), "informative")
})

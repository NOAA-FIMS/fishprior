make_test_prior <- function(
  distribution = "lognormal",
  trait = "natural_mortality",
  type = "informative",
  group = "Gadus morhua"
) {
  methods::new(
    "prior",
    distribution = distribution,
    parameters   = list(mean = -1.5, sd = 0.31),
    trait        = trait,
    type         = type,
    group        = group,
    data         = data.frame()
  )
}

test_that("prior objects can be created with new()", {
  expect_no_error(make_test_prior())
  expect_s4_class(make_test_prior(), "prior")
})

test_that("show() prints without error", {
  p <- make_test_prior()
  expect_no_error(show(p))
})

test_that("print() prints without error", {
  p <- make_test_prior()
  expect_no_error(print(p))
})

test_that("get_distribution() returns the distribution slot", {
  expect_equal(get_distribution(make_test_prior("lognormal")), "lognormal")
  expect_equal(get_distribution(make_test_prior("normal")), "normal")
})

test_that("get_parameters() returns the parameters list", {
  params <- get_parameters(make_test_prior())
  expect_type(params, "list")
  expect_named(params, c("mean", "sd"))
  expect_equal(params[["mean"]], -1.5)
  expect_equal(params[["sd"]], 0.31)
})

test_that("get_trait() returns the trait slot", {
  p <- make_test_prior(trait = "natural_mortality")
  expect_equal(get_trait(p), "natural_mortality")
})

test_that("get_group() returns the group slot", {
  p <- make_test_prior(group = "Gadus morhua")
  expect_equal(get_group(p), "Gadus morhua")
})

test_that("get_type() returns the type slot", {
  expect_equal(get_type(make_test_prior(type = "informative")), "informative")
  expect_equal(get_type(make_test_prior(type = "diffuse")), "diffuse")
})

test_that("get_data() returns a data.frame", {
  expect_s3_class(get_data(make_test_prior()), "data.frame")
})

test_that("evaluate() returns a tibble with the correct shape for normal", {
  p <- make_test_prior(
    distribution = "normal", group = "TestGroup",
    trait = "growth_coefficient"
  )
  result <- evaluate(p, n = 200)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 200)
  expect_named(result, c("group", "trait", "value"))
  expect_equal(unique(result[["group"]]), "TestGroup")
  expect_equal(unique(result[["trait"]]), "growth_coefficient")
  expect_type(result[["value"]], "double")
})

test_that("evaluate() returns positive values for lognormal", {
  p <- make_test_prior(distribution = "lognormal")
  result <- evaluate(p, n = 500)

  expect_equal(nrow(result), 500)
  expect_true(all(result[["value"]] > 0))
})

test_that("evaluate() defaults to n = 1000", {
  p <- make_test_prior()
  expect_equal(nrow(evaluate(p)), 1000)
})

test_that("evaluate() errors informatively for an unsupported distribution", {
  p <- methods::new(
    "prior",
    distribution = "beta",
    parameters   = list(mean = 0.5, sd = 0.1),
    trait        = "x",
    type         = "diffuse",
    group        = "x",
    data         = data.frame()
  )
  expect_error(evaluate(p), "Unsupported distribution")
})

test_that("plot() returns a ggplot object without error", {
  p <- make_test_prior(distribution = "lognormal", trait = "natural_mortality")
  result <- plot(p)
  expect_s3_class(result, "ggplot")
})

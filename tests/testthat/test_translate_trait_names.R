test_that("translate_trait_names maps all known FishBase trait names", {
  expect_equal(translate_trait_names("tmax"), "log(age_max)")
  expect_equal(translate_trait_names("FecundityMean"), "log(fecundity)")
  expect_equal(translate_trait_names("K"), "log(growth_coefficient)")
  expect_equal(translate_trait_names("Lmax"), "log(length_max)")
  expect_equal(translate_trait_names("Loo"), "log(length_infinity)")
  expect_equal(translate_trait_names("Lm"), "log(length_maturity)")
  expect_equal(translate_trait_names("tm"), "log(age_maturity)")
  expect_equal(translate_trait_names("M"), "log(natural_mortality)")
})

test_that("translate_trait_names passes through unknown trait names unchanged", {
  expect_equal(translate_trait_names("UnknownTrait"), "UnknownTrait")
  expect_equal(translate_trait_names("SomeOther"), "SomeOther")
})

test_that("translate_trait_names handles a mixed character vector", {
  result <- translate_trait_names(c("Loo", "K", "M", "UnknownTrait"))
  expect_equal(result, c(
    "log(length_infinity)",
    "log(growth_coefficient)",
    "log(natural_mortality)",
    "UnknownTrait"
  ))
})

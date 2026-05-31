# check_rfishbase_version() has four distinct paths:
#   1. rfishbase not installed
#   2. CRAN unavailable (tryCatch catches error)
#   3. installed version != CRAN version
#   4. installed version == CRAN version
#
# pkg_is_installed() and available.packages() are both imported into the
# fishprior namespace, so local_mocked_bindings() can replace them without
# touching any locked base or utils namespace.

# Helper: build a one-row matrix that available.packages() would return
cran_matrix <- function(version) {
  matrix(
    version,
    nrow = 1,
    ncol = 1,
    dimnames = list("rfishbase", "Version")
  )
}

test_that("check_rfishbase_version() messages and returns FALSE when rfishbase absent", {
  local_mocked_bindings(pkg_is_installed = function(...) FALSE)
  
  expect_message(
    result <- check_rfishbase_version(),
    "not installed"
  )
  expect_false(result)
})

test_that("check_rfishbase_version() messages and returns FALSE when CRAN unavailable", {
  local_mocked_bindings(
    available.packages = function(...) stop("no internet connection")
  )
  
  expect_message(
    result <- check_rfishbase_version(),
    "Could not check CRAN version"
  )
  expect_false(result)
})

test_that("check_rfishbase_version() messages and returns TRUE when versions differ", {
  local_mocked_bindings(
    available.packages = function(...) cran_matrix("99.99.0")
  )
  
  expect_message(
    result <- check_rfishbase_version(),
    "out of date"
  )
  expect_true(result)
})

test_that("check_rfishbase_version() message contains installed and CRAN versions", {
  local_mocked_bindings(
    available.packages = function(...) cran_matrix("99.99.0")
  )
  
  expect_message(check_rfishbase_version(), "Installed:")
  expect_message(check_rfishbase_version(), "CRAN:")
})

test_that("check_rfishbase_version() returns TRUE silently when versions match", {
  installed <- as.character(utils::packageVersion("rfishbase"))
  local_mocked_bindings(
    available.packages = function(...) cran_matrix(installed)
  )
  
  expect_no_message(result <- check_rfishbase_version())
  expect_true(result)
})
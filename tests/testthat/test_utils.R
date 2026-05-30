# check_rfishbase_version() has four distinct paths:
#   1. rfishbase not installed                     → message + invisible(FALSE)
#   2. CRAN unavailable (tryCatch catches error)   → message + invisible(FALSE)
#   3. installed version != CRAN version           → message + invisible(TRUE)
#   4. installed version == CRAN version           → silent  + invisible(TRUE)
#
# We use mockery::stub() to intercept requireNamespace() and
# available.packages() inside the fishprior namespace so no network calls are made.

# Helper: build a one-row matrix that available.packages() would return
cran_matrix <- function(version) {
  matrix(
    version,
    nrow = 1,
    ncol = 1,
    dimnames = list("rfishbase", "Version")
  )
}

# ── Branch 1: rfishbase not installed ─────────────────────────────────────────

test_that("check_rfishbase_version() messages and returns FALSE when rfishbase absent", {
  mockery::stub(check_rfishbase_version, "requireNamespace", FALSE)

  expect_message(
    result <- check_rfishbase_version(),
    "not installed"
  )
  expect_false(result)
})

# ── Branch 2: CRAN unreachable ────────────────────────────────────────────────

test_that("check_rfishbase_version() messages and returns FALSE when CRAN unavailable", {
  mockery::stub(
    check_rfishbase_version, "available.packages",
    function(...) stop("no internet connection")
  )

  expect_message(
    result <- check_rfishbase_version(),
    "Could not check CRAN version"
  )
  expect_false(result)
})

# ── Branch 3: versions differ ─────────────────────────────────────────────────

test_that("check_rfishbase_version() messages and returns TRUE when versions differ", {
  mockery::stub(
    check_rfishbase_version, "available.packages",
    function(...) cran_matrix("99.99.0")
  )

  expect_message(
    result <- check_rfishbase_version(),
    "out of date"
  )
  expect_true(result)
})

test_that("check_rfishbase_version() message contains installed and CRAN versions", {
  mockery::stub(
    check_rfishbase_version, "available.packages",
    function(...) cran_matrix("99.99.0")
  )

  expect_message(check_rfishbase_version(), "Installed:")
  expect_message(check_rfishbase_version(), "CRAN:")
})

# ── Branch 4: versions match ──────────────────────────────────────────────────

test_that("check_rfishbase_version() returns TRUE silently when versions match", {
  installed <- as.character(utils::packageVersion("rfishbase"))
  mockery::stub(
    check_rfishbase_version, "available.packages",
    function(...) cran_matrix(installed)
  )

  expect_no_message(result <- check_rfishbase_version())
  expect_true(result)
})

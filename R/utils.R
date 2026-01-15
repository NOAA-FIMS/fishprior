#' Check if the Installed Version of `rfishbase` is Up to Date
#'
#' This function compares the installed version of the \code{rfishbase} package
#' with the latest version available on CRAN. It prints a message if the
#' installed version is outdated.
#'
#' @importFrom utils available.packages packageVersion
#' @examples
#' check_rfishbase_version()
#'
#' @export
check_rfishbase_version <- function() {
  pkg <- "rfishbase"

  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("rfishbase is not installed.")
    return(invisible(FALSE))
  }

  installed_version <- as.character(packageVersion(pkg))

  cran_version <- tryCatch(
    {
      available <- available.packages()
      available[pkg, "Version"]
    },
    error = function(e) {
      message("Could not check CRAN version")
      return(NA)
    }
  )

  if (is.na(cran_version)) {
    return(invisible(FALSE))
  }

  if (installed_version != cran_version) {
    message(sprintf(
      "rfishbase is out of date.\n  Installed: %s\n  CRAN: %s",
      installed_version, cran_version
    ))
  }

  invisible(TRUE)
}

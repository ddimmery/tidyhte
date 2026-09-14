# Skip helpers for optional (Suggests) dependencies.
#
# Most of the test-suite exercises SuperLearner ensembles; those tests are
# skipped when the optional packages are missing so that the package can be
# checked with only its hard dependencies installed.
skip_if_missing_suggests <- function(...) {
  for (pkg in c(...)) {
    testthat::skip_if_not_installed(pkg)
  }
  invisible(TRUE)
}

skip_if_no_superlearner <- function() skip_if_missing_suggests("SuperLearner")
skip_if_no_vimp <- function() skip_if_missing_suggests("SuperLearner", "vimp", "quadprog")

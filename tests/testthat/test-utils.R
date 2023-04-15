test_that("muffle_messages works", {
    f1 <- function() {
        message("test message 1")
    }
    f2 <- function() {
    }
    f3 <- function() {
        message("test message 2")
    }
    expect_message(muffle_messages(f1(), "test"), NA)
    expect_message(muffle_messages(f2(), "test"), NA)
    expect_message(muffle_messages(f2(), ""), NA)
    expect_message(muffle_messages(f3(), "test message 1"), "test message 2")
})

test_that("muffle_warnings works", {
    f1 <- function() {
        warning("test warning 1")
    }
    f2 <- function() {
    }
    f3 <- function() {
        warning("test warning 2")
    }
    expect_warning(muffle_warnings(f1(), "test"), NA)
    expect_warning(muffle_warnings(f2(), "test"), NA)
    expect_warning(muffle_warnings(f2(), ""), NA)
    expect_warning(muffle_warnings(f3(), "test warning 1"), "test warning 2")
})

test_that("zero_range works", {
    expect_true(zero_range(1))
    expect_true(zero_range(.Machine$double.eps, 2 * .Machine$double.eps))
})

test_that("soft_require works", {
    expect_error(
        soft_require("thispackagedoesntexist"),
        class = "rlib_error_package_not_found"
    )

    expect_error(soft_require("SuperLearner"), NA)
})

test_that("cluster robust SEs are correct", {
    skip_if_not_installed("estimatr")
    skip_on_cran()

    y <- rnorm(100)
    cl <- sample(1:4, size = 100, replace = TRUE)
    lmr <- estimatr::lm_robust(y ~ 1, clusters = cl, se_type = "stata")

    expect_error(result <- clustered_se_of_mean(y, cl), NA)
    expect_equal(result, lmr$std.error, ignore_attr = TRUE)

    cl <- sample(1:10, size = 125, replace = TRUE)
    y <- cl + rnorm(125)
    lmr <- estimatr::lm_robust(y ~ 1, clusters = cl, se_type = "stata")

    expect_error(result <- clustered_se_of_mean(y, cl), NA)
    expect_equal(result, lmr$std.error, ignore_attr = TRUE)

    cl <- sample(1:10, size = 125, replace = TRUE)
    y <- cl + rnorm(125)
    w <- 1 / 5 + rexp(125)
    lmr <- estimatr::lm_robust(y ~ 1, clusters = cl, se_type = "stata", weights = w)

    expect_error(result <- clustered_se_of_mean(y, cl, weights = w), NA)
    expect_equal(result, lmr$std.error, ignore_attr = TRUE)
})

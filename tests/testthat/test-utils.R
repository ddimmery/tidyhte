test_that("soft_require works", {
    expect_error(
        soft_require("this_package_doesnt_exist"),
        "loading required package (this_package_doesnt_exist) failed",
        fixed = TRUE
    )

    expect_error(soft_require("SuperLearner"), NA)
})

test_that("cluster robust SEs are correct", {
    skip_if_not_installed("estimatr")
    skip_on_cran()

    y <- rnorm(100)
    cl <- sample(1:4, size = 100, replace = TRUE)
    lmr <- estimatr::lm_robust(y~1, clusters = cl, se_type = "stata")

    expect_error(result <- clustered_se_of_mean(y, cl), NA)
    expect_equal(result, lmr$std.error, ignore_attr = TRUE)

    cl <- sample(1:10, size = 125, replace = TRUE)
    y <- cl + rnorm(125)
    lmr <- estimatr::lm_robust(y~1, clusters = cl, se_type = "stata")

    expect_error(result <- clustered_se_of_mean(y, cl), NA)
    expect_equal(result, lmr$std.error, ignore_attr = TRUE)
})

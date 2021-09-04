test_that("soft_require works", {
    expect_error(
        soft_require("this_package_doesnt_exist"),
        "loading required package (this_package_doesnt_exist) failed",
        fixed = TRUE
    )

    expect_error(soft_require("SuperLearner"), NA)
})

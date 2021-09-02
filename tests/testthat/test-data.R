test_that("listwise_deletion", {
    df <- dplyr::tibble(
        a = 1:100,
        b = rnorm(100),
        c = c(NA, NA, rnorm(98)),
        d = c(rnorm(98), NA, NA)
    )
    expect_error(df1 <- listwise_deletion(df), NA)
    expect_true(nrow(df) == nrow(df1))
    expect_message(
        listwise_deletion(df, c),
        "Dropped 2 of 100 rows (2%) through listwise deletion.",
        fixed = TRUE
    )
    expect_message(
        listwise_deletion(df, c, d),
        "Dropped 4 of 100 rows (4%) through listwise deletion.",
        fixed = TRUE
    )
})

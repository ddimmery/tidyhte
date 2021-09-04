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

test_that("check_identifier", {
    df <- dplyr::tibble(
        uid1 = 1:10,
        uid2 = c(1:9, NA),
        uid3 = rnorm(10),
        uid4 = paste0("uid", 1:10)
    )
    msg <- "Invalid identifier. Each unit / cluster must have its own unique ID."
    expect_error(check_identifier(df, "uid1"), NA)
    expect_error(check_identifier(df, "uid2"), msg)
    expect_error(check_identifier(df, "uid3"), msg)
    expect_error(check_identifier(df, "uid4"), NA)
    expect_error(check_identifier(df, "uid5"), msg)
})
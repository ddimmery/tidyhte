set.seed(20051920) # 20051920 is derived from 'test'

n <- 100
d <- dplyr::tibble(
    uid = 1:n,
    cov1 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov2 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE)
)

test_that("make_splits output", {
        checkmate::expect_data_frame(
            make_splits(d, uid, .num_splits = 2),
            any.missing = FALSE,
            nrows = 100,
            ncols = 4,
            col.names = "named"
        )
        expect_true(".split_id" %in% names(make_splits(d, uid, .num_splits = 2)))
})

s_df <- make_splits(d, uid, .num_splits = 2)
s_id <- s_df$.split_id
test_that("2 splits are ok", {
    expect_length(unique(s_id), 2)
    checkmate::expect_integerish(s_id, len = n, any.missing = FALSE, lower = 1, upper = 2)
    expect_true(all(table(s_id) == (n / 2)))
})

test_that("resplitting works", {
    expect_error(s_df2 <- make_splits(s_df, uid, .num_splits = 4), NA)
    checkmate::expect_data_frame(s_df2, min.rows = n, max.rows = n)
    expect_true(".split_id" %in% names(s_df2))
})

test_that("check_splits", {
    expect_error(check_splits(d), "You must first construct splits with `tidyhte::make_splits`.")
})

test_that("check that splits can be used to create dataframes", {
    checkmate::expect_r6(split_data(s_df, 1), classes = "HTEFold")
    expect_error(split_data(d, 1), "Must construct split identifiers before splitting.")
})

test_that("train split is larger than holdout", {
    s_df <- make_splits(d, uid, .num_splits = 4)
    fold <- split_data(s_df, 1)
    checkmate::expect_r6(fold, classes = "HTEFold")
    expect_true(nrow(fold$train) == (3 * nrow(fold$holdout)))
    expect_true(nrow(fold$holdout) == sum(fold$in_holdout))
})

s_id <- make_splits(d, uid, .num_splits = 7)$.split_id
test_that("check that 7 splits are ok", {
    expect_length(unique(s_id), 7)
    checkmate::expect_integerish(s_id, len = n, any.missing = FALSE, lower = 1, upper = 10)
    checkmate::expect_integer(table(s_id), lower = floor(n / 7), upper = ceiling(n / 7))
})

n <- 100
d <- dplyr::tibble(
    uid = 1:n,
    cov1 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov2 = sample(rep(1:2, c(n / 20, 19 * n / 20)), n, replace = TRUE)
)

s_df <- make_splits(d, uid, cov2, .num_splits = 2)
test_that("check that stratification on one variable works", {
    s_df %>%
    dplyr::group_by(cov2) %>%
    dplyr::summarize(all_splits = all(1:2 %in% .split_id)) %>%
    dplyr::summarize(all_splits = all(all_splits)) %>%
    unlist() -> result
    expect_true(result)
})

s_df <- make_splits(d, uid, cov1, cov2, .num_splits = 2)
test_that("check that stratification on multiple variables works", {
    s_df %>%
    dplyr::group_by(cov2) %>%
    dplyr::summarize(all_splits = all(1:2 %in% .split_id)) %>%
    dplyr::summarize(all_splits = all(all_splits)) %>%
    unlist() -> result
    expect_true(result)

    s_df %>%
    dplyr::group_by(cov1) %>%
    dplyr::summarize(all_splits = all(1:2 %in% .split_id)) %>%
    dplyr::summarize(all_splits = all(all_splits)) %>%
    unlist() -> result
    expect_true(result)
})


d <- dplyr::tibble(
    uid = paste0("uid is ", as.character(1:n)),
    cov1 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov2 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE)
)

test_that("character uids", {
        checkmate::expect_data_frame(
            make_splits(d, uid, .num_splits = 2),
            any.missing = FALSE,
            nrows = 100,
            ncols = 4,
            col.names = "named"
        )
        expect_true(".split_id" %in% names(make_splits(d, uid, .num_splits = 2)))
})


d <- dplyr::tibble(
    uid = paste0("uid is ", as.character(1:n)),
    cov1 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov2 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov3 = rnorm(n),
    cov4 = runif(n)
)

test_that("lots of covariates informing the strata", {
        newd <- make_splits(d, uid, cov1, cov2, cov3, cov4, .num_splits = 2)
        checkmate::expect_data_frame(
            newd,
            any.missing = FALSE,
            nrows = 100,
            ncols = 6,
            col.names = "named"
        )
        expect_true(".split_id" %in% names(newd))
})

d <- dplyr::tibble(
    uid = rep(paste0("uid is ", as.character(1:(n / 2))), 2),
    cov1 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov2 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov3 = rnorm(n),
    cov4 = runif(n)
)

test_that("clustered data", {
        newd <- make_splits(d, uid, cov1, cov2, cov3, cov4, .num_splits = 2)
        checkmate::expect_data_frame(
            newd,
            any.missing = FALSE,
            nrows = 100,
            ncols = 6,
            col.names = "named"
        )
        expect_true(".split_id" %in% names(newd))
})

test_that("splitting works when quickblock isn't installed", {
    expect_message(
        mockr::with_mock(
            package_present = function(x) FALSE,
            {
                make_splits(d, uid, cov1, .num_splits = 4)
            }
        ),
        "`quickblock` is not installed, so falling back to un-stratified CV."
    )
})

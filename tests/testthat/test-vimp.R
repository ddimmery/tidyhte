n <- 500
d <- dplyr::tibble(
    uid = rep(paste0("uid is ", as.character(1:(n / 2))), 2),
    cov1 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov2 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov3 = rnorm(n),
    cov4 = runif(n),
    w = rep(1, n)
) %>% dplyr::mutate(y = 2 * cov1 + cov3 + rnorm(n))

test_that("splitting ensures even number of splits with VIMP", {
    attr(d, "HTE_cfg") <- HTE_cfg$new(qoi = QoI_cfg$new(vimp = VIMP_cfg$new()))
    expect_message(
        make_splits(d, uid, .num_splits = 5),
        "`num_splits` must be even if VIMP is requested as a QoI. Rounding up."
    )
    expect_message(make_splits(d, cov1, .num_splits = 4), NA)
})

test_that("VIMP ensures even number of splits", {
    d <- make_splits(d, uid, .num_splits = 5)
    attr(d, "HTE_cfg") <- HTE_cfg$new(qoi = QoI_cfg$new(vimp = VIMP_cfg$new()))
    expect_error(
        calculate_vimp(
            d, w, y, cov1, cov2, .VIMP_cfg = attr(d, "HTE_cfg")$qoi$vimp,
            .Model_cfg = SLEnsemble_cfg$new()
        ),
        "Number of splits must be even to calculate VIMP."
    )
    attr(d, "num_splits") <- 4
    expect_error(
        calculate_vimp(
            d, w, y, cov1, cov2, .VIMP_cfg = attr(d, "HTE_cfg")$qoi$vimp,
            .Model_cfg = SLEnsemble_cfg$new()
        ),
        "Number of splits is inconsistent."
    )
})

test_that("linear VIMP works with weights", {
    d <- make_splits(d, uid, .num_splits = 4)
    d$w <- rexp(n, 1 / 0.9) + 0.1
    attr(d, "HTE_cfg") <- HTE_cfg$new(qoi = QoI_cfg$new(vimp = VIMP_cfg$new()))
    expect_error(result <- calculate_linear_vimp(
        d, w, y, cov1, cov2, cov3,
        .VIMP_cfg = attr(d, "HTE_cfg")$qoi$vimp, .Model_cfg = SLEnsemble_cfg$new()
    ), NA)

    expect_gt(result$estimate[1], result$estimate[2])
    expect_gt(result$estimate[3], result$estimate[2])

    expect_gt(result$estimate[1] / result$std_error[1], 2)
    expect_gt(result$estimate[3] / result$std_error[3], 2)
    expect_lt(result$estimate[2] / result$std_error[2], 2)
})

test_that("VIMP works with weights", {
    d <- make_splits(d, uid, .num_splits = 4)
    d$w <- rexp(n, 1 / 0.9) + 0.1
    attr(d, "HTE_cfg") <- HTE_cfg$new(qoi = QoI_cfg$new(vimp = VIMP_cfg$new()))
    expect_error(result <- calculate_vimp(
        d, w, y, cov1, cov2, cov3,
        .VIMP_cfg = attr(d, "HTE_cfg")$qoi$vimp, .Model_cfg = SLEnsemble_cfg$new()
    ), NA)

    expect_gt(result$estimate[1], result$estimate[2])
    expect_gt(result$estimate[3], result$estimate[2])

    expect_gt(result$estimate[1] / result$std_error[1], 2)
    expect_gt(result$estimate[3] / result$std_error[3], 2)
    expect_lt(result$estimate[2] / result$std_error[2], 2)
})

test_that("VIMP works with weights without sample splitting", {
    d <- make_splits(d, uid, .num_splits = 4)
    d$w <- rexp(n, 1 / 0.9) + 0.1
    attr(d, "HTE_cfg") <- HTE_cfg$new(
        qoi = QoI_cfg$new(vimp = VIMP_cfg$new(sample_splitting = FALSE))
    )
    expect_error(result <- calculate_vimp(
        d, w, y, cov1, cov2, cov3,
        .VIMP_cfg = attr(d, "HTE_cfg")$qoi$vimp, .Model_cfg = SLEnsemble_cfg$new()
    ), NA)

    expect_gt(result$estimate[1], result$estimate[2])
    expect_gt(result$estimate[3], result$estimate[2])

    expect_gt(result$estimate[1] / result$std_error[1], 2)
    expect_gt(result$estimate[3] / result$std_error[3], 2)
    expect_lt(result$estimate[2] / result$std_error[2], 2)
})

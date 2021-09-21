n <- 100
d <- dplyr::tibble(
    uid = rep(paste0("uid is ", as.character(1:(n / 2))), 2),
    cov1 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov2 = sample(rep(1:2, c(n / 2, n / 2)), n, replace = TRUE),
    cov3 = rnorm(n),
    cov4 = runif(n),
    y = rnorm(n)
)

test_that("splitting ensures even number of splits with VIMP", {
    attr(d, "HTE_cfg") <- HTE_cfg$new(qoi = QoI_cfg$new(vimp = VIMP_cfg$new(model_cfg = SLEnsemble_cfg$new())))
    expect_message(
        make_splits(d, uid, .num_splits = 5),
        "`num_splits` must be even if VIMP is requested as a QoI. Rounding up."
    )
    expect_message(make_splits(d, cov1, .num_splits = 4), NA)
})

test_that("VIMP ensures even number of splits", {
    d <- make_splits(d, uid, .num_splits = 5)
    attr(d, "HTE_cfg") <- HTE_cfg$new(qoi = QoI_cfg$new(vimp = VIMP_cfg$new(model_cfg = SLEnsemble_cfg$new())))
    expect_error(
        calculate_vimp(d, y, cov1, cov2, .VIMP_cfg = attr(d, "HTE_cfg")$qoi$vimp),
        "Number of splits must be even to calculate VIMP."
    )
    attr(d, "num_splits") <- 4
    expect_error(
        calculate_vimp(d, y, cov1, cov2, .VIMP_cfg = attr(d, "HTE_cfg")$qoi$vimp),
        "Number of splits is inconsistent."
    )
})
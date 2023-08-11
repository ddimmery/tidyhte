set.seed(20051920) # 20051920 is derived from 'test'

n <- 250
data <- dplyr::tibble(
    uid = 1:n
) %>%
    dplyr::mutate(
        a = rbinom(n, 1, 0.5),
        ps = rep(0.5, n),
        x1 = rnorm(n),
        x2 = factor(sample(1:4, n, prob = c(1 / 5, 1 / 5, 1 / 5, 2 / 5), replace = TRUE)),
        x3 = factor(sample(1:3, n, prob = c(1 / 5, 1 / 5, 3 / 5), replace = TRUE)),
        x4 = (x1 + rnorm(n)) / 2,
        x5 = rnorm(n),
        y = a + x1 - 0.5 * a * (x1 - mean(x1)) + as.double(x2) + rnorm(n)
    )

userid <- rlang::expr(uid)

propensity_score_variable_name <- "ps"

continuous_covariates <- c("x1")

discrete_covariates <- c("x2", "x3")

continuous_moderators <- rlang::exprs(x1, x4, x5)
discrete_moderators <- rlang::exprs(x2, x3)
moderators <- c(continuous_moderators, discrete_moderators)

model_covariate_names <- c(continuous_covariates, discrete_covariates)
model_covariates <- rlang::syms(model_covariate_names)

outcome_variable <- rlang::expr(y)
treatment_variable <- rlang::expr(a)

trt.cfg <- Known_cfg$new(propensity_score_variable_name)

regression.cfg <- SLEnsemble_cfg$new(
    learner_cfgs = list(
        SLLearner_cfg$new(
            "SL.glm"
        ),
        SLLearner_cfg$new(
            "SL.glmnet",
            list(
                alpha = c(0.05, 0.15)
            )
        )
    )
)

qoi.list <- list()
for (cov in continuous_moderators) {
    qoi.list[[rlang::as_string(cov)]] <- KernelSmooth_cfg$new(neval = 100, eval_min_quantile = 0.05)
}
for (cov in discrete_moderators) {
    qoi.list[[rlang::as_string(cov)]] <- Stratified_cfg$new(cov)
}

qoi.cfg <- QoI_cfg$new(
    mcate = MCATE_cfg$new(cfgs = qoi.list),
    pcate = PCATE_cfg$new(
        cfgs = qoi.list,
        model_covariates = model_covariate_names,
        num_mc_samples = list(x1 = 5, x2 = 10, x3 = 10, x4 = 5, x5 = 5)
    ),
    diag = Diagnostics_cfg$new(
        outcome = c("SL_risk", "SL_coefs", "MSE"),
        effect = c("SL_risk", "SL_coefs")
    ),
    predictions = TRUE
)

qoi.cfg2 <- QoI_cfg$new(
    mcate = MCATE_cfg$new(cfgs = qoi.list, std_errors = FALSE),
    pcate = PCATE_cfg$new(
        cfgs = qoi.list,
        model_covariates = model_covariate_names,
        num_mc_samples = list(x1 = 5, x2 = 10, x3 = 10, x4 = 5, x5 = 5)
    ),
    diag = Diagnostics_cfg$new(
        outcome = c("SL_risk", "SL_coefs", "MSE"),
        effect = c("SL_risk", "SL_coefs")
    )
)

cfg <- HTE_cfg$new(
    treatment = trt.cfg,
    outcome = regression.cfg,
    effect = regression.cfg,
    qoi = qoi.cfg
)

cfg2 <- HTE_cfg$new(
    treatment = trt.cfg,
    outcome = regression.cfg,
    effect = regression.cfg,
    qoi = qoi.cfg2
)

E = new.env(parent = emptyenv())

test_that("add config", {
    E$data <- attach_config(data, cfg)
    checkmate::expect_data_frame(E$data)
    expect_true("HTE_cfg" %in% names(attributes(E$data)))
})

test_that("Split data", {
    E$data2 <- make_splits(E$data, {{ userid }}, .num_splits = 3)
    checkmate::expect_data_frame(E$data2)
})

test_that("Estimate Plugin Models", {
    E$data3 <- produce_plugin_estimates(
        E$data2,
        {{ outcome_variable }},
        {{ treatment_variable }},
        !!!model_covariates
    )
    checkmate::expect_data_frame(E$data3)
})

test_that("Construct Pseudo-outcomes", {
    E$data4 <- construct_pseudo_outcomes(E$data3, {{ outcome_variable }}, {{ treatment_variable }})
    checkmate::expect_data_frame(E$data4)
})

test_that("Estimate QoIs", {
    skip_on_cran()
    expect_warning(
        E$results <- estimate_QoI(E$data4, !!!moderators),
        "Only use PCATEs if you know what you're doing!"
    )
    checkmate::expect_data_frame(E$results)
    E$data4 <- attach_config(E$data4, cfg2)
    expect_warning(
        E$results2 <- estimate_QoI(E$data4, !!!moderators),
        "Only use PCATEs if you know what you're doing!"
    )
    checkmate::expect_data_frame(E$results2)
})

n_rows <- (
    1 + # SATE estimate
    2 + # MSE for y(0) & y(1)
    3 * 3 + # one row per model in the ensemble for each PO / fx for SL risk
    3 * 3 + # one row per model in the ensemble for each PO / fx for SL coefficient
    2 * 3 * 100 + # 100 rows per continuous moderator for local regression for MCATE and for PCATE
    2 * (4 + 3) + # 2 rows per discrete moderator level for MCATE and for PCATE
    n # One row for each predicted value
)


test_that("Check results data", {
    skip_on_cran()
    checkmate::check_character(E$results$estimand, any.missing = FALSE)
    checkmate::check_double(E$results$estimate, any.missing = FALSE)
    checkmate::check_double(E$results$std_error, any.missing = FALSE)

    checkmate::expect_tibble(
        E$results,
        all.missing = FALSE,
        nrows = n_rows,
        ncols = 6,
        types = c(
            estimand = "character",
            term = "character",
            value = "double",
            level = "character",
            estimate = "double",
            std_error = "double"
        )
    )
})

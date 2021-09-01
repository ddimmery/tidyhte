set.seed(20051920) # 20051920 == 'test'

n <- 250
n0 <- 20
data <- dplyr::tibble(
    uid = 1:(n + n0)
) %>%
    dplyr::mutate(
        a = rbinom(n + n0, 1, 0.5),
        ps = rep(0.5, n + n0),
        x1 = c(rnorm(n), rep(0, n0)),
        x2 = factor(sample(1:4, n + n0, prob = c(1 / 5, 1 / 5, 1 / 5, 2 / 5), replace = TRUE)),
        y = a + x1 - 0.5 * a * (x1 - mean(x1)) + as.double(x2) + c(rnorm(n), rep(rnorm(1), n0))
    )

userid <- rlang::expr(uid)

propensity_score_variable_name <- "ps"

continuous_covariates <- c("x1")

discrete_covariates <- c("x2")

continuous_moderators <- rlang::exprs(x1)
discrete_moderators <- rlang::exprs(x2)
moderators <- c(continuous_moderators, discrete_moderators)

model_covariate_names <- c(continuous_covariates, discrete_covariates)
model_covariates <- rlang::syms(model_covariate_names)

outcome_variable <- rlang::expr(y)
treatment_variable <- rlang::expr(a)

trt.cfg <- Constant_cfg$new()

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
    qoi.list[[rlang::as_string(cov)]] <- KernelSmooth_cfg$new(neval = 100)
}
for (cov in discrete_moderators) {
    qoi.list[[rlang::as_string(cov)]] <- Stratified_cfg$new(cov)
}

qoi.cfg <- QoI_cfg$new(
    mcate = MCATE_cfg$new(cfgs = qoi.list),
    vimp = VIMP_cfg$new(model_cfg = regression.cfg, num_splits = 4),
    diag = Diagnostics_cfg$new(
        outcome = c("SL_risk", "SL_coefs", "MSE")
    )
)

cfg <- HTE_cfg$new(
    treatment = trt.cfg,
    outcome = regression.cfg,
    qoi = qoi.cfg
)

test_that("Split data", {
    data2 <<- make_splits(data, {{ userid }}, .num_splits = 4)
    checkmate::expect_data_frame(data2)
})

test_that("Estimate Plugin Models", {
    data3 <<- produce_plugin_estimates(
        data2,
        {{ outcome_variable }},
        {{ treatment_variable }},
        !!!model_covariates,
        .HTE_cfg = cfg
    )
    checkmate::expect_data_frame(data3)
})

test_that("Construct Pseudo-outcomes", {
    data4 <<- construct_pseudo_outcomes(data3, {{ outcome_variable }}, {{ treatment_variable }})
    checkmate::expect_data_frame(data4)
})

test_that("Estimate QoIs", {
    skip_on_cran()
    results <<- estimate_QoI(data4, !!!moderators, .HTE_cfg = cfg)
    checkmate::expect_data_frame(results)
})

n_rows <- (
    1 + # ATE estimate
    2 + # MSE for y(0) & y(1)
    2 * 3 + # one row per model in the ensemble for each PO + ps for SL risk
    2 * 3 + # one row per model in the ensemble for each PO + ps for SL coefficient
    2 + # one row per moderator for variable importance
    1 * 100 + # 100 rows per continuous moderator for local regression for MCATE and for PCATE
    (2 + 2) # 2 rows per discrete moderator level for MCATE and for PCATE
)


test_that("Check results data", {
    skip_on_cran()
    checkmate::expect_tibble(
        results,
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
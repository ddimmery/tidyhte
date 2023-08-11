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

trt.cfg <- SLEnsemble_cfg$new(
    learner_cfgs = list(
        SLLearner_cfg$new(
            "SL.glm"
        )
    ),
    family = stats::binomial()
)

regression.cfg <- SLEnsemble_cfg$new(
    learner_cfgs = list(
        SLLearner_cfg$new(
            "SL.glm"
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
    diag = Diagnostics_cfg$new(
        outcome = c("SL_risk", "SL_coefs", "MSE"),
        ps = c("SL_risk", "SL_coefs", "AUC")
    )
)

cfg <- HTE_cfg$new(
    treatment = trt.cfg,
    outcome = regression.cfg,
    qoi = qoi.cfg
)

E = new.env(parent = emptyenv())

test_that("add config", {
    E$data1 <- attach_config(data, cfg)
    checkmate::expect_data_frame(E$data1)
    expect_true("HTE_cfg" %in% names(attributes(E$data1)))
})

test_that("Split data", {
    E$data2 <- make_splits(E$data1, {{ userid }}, .num_splits = 3)
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

test_that("Errors on unknown type", {
    expect_error(
        construct_pseudo_outcomes(
            E$data3, {{ outcome_variable }}, {{ treatment_variable }}, type = "idk"
        ),
        "Unknown type of pseudo-outcome."
    )
})

test_that("Construct DR Pseudo-outcomes", {
    data4 <- construct_pseudo_outcomes(
        E$data3, {{ outcome_variable }}, {{ treatment_variable }}
    )
    checkmate::expect_data_frame(data4)
})

test_that("Construct IPW Pseudo-outcomes", {
    data4 <- construct_pseudo_outcomes(
        E$data3, {{ outcome_variable }}, {{ treatment_variable }}, "ipw"
    )
    checkmate::expect_data_frame(data4)
})

test_that("Construct Plugin Pseudo-outcomes", {
    data4 <- construct_pseudo_outcomes(
        E$data3, {{ outcome_variable }}, {{ treatment_variable }}, "plugin"
    )
    checkmate::expect_data_frame(data4)
})

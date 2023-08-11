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
        y = a + x1 - 2.5 * a * (x1 - mean(x1)) + as.double(x2) + rnorm(n),
        w = rexp(n, plogis(x1 - mean(x1)))
    )

userid <- rlang::expr(uid)
weight_variable <- rlang::expr(w)

propensity_score_variable_name <- "ps"

continuous_covariates <- c("x1")

discrete_covariates <- c("x2", "x3")

continuous_moderators <- rlang::exprs(x1)
discrete_moderators <- rlang::exprs(x2, x3)

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
    family = stats::quasibinomial()
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
    vimp = VIMP_cfg$new(),
    diag = Diagnostics_cfg$new(
        outcome = c("SL_risk", "SL_coefs", "MSE"),
        ps = c("SL_risk", "SL_coefs", "AUC")
    )
)

cfg <- HTE_cfg$new(
    treatment = trt.cfg,
    outcome = regression.cfg,
    effect = regression.cfg,
    qoi = qoi.cfg
)

E = new.env(parent = emptyenv())

test_that("add config", {
    E$data1 <- attach_config(data, cfg)
    checkmate::expect_data_frame(E$data1)
    expect_true("HTE_cfg" %in% names(attributes(E$data1)))
})

test_that("Split data", {
    E$data2 <- make_splits(E$data1, {{ userid }}, .num_splits = 4)
    checkmate::expect_data_frame(E$data2)
})

test_that("Estimate Plugin Models", {
    E$data3 <- produce_plugin_estimates(
        E$data2,
        {{ outcome_variable }},
        {{ treatment_variable }},
        !!!model_covariates,
        .weights = {{ weight_variable }}
    )
    checkmate::expect_data_frame(E$data3)
})

test_that("Construct Pseudo-outcomes", {
    E$data4 <- construct_pseudo_outcomes(E$data3, {{ outcome_variable }}, {{ treatment_variable }})
    checkmate::expect_data_frame(E$data4)
})

test_that("Estimate QoIs (continuous)", {
    expect_error(
        estimate_QoI(E$data4, !!!continuous_moderators),
        "`nprobust` does not support the use of weights."
    )
})

test_that("Estimate QoIs (discrete)", {
    E$results <- estimate_QoI(E$data4, !!!discrete_moderators)
    checkmate::expect_data_frame(E$results)
})

n_rows <- (
    1 + # SATE estimate
    1 + # PATE estimate
    2 + # MSE for y(0) & y(1)
    1 + # AUC for pscore
    2 * 1 + 1 + # one row per model in the ensemble for each PO + ps for SL risk
    2 * 1 + 1 + # one row per model in the ensemble for each PO + ps for SL coefficient
    2 + # one row per moderator for variable importance
    # 1 * 100 + # 100 rows per continuous moderator for local regression for MCATE
    (4 + 3) # 1 rows per discrete moderator level for MCATE
)

test_that("PATE > SATE", {
    # PATE has larger treatment effects (because weight is correlated with moderator)
    pate <- E$results %>% dplyr::filter(grepl("PATE", estimand)) %>% select(estimate) %>% unlist()
    sate <- E$results %>% dplyr::filter(grepl("SATE", estimand)) %>% select(estimate) %>% unlist()
    expect_gt(pate, sate)

    # Weighting makes the standard error larger
    pate <- E$results %>% dplyr::filter(grepl("PATE", estimand)) %>% select(std_error) %>% unlist()
    sate <- E$results %>% dplyr::filter(grepl("SATE", estimand)) %>% select(std_error) %>% unlist()
    expect_gt(pate, sate)
})

test_that("Check results data", {
    checkmate::check_character(E$results$estimand, any.missing = FALSE)
    checkmate::check_double(E$results$estimate, any.missing = FALSE)
    checkmate::check_double(E$results$std_error, any.missing = FALSE)

    checkmate::expect_tibble(
        E$results,
        all.missing = FALSE,
        nrows = n_rows,
        ncols = 5, # only 5 columns because no continuous moderators
        types = c(
            estimand = "character",
            term = "character",
            value = "numeric",
            level = "character",
            estimate = "double",
            std_error = "double"
        )
    )
})

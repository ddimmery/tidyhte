set.seed(20051920) # 20051920 == 'test'

n <- 1000
data <- dplyr::tibble(
    uid = 1:n
) %>%
    dplyr::mutate(
        a = rbinom(n, 1, 0.5),
        ps = rep(0.5, n),
        x1 = rnorm(n),
        x2 = factor(sample(1:4, n, prob = c(1 / 5, 1 / 5, 1 / 5, 2 / 5), replace = TRUE)),
        y = a + x1 + as.double(x2) + rnorm(n)
    )

propensity_score_variable_name <- "ps"
continuous_covariates <- c("x1")
discrete_covariates <- c("x2")

outcome_variable <- rlang::expr(y)
treatment_variable <- rlang::expr(a)


all_covariates <- rlang::syms(c(continuous_covariates, discrete_covariates))

trt.cfg <- Known_cfg$new(propensity_score_variable_name)

regression.cfg <- SLEnsemble_cfg$new(
    learner_cfgs = list(
        SLLearner_cfg$new(
            "SL.glm"
        ),
        SLLearner_cfg$new(
            "SL.glm.interaction"
        ),
        SLLearner_cfg$new(
            "SL.gam",
            list(
                deg.gam = c(2, 3, 4)
            )
        )
    )
)

qoi.list <- list()
for (cov in continuous_covariates) {
    qoi.list[[cov]] <- KernelSmooth_cfg$new(neval = 100)
}
for (cov in discrete_covariates) {
    qoi.list[[cov]] <- Stratified_cfg$new(cov)
}

qoi.cfg <- QoI_cfg$new(
    mcate = MCATE.cfg$new(cfgs = qoi.list),
    vimp = VIMP.cfg$new(model_cfg = regression.cfg)
)

cfg <- HTE_cfg$new(
    treatment = trt.cfg,
    outcome = regression.cfg,
    qoi = qoi.cfg
)

data %>%
    make_splits(uid, .num_splits = 3) %>%
    produce_plugin_estimates(
        {{ outcome_variable }},
        {{ treatment_variable }},
        !!!all_covariates,
        .HTE_cfg = cfg
    ) %>%
    construct_pseudo_outcomes({{ outcome_variable }}, {{ treatment_variable }}) %>%
    estimate_QoI({{ outcome_variable }}, {{ treatment_variable }}, !!!all_covariates, .HTE_cfg = cfg) -> results

devtools::load_all()

set.seed(100)
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

trt.cfg <- Known.cfg$new(propensity_score_variable_name)

regression.cfg <- SL.cfg$new(
    learner_cfgs = list(
        SL.Learner.cfg$new(
            "SL.glm"
        ),
        SL.Learner.cfg$new(
            "SL.glm.interaction"
        ),
        SL.Learner.cfg$new(
            "SL.gam",
            list(
                deg.gam = c(2, 3, 4, 5, 7, 9) #13, 17
            )
        )
        # SL.Learner.cfg$new(
        #     "SL.glmnet",
        #     list(
        #         alpha=c(0.05, 0.15, 0.2, 0.25),
        #         loss=c('mse', 'deviance')
        #     )
        # ),
        # SL.Learner.cfg$new(
        #     "SL.glmnet.interaction",
        #     list(
        #         alpha=c(0.05, 0.15, 0.2, 0.25),
        #         loss=c('mse', 'deviance')
        #     )
        # ),
        # SL.Learner.cfg$new(
        #     "SL.ranger",
        #     list(
        #         num.trees = c(25, 50, 100, 250, 500), #750, 1000, 2000
        #         splitrule=c('gini', 'extratrees', 'hellinger')
        #     )
        # ),
        # SL.Learner.cfg$new(
        #     "SL.xgboost",
        #     list(
        #         ntrees = c(50, 100, 250, 500),#1000, 2500
        #         max_depth = c(1, 3, 5, 7, 9),
        #         shrinkage = c(0.01, 0.1)
        #     )
        # )
    )
)

qoi.list <- list()
for (cov in continuous_covariates) {
    qoi.list[[cov]] <- KernelSmooth.cfg$new(neval = 100)
}
for (cov in discrete_covariates) {
    qoi.list[[cov]] <- Stratified.cfg$new(cov)
}

qoi.cfg <- QoI.cfg$new(
    mcate = MCATE.cfg$new(cfgs = qoi.list),
    vimp = VIMP.cfg$new(model_cfg = regression.cfg)
)

cfg <- HTE.cfg$new(
    treatment = trt.cfg,
    outcome = regression.cfg,
    qoi = qoi.cfg
)

# initialize_config() %>%
#     add_quantity_of_interest("MCATE", x1, 'KernelSmooth') %>%
#     add_propensity_model("known", pscore) %>%

data %>%
    make.splits(uid, .num_splits = 3) %>%
    produce.plugin.estimates(
        {{ outcome_variable }},
        {{ treatment_variable }},
        !!!all_covariates,
        .HTE.cfg = cfg
    ) %>%
    construct.pseudo.outcomes({{ outcome_variable }}, {{ treatment_variable }}) %>%
    estimate.QoI(!!!all_covariates, .HTE.cfg = cfg) -> results


# ggplot2::ggplot(dplyr::filter(results, estimand == 'VIMP')) +
#     ggplot2::geom_pointrange(
#         ggplot2::aes(x = covariate_name, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error)) +
#     ggplot2::coord_flip() +
#     ggplot2::theme_minimal() -> gp
# print(gp)


# for(cov in continuous_covariates) {
#     ggplot2::ggplot(dplyr::filter(results, estimand == 'MCATE', covariate_name == cov)) +
#         ggplot2::geom_ribbon(
#             ggplot2::aes(x = value, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), alpha = 0.75
#         ) +
#         ggplot2::geom_line(
#             ggplot2::aes(x=value, y=estimate)
#         ) +
#         ggplot2::scale_x_continuous() +
#         ggplot2::facet_wrap(~covariate_name, scales='free') +
#         ggplot2::theme_minimal() -> gp

#     print(gp)
# }

# for(cov in discrete_covariates) {
#     ggplot2::ggplot(dplyr::filter(results, estimand == 'MCATE', covariate_name == cov)) +
#         ggplot2::geom_pointrange(
#             ggplot2::aes(x = level, y = estimate, ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), data = dplyr::filter(results, !is.na(level))) +
#         ggplot2::scale_x_discrete() +
#         ggplot2::facet_wrap(~covariate_name, scales='free') +
#         ggplot2::theme_minimal() -> gp

#     print(gp)
# }

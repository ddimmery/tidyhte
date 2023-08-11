propensity_score_variable_name <- "ps"
continuous_covariates <- c("x1")
discrete_covariates <- c("x2")

outcome_variable <- rlang::expr(y)
treatment_variable <- rlang::expr(a)


all_covariates <- rlang::syms(c(continuous_covariates, discrete_covariates))
model_covariate_names <- c(continuous_covariates, discrete_covariates)

E = new.env(parent = emptyenv())

test_that("Configs can be constructed successfully.", {
    E$trt.cfg <- Known_cfg$new(propensity_score_variable_name)

    E$basic.regression.cfg <- SLEnsemble_cfg$new(cvControl = list(V = 5))

    E$regression.cfg <- SLEnsemble_cfg$new(
        learner_cfgs = list(
            SLLearner_cfg$new(
                "SL.glm"
            ),
            SLLearner_cfg$new(
                "SL.gam",
                list(
                    deg.gam = c(2, 3, 4, 5, 7, 9)
                )
            ),
            SLLearner_cfg$new(
                "SL.glmnet",
                list(
                    alpha = c(0.05, 0.15, 0.2, 0.25),
                    loss = c("mse", "deviance")
                )
            ),
            SLLearner_cfg$new(
                "SL.glmnet.interaction",
                list(
                    alpha = c(0.05, 0.15, 0.2, 0.25),
                    loss = c("mse", "deviance")
                )
            ),
            SLLearner_cfg$new(
                "SL.ranger",
                list(
                    num.trees = c(25, 50, 100, 250, 500),
                    splitrule = c("gini", "extratrees", "hellinger")
                )
            ),
            SLLearner_cfg$new(
                "SL.xgboost",
                list(
                    ntrees = c(50, 100, 250, 500),
                    max_depth = c(1, 3, 5, 7, 9),
                    shrinkage = c(0.01, 0.1)
                )
            )
        )
    )

    E$regression.cfg <- E$regression.cfg$add_sublearner("SL.glm.interaction")

    E$qoi.list <- list()
    for (cov in continuous_covariates) {
        E$qoi.list[[cov]] <- KernelSmooth_cfg$new(neval = 100)
    }
    for (cov in discrete_covariates) {
        E$qoi.list[[cov]] <- Stratified_cfg$new(cov)
    }

    E$diag.cfg <- Diagnostics_cfg$new(
        outcome = c("SL_risk", "SL_coefs", "MSE"),
        effect = c("SL_risk", "SL_coefs")
    )

    E$diag.cfg$add(effect = "MSE")
    E$diag.cfg$add(ps = "MSE")
    E$diag.cfg$add(outcome = "MSE")

    Diagnostics_cfg$new(
        ps = "MSE"
    )

    E$mcate.cfg <- MCATE_cfg$new(cfgs = E$qoi.list)

    E$mcate.cfg$add_moderator("new_x", Stratified_cfg$new("new_x"))

    E$pcate.cfg <- PCATE_cfg$new(
        cfgs = E$qoi.list,
        model_covariates = model_covariate_names,
        num_mc_samples = 10
    )

    E$pcate.cfg$add_moderator("new_x", Stratified_cfg$new("new_x"))

    expect_error(PCATE_cfg$new(
        cfgs = E$qoi.list,
        model_covariates = model_covariate_names,
        num_mc_samples = "not a number"
    ), "Unknown type of num_mc_samples")

    E$pcate.cfg <- PCATE_cfg$new(
        cfgs = E$qoi.list,
        model_covariates = model_covariate_names,
        num_mc_samples = list(x1 = 5, x2 = 10, x3 = 10, x4 = 5, x5 = 5)
    )

    E$vimp.cfg <- VIMP_cfg$new()

    E$qoi.cfg <- QoI_cfg$new(
        mcate = E$mcate.cfg,
        vimp = E$vimp.cfg,
        pcate = E$pcate.cfg,
        diag = E$diag.cfg
    )

    expect_error(QoI_cfg$new(), "Must define at least one QoI!")

    E$cfg <- HTE_cfg$new(
        treatment = E$trt.cfg,
        outcome = E$regression.cfg,
        effect = E$regression.cfg,
        qoi = E$qoi.cfg
    )

    expect_error(HTE_cfg$new(), "Must define at least one QoI!")

    HTE_cfg$new(qoi = E$qoi.cfg)
})


test_that("Config types are as expected.", {
    checkmate::expect_r6(E$cfg, classes = "HTE_cfg")
    checkmate::expect_r6(E$qoi.cfg, classes = "QoI_cfg")
    for (qoi in E$qoi.list) {
        checkmate::expect_r6(qoi, classes = "Model_cfg")
    }
    checkmate::expect_r6(E$regression.cfg, classes = "SLEnsemble_cfg")
    checkmate::expect_environment(E$regression.cfg$SL.env)
    checkmate::expect_character(E$regression.cfg$SL.library, len = 79)
    checkmate::expect_r6(E$trt.cfg, classes = c("Known_cfg", "Model_cfg"))
    checkmate::expect_r6(E$vimp.cfg, classes = "VIMP_cfg")
    checkmate::expect_r6(E$pcate.cfg, classes = "PCATE_cfg")
    checkmate::expect_r6(E$mcate.cfg, classes = "MCATE_cfg")
    checkmate::expect_r6(E$diag.cfg, classes = "Diagnostics_cfg")
})

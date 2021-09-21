propensity_score_variable_name <- "ps"
continuous_covariates <- c("x1")
discrete_covariates <- c("x2")

outcome_variable <- rlang::expr(y)
treatment_variable <- rlang::expr(a)


all_covariates <- rlang::syms(c(continuous_covariates, discrete_covariates))
model_covariate_names <- c(continuous_covariates, discrete_covariates)

trt.cfg <- Known_cfg$new(propensity_score_variable_name)

test_that("Configs can be constructed successfully.", {
    basic.regression.cfg <<- SLEnsemble_cfg$new(cvControl = list(V = 5))

    regression.cfg <<- SLEnsemble_cfg$new(
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

    qoi.list <<- list()
    for (cov in continuous_covariates) {
        qoi.list[[cov]] <- KernelSmooth_cfg$new(neval = 100)
    }
    for (cov in discrete_covariates) {
        qoi.list[[cov]] <- Stratified_cfg$new(cov)
    }

    diag.cfg <<- Diagnostics_cfg$new(
        outcome = c("SL_risk", "SL_coefs", "MSE"),
        effect = c("SL_risk", "SL_coefs")
    )

    Diagnostics_cfg$new(
        ps = "MSE"
    )

    mcate.cfg <<- MCATE_cfg$new(cfgs = qoi.list)

    PCATE_cfg$new(
        cfgs = qoi.list,
        effect_cfg = regression.cfg,
        model_covariates = model_covariate_names,
        num_mc_samples = 10
    )

    expect_error(PCATE_cfg$new(
        cfgs = qoi.list,
        effect_cfg = regression.cfg,
        model_covariates = model_covariate_names,
        num_mc_samples = "not a number"
    ), "Unknown type of num_mc_samples")

    pcate.cfg <<- PCATE_cfg$new(
        cfgs = qoi.list,
        effect_cfg = regression.cfg,
        model_covariates = model_covariate_names,
        num_mc_samples = list(x1 = 5, x2 = 10, x3 = 10, x4 = 5, x5 = 5)
    )

    vimp.cfg <<- VIMP_cfg$new(model_cfg = regression.cfg)

    qoi.cfg <<- QoI_cfg$new(
        mcate = mcate.cfg,
        vimp = vimp.cfg,
        pcate = pcate.cfg,
        diag = diag.cfg
    )

    expect_error(QoI_cfg$new(), "Must define at least one QoI!")

    cfg <<- HTE_cfg$new(
        treatment = trt.cfg,
        outcome = regression.cfg,
        qoi = qoi.cfg
    )

    expect_error(HTE_cfg$new(), "Must define at least one QoI!")

    HTE_cfg$new(qoi = qoi.cfg)
})


test_that("Config types are as expected.", {
    checkmate::expect_r6(cfg, classes = "HTE_cfg")
    checkmate::expect_r6(qoi.cfg, classes = "QoI_cfg")
    for (qoi in qoi.list) {
        checkmate::expect_r6(qoi, classes = "Model_cfg")
    }
    checkmate::expect_r6(regression.cfg, classes = "SLEnsemble_cfg")
    checkmate::expect_environment(regression.cfg$SL.env)
    checkmate::expect_character(regression.cfg$SL.library, len = 79)
    checkmate::expect_r6(trt.cfg, classes = c("Known_cfg", "Model_cfg"))
    checkmate::expect_r6(vimp.cfg, classes = "VIMP_cfg")
    checkmate::expect_r6(pcate.cfg, classes = "PCATE_cfg")
    checkmate::expect_r6(mcate.cfg, classes = "MCATE_cfg")
    checkmate::expect_r6(diag.cfg, classes = "Diagnostics_cfg")
})


<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyhte

<!-- badges: start -->

[![lint](https://github.com/ddimmery/tidyhte/actions/workflows/lint.yaml/badge.svg)](https://github.com/ddimmery/tidyhte/actions/workflows/lint.yaml)
[![codecov](https://codecov.io/gh/ddimmery/tidyhte/branch/main/graph/badge.svg?token=AHT3X4S2KQ)](https://codecov.io/gh/ddimmery/tidyhte)
[![R-CMD-check](https://github.com/ddimmery/tidyhte/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ddimmery/tidyhte/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`tidyhte` provides tidy semantics for estimation of heterogeneous
treatment effects through the use of [Kennedy’s (n.d.) doubly-robust
learner](https://arxiv.org/abs/2004.14497).

The goal of `tidyhte` is to use a sort of “recipe” design. This should
(hopefully) make it extremely easy to scale an analysis of HTE from the
common single-outcome / single-moderator case to many outcomes and many
moderators. The configuration of `tidyhte` should make it extremely easy
to perform the same analysis across many outcomes and for a wide-array
of moderators. It’s written to be fairly easy to extend to different
models and to add additional diagnostics and ways to output information
from a set of HTE estimates.

The best place to start for learning how to use `tidyhte` is the
vignette which runs through an example analysis from start to finish:
`vignette("example_analysis")`

# Installation

You will be able to install the released version of tidyhte from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("tidyhte")
```

But this does not yet exist. In the meantime, install the development
version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ddimmery/tidyhte")
```

# Setting up a configuration

The eventual API for this will look more or less like the following:

``` r
initialize_config() %>%
    add_propensity_model("known", pscore) %>%
    add_outcome_model("glmnet", interactions = FALSE, alpha = c(0.05, 0.15, 0.2, 0.25)) %>%
    add_outcome_model("glmnet", interactions = TRUE, alpha = c(0.05, 0.15, 0.2, 0.25)) %>%
    add_outcome_model("xgboost", ntrees = c(50, 100, 250, 500), shrinkage = c(0.01, 0.1)) %>%
    add_quantity_of_interest("MCATE", x1, "KernelSmooth") %>%
    add_quantity_of_interest("MCATE", x2, "Stratified") %>%
    add_quantity_of_interest("VIMP", x1, x2) %>%
    add_propensity_diagnostic("AUC") %>%
    add_propensity_diagnostic("SL coefficients") %>%
    add_outcome_diagnostic("MSE") %>%
    add_outcome_diagnostic("SL coefficients")
```

This is not yet implemented, so the current approach is a bit less
pretty:

``` r
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
    qoi.list[[rlang::as_string(cov)]] <- KernelSmooth_cfg$new(neval = 100)
}
for (cov in discrete_moderators) {
    qoi.list[[rlang::as_string(cov)]] <- Stratified_cfg$new(cov)
}

qoi.cfg <- QoI_cfg$new(
    mcate = MCATE.cfg$new(cfgs = qoi.list),
    pcate = PCATE.cfg$new(
        cfgs = qoi.list,
        effect_cfg = regression.cfg,
        model_covariates = model_covariate_names,
        num_mc_samples = list(x1 = 5, x2 = 10, x3 = 10, x4 = 5, x5 = 5)
    ),
    vimp = VIMP.cfg$new(model_cfg = regression.cfg),
    diag = Diagnostics.cfg$new(
        outcome = c("SL_risk", "SL_coefs", "MSE"),
        effect = c("SL_risk", "SL_coefs")
    )
)

cfg <- HTE_cfg$new(
    treatment = trt.cfg,
    outcome = regression.cfg,
    qoi = qoi.cfg
)
```

# Running an Analysis

``` r
data %>%
    make_splits(userid, .num_splits = 12) %>%
    produce_plugin_estimates(
        outcome_variable,
        treatment_variable,
        covariate1, covariate2, covariate3, covariate4, covariate5, covariate6,
        .HTE_cfg = cfg
    ) %>%
    construct_pseudo_outcomes(outcome_variable, treatment_variable) -> data

data %>%
    estimate_QoI(outcome_variable, treatment_variable, covariate1, covariate2, .HTE_cfg = cfg) -> results
```

To get information on a moderator not included above would just require
rerunning the final line:

``` r
data %>%
    estimate_QoI(outcome_variable, treatment_variable, covariate3, .HTE_cfg = cfg) -> results
```

Replicating this on a new outcome would be as simple as running the
following, with no reconfiguration necessary.

``` r
data %>%
    produce_plugin_estimates(
        second_outcome_variable,
        treatment_variable,
        covariate1, covariate2, covariate3, covariate4, covariate5, covariate6,
        .HTE_cfg = cfg
    ) %>%
    construct_pseudo_outcomes(second_outcome_variable, treatment_variable) %>%
    estimate_QoI(second_outcome_variable, treatment_variable, covariate1, covariate2, .HTE_cfg = cfg) -> results
```

This leads to the ability to easily chain together analyses across many
outcomes in an easy way:

``` r
data %>%
    make_splits(userid, .num_splits = 12) -> data

foreach(outcome = list_of_outcomes, .combine = "bind_rows") %dopar% {
    data %>%
    produce_plugin_estimates(
        outcome,
        treatment_variable,
        covariate1, covariate2, covariate3, covariate4, covariate5, covariate6,
        .HTE_cfg = cfg
    ) %>%
    construct_pseudo_outcomes(outcome, treatment_variable) %>%
    estimate_QoI(outcome, treatment_variable, covariate1, covariate2, .HTE_cfg = cfg) %>%
    mutate(outcome = rlang::as_string(outcome))
}
```

I’m sure you could do that purely tidily if you wanted.

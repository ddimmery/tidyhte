
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyhte

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![lint](https://github.com/ddimmery/tidyhte/actions/workflows/lint.yaml/badge.svg)](https://github.com/ddimmery/tidyhte/actions/workflows/lint.yaml)
[![codecov](https://codecov.io/gh/ddimmery/tidyhte/branch/main/graph/badge.svg?token=AHT3X4S2KQ)](https://app.codecov.io/gh/ddimmery/tidyhte)
[![R-CMD-check](https://github.com/ddimmery/tidyhte/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ddimmery/tidyhte/actions/workflows/R-CMD-check.yaml)
[![CRAN
Status](https://www.r-pkg.org/badges/version/tidyhte)](https://cran.r-project.org/package=tidyhte)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit/)
[![DOI](https://zenodo.org/badge/375330850.svg)](https://zenodo.org/badge/latestdoi/375330850)
[![](https://dcbadge.vercel.app/api/server/MrxjbHc3jD?style=flat)](https://discord.com/invite/MrxjbHc3jD)
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

The best place to start for learning how to use `tidyhte` are the
vignettes which runs through example analyses from start to finish:
`vignette("experimental_analysis")` and
`vignette("observational_analysis")`. There is also a writeup
summarizing the method and implementation in
`vignette("methodological_details")`.

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

To set up a simple configuration, it’s straightforward to use the Recipe
API:

``` r
library(tidyhte)
library(dplyr)

basic_config() %>%
    add_propensity_score_model("SL.glmnet") %>%
    add_outcome_model("SL.glmnet") %>%
    add_moderator("Stratified", x1, x2) %>%
    add_moderator("KernelSmooth", x3) %>%
    add_vimp(sample_splitting = FALSE) -> hte_cfg
```

The `basic_config` includes a number of defaults: it starts off the
SuperLearner ensembles for both treatment and outcome with linear models
(`"SL.glm"`)

# Running an Analysis

``` r
data %>%
    attach_config(hte_cfg) %>%
    make_splits(userid, .num_splits = 12) %>%
    produce_plugin_estimates(
        outcome_variable,
        treatment_variable,
        covariate1, covariate2, covariate3, covariate4, covariate5, covariate6
    ) %>%
    construct_pseudo_outcomes(outcome_variable, treatment_variable) -> data

data %>%
    estimate_QoI(covariate1, covariate2) -> results
```

To get information on estimate CATEs for a moderator not included
previously would just require rerunning the final line:

``` r
data %>%
    estimate_QoI(covariate3) -> results
```

Replicating this on a new outcome would be as simple as running the
following, with no reconfiguration necessary.

``` r
data %>%
    attach_config(hte_cfg) %>%
    produce_plugin_estimates(
        second_outcome_variable,
        treatment_variable,
        covariate1, covariate2, covariate3, covariate4, covariate5, covariate6
    ) %>%
    construct_pseudo_outcomes(second_outcome_variable, treatment_variable) %>%
    estimate_QoI(covariate1, covariate2) -> results
```

This leads to the ability to easily chain together analyses across many
outcomes in an easy way:

``` r
library("foreach")

data %>%
    attach_config(hte_cfg) %>%
    make_splits(userid, .num_splits = 12) -> data

foreach(outcome = list_of_outcomes, .combine = "bind_rows") %do% {
    data %>%
    produce_plugin_estimates(
        outcome,
        treatment_variable,
        covariate1, covariate2, covariate3, covariate4, covariate5, covariate6
    ) %>%
    construct_pseudo_outcomes(outcome, treatment_variable) %>%
    estimate_QoI(covariate1, covariate2) %>%
    mutate(outcome = rlang::as_string(outcome))
}
```

The function `estimate_QoI` returns results in a tibble format which
makes it easy to manipulate or plot results.

# Getting help

There are two main ways to get help:

## GitHub Issues

If you have a problem, feel free to [open an issue on
GitHub](https://github.com/ddimmery/tidyhte/issues/new/choose). Please
try to provide a [minimal reproducible
example](https://stackoverflow.com/help/minimal-reproducible-example).
If that isn’t possible, explain as clearly and simply why that is, along
with all of the relevant debugging steps you’ve already taken.

## Discord

Support for the package will also be provided in the Experimentation
Community Discord:
[![](https://dcbadge.vercel.app/api/server/MrxjbHc3jD?theme=clean&compact=true)](https://discord.com/invite/MrxjbHc3jD)

You are welcome to come in and get support for your usage in the
`tidyhte` channel. Keep in mind that everyone is volunteering their time
to help, so try to come prepared with the debugging steps you’ve already
taken.

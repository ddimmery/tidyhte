---
title: 'tidyhte: Tidy Estimation of Heterogeneous Treatment Effects in R'
tags:
  - R
  - causal inference
  - heterogeneous treatment effects
  - machine learning
  - experimental design
authors:
  - name: Drew Dimmery
    orcid: 0000-0001-9602-6325
    affiliation: 1
  - name: Edward Kennedy
    orcid: 0000-0002-0227-158X
    affiliation: 2
affiliations:
 - name: Hertie School Data Science Lab
   index: 1
   ror: 0473a4773
 - name: Department of Statistics and Data Science, Carnegie Mellon University, USA
   index: 2
   ror: 05x2bcf33
date: 4 October 2025
bibliography: paper.bib
---

# Summary

Heterogeneous treatment effects (HTE) describe how intervention impacts vary across individuals or subgroups. Understanding this variation is crucial for targeting interventions, optimizing resource allocation, and identifying mechanisms of action. `tidyhte` provides a principled framework for estimating heterogeneous treatment effects using modern machine learning methods. The package implements the doubly-robust learner of @kennedy2023towards, combining outcome modeling and propensity score estimation with cross-validation to produce valid statistical inference on treatment effect heterogeneity.

The package uses a "recipe" design that scales naturally from single to multiple outcomes and moderators. Users specify machine learning algorithms for nuisance function estimation (treatment propensity and outcome models) and define moderators of interest; `tidyhte` handles cross-validation, model selection, diagnostics, and construction of quantities of interest. The tidy design integrates seamlessly with the broader R data science ecosystem and supports common empirical practices including weighting for population average effects and clustered treatment assignment.

# Statement of Need

Existing tools for heterogeneous treatment effect estimation often require substantial statistical expertise and involve navigating complex decisions about cross-validation, model selection, doubly-robust estimation, and valid inference. Implementations are scattered across packages with inconsistent interfaces, making reliable application difficult.

`tidyhte` addresses these challenges by providing a unified interface that: (1) implements state-of-the-art doubly-robust methods with automatic cross-validation, (2) uses intuitive "recipe" semantics familiar from packages like `recipes` [@recipes_package] and `parsnip` [@parsnip_package], (3) handles both experimental and observational data, (4) scales from single to multiple outcomes and moderators, (5) provides built-in diagnostics for model quality and effect heterogeneity, and (6) returns tidy data formats for downstream analysis. By automating technical details while maintaining statistical rigor, the package makes modern HTE methods accessible to applied researchers who need to understand treatment effect variation.

# Research Applications

`tidyhte` supports research across clinical trials (identifying patient subgroups), policy evaluation (understanding differential population impacts), technology (optimizing interventions for user segments), economics (studying policy effects across demographics), and education (evaluating differential intervention impacts). The package was stress-tested as the HTE estimation software for publications on Facebook and Instagram's effects on the 2020 US Presidential election, involving estimation across approximately ten moderators and dozens of outcomes [@guess2023social; @nyhan2023like; @guess2023reshares].

# Related Work

The package builds on the theoretical framework of @kennedy2023towards for doubly-robust HTE estimation. It leverages the SuperLearner ensemble learning framework [@van2007super] for flexible machine learning. The tidy design philosophy follows @wickham2014tidy and integrates with the broader tidyverse ecosystem [@wickham2019welcome].

Other R packages for HTE estimation include `grf` [@grf_package; @grf] for generalized random forests, `bcf` [@bcf_package; @bcf] for Bayesian causal forests, and `FindIt` [@FindIt_package; @imai2013estimating]. `tidyhte` differentiates itself through its doubly-robust methodology [@kennedy2023towards], recipe interface, support for scaling to multiple outcomes and moderators, and native handling of weights and clustering.

# Acknowledgements

We gratefully acknowledge the collaboration with the US 2020 Facebook and Instagram Election Study team for being a testbed for the initial versions of `tidyhte`, particularly Pablo Barberá for testing this package during development and providing valuable feedback on the design and functionality. We received no financial support for this software project.

# References
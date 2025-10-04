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
affiliations:
 - name: Hertie School Data Science Lab
   index: 1
   ror: 0473a4773
date: 2 October 2025
bibliography: paper.bib
---

# Summary

Heterogeneous treatment effects (HTE) describe how the impact of an intervention varies across individuals or subgroups. Understanding this variation is crucial for targeting interventions, optimizing resource allocation, and gaining insights into mechanisms of action. `tidyhte` is an R package that provides a principled, user-friendly framework for estimating heterogeneous treatment effects using modern machine learning methods. The package implements the doubly-robust learner of @kennedy2023towards, which combines outcome modeling and propensity score estimation with cross-validation to produce valid statistical inference on treatment effect heterogeneity.

The package uses a "recipe" design that makes it straightforward to scale analyses from single outcomes and moderators to many outcomes and moderators. Users specify machine learning algorithms for nuisance function estimation (treatment propensity and outcome models), define moderators of interest, and `tidyhte` handles cross-validation, model selection, diagnostics, and construction of relevant quantities of interest. The tidy data principles make it easy to chain operations and integrate with the broader R data science ecosystem.

# Statement of Need

While heterogeneous treatment effect estimation has become increasingly important in applied research, existing tools often require substantial statistical expertise to implement correctly. Researchers must navigate complex decisions about cross-validation strategies, model selection, doubly-robust estimation, and valid inference. Many implementations are scattered across different packages with inconsistent interfaces, making it difficult to apply these methods reliably and at scale.

`tidyhte` addresses these challenges by providing a unified interface for HTE estimation that:

1. Implements state-of-the-art doubly-robust methods with automatic cross-validation
2. Uses intuitive "recipe" semantics familiar to R users from packages like `recipes` and `parsnip`
3. Handles both experimental and observational data
4. Scales easily from single to multiple outcomes and moderators
5. Provides built-in diagnostics for model quality and effect heterogeneity
6. Returns results in tidy data formats for easy downstream analysis and visualization

The package is designed for applied researchers in social sciences, policy evaluation, medicine, and technology companies who need to understand treatment effect variation but may not be experts in the underlying statistical methods. By automating many technical details while maintaining statistical rigor, `tidyhte` makes modern HTE methods accessible to a broader audience.

# Research Applications

`tidyhte` has been designed to support research in fields where understanding treatment effect heterogeneity is important:

- **Clinical trials**: Identifying patient subgroups that benefit most from treatments
- **Policy evaluation**: Understanding which populations are most affected by policy interventions
- **Technology**: Optimizing product interventions for different user segments
- **Economics**: Studying heterogeneous effects of economic policies across demographics
- **Education**: Evaluating differential impacts of educational interventions

It was created specifically as the HTE estimation software for use in the high-profile series of publications on the effects of Facebook and Instagram on the US 2020 Presidential election and was stress tested for this application which involved estimation of HTEs on around ten moderators across dozens of outcomes [@guess2023social; @nyhan2023like; @guess2023reshares].

# Related Work

The package builds on the theoretical framework of @kennedy2023towards for doubly-robust HTE estimation. It leverages the SuperLearner ensemble learning framework [@van2007super] for flexible machine learning. The tidy design philosophy follows @wickham2014tidy and integrates with the broader tidyverse ecosystem [@wickham2019welcome].

Other R packages for HTE estimation include `grf` [@grf_package; @grf] for generalized random forests, `bcf` [@bcf_package; @bcf] for Bayesian causal forests, and `FindIt` [@FindIt_package; @imai2013estimating] for treatment effect heterogeneity. `tidyhte` differentiates itself through its doubly-robust methodology [@kennedy2023towards], recipe interface, and focus on scaling to multiple outcomes and moderators.

# Acknowledgements

We thank Pablo Barberá for testing this package during development and providing valuable feedback on the design and functionality. We also acknowledge the members of the US 2020 Facebook and Instagram Election Study team for serving as the testbed for this software in a large-scale research application.

# References
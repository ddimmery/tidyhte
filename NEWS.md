# tidyhte (dev)

* Add methods to configs which allow the addition of models and moderators after the initial instantiation. This is a first step towards the eventual recipe API.

# tidyhte 0.1.5

* Fixes to VIMP. Arguments to `cv_vim` were previously constructed incorrectly.

* Allows users to specify whether VIMP uses sample splitting (and is therefore has inference robust to zero variable importance) or not (lower variance of estimates). The default is to use sample splitting.

# tidyhte 0.1.4

* Export `attach_config`.

* Clean up vignette a little.

* Add support for different pseudo-outcomes, for example based on an IPW (only), or direct-estimation (only) in addition to the (default) doubly-robust method.

# tidyhte 0.1.2

* Fix error when using `glmnet` as a model in the VIMP regression by setting the (unused) censoring model to be `SL.mean` (a constant model, which is fast and easy to estimate).

# tidyhte 0.1.1

* Greatly increase speed of calculating VIMP.

# tidyhte 0.1.0

* Increment to first minor version number. The package is generally feature-complete enough for general usage, although there are still lots of things to do before an initial major version release.

* Public API no longer constantly passes in the `HTE_cfg`. Instead, a new public function is added, `attach_config`, which adds metadata to a given dataframe expressing how HTEs should be calculated.

* Listwise deletion no longer fully drops rows from the dataset. It instead will drop rows at each step based on the columns that are necessary to be non-missing in that particular step.

* `soft_require` now uses the underlying `rlang::check_installed` to also prompt the end-user to install necessary pacakges.

## Population weights

Adds support for population weights. This is a large change affecting a lot of functionality.

* Discrete MCATEs now support population weights.

* Continuous MCATEs will thrown an error indicating that `nprobust` does not support weights, currently. A subsequent release will provide other ways to smooth a final estimate with, e.g., binscatter methods.

* `calculate_ate` now returns both a SATE and (when weights are given) a PATE.

* Diagnostics now support weighting and will return the population version of themselves (e.g. AUC / MSE). This entailed switching AUC calculation from the `pROC` package to `WeightedROC`.

* An additional suite of tests have been added to check that weighted analyses work correctly and have at least some modicum of correctness.

# tidyhte 0.0.0.13

* Add tests for `check_identifier`, splitting multiple times on a dataset, `soft_require`, output of `SLPredictor`.

* Test for correctness of clustered standard error estimates (against `estimatr::lm_robust`).

* Force progress bar to print even if the stream doesn't seem to support it. This may cause some log-spew when used in batch mode, but will at least be informative about the place in the fitting process.

# tidyhte 0.0.0.12

* Add tests for VIMP, `listwise_deletion`

* Add validity checking for the identifier column.

* Fix R CMD check issue with VIMP not having SuperLearner attached.

* Fix VIMP issue from using `make_splits` after a `.split_id` column already exists in the data.

# tidyhte 0.0.0.11

* Add sorting to API Reference web page

* Add NEWS.md

* A small selection of stylistic changes to remove lint messages.

* Bandwidth selection in kernel smoothing for PCATEs now uses the appropriate
evaluation points.

* Adds a message to the user when a lack of `quickblock` installed causes reversion to
un-stratified CV.

# tidyhte 0.0.0.10

* This is the first release of `tidyhte` documented in NEWS.md

## Bug Fixes

* Lots of them

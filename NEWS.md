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

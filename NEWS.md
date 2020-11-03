# stressaddition 3.1.0

* Improved the documentation of the `which` argument in `plot_survival()` and `plot_stress()`.
* Changed the default value of the `which` argument in the plot functions. Now it contains the proper default curve names. If it is `NA` only the axes and labels will get drawn.


# stressaddition 3.0.3

* Added references to the journal article about the Multi-Tox model which was recently published.


# stressaddition 3.0.2

* Fixed a bug where the plotting functions printed `NULL` to the console.
* Changed maintainer e-mail address to ensure long-term maintainability.
* Internal improvements related to package testing.


# stressaddition 3.0.1

* Added missing return values in documentation to comply with CRAN repository policy.
* Added a link to the paper in the description file.


# stressaddition 3.0.0

## Breaking changes
* Renamed all instances of "effect" to "survival".
* Renamed all instances of "ec" to "lc".
* Renamed `predict_mixture()`, which was a temporary development name, to `multi_tox()`.
* The argument `proportion_ca` in the mixture model `multi_tox()` was renamed and its value reversed. It is now called `sa_contribution` and specifies the proportion of stress addition in the calculation of toxicant stress. To convert your code from the old version use this equation: `sa_contribution = 1 - proportion_ca`.
* Renamed `stress_tox_sam` to `stress_tox_sa` in the output of `multi_tox()`.

## Bugfixes
* Fixed a bug where `plot_stress()` with argument `which = NULL` would result in an error. Now it correctly draws the axes without data.
* Fixed some errors in the documentation and improved the examples.

## New
* Exported function `log10_ticks()` for calculating tick mark labels and positions on a base 10 logarithmic axis.
* Added example data set `multiple_stress` for use with `multi_tox()`.
* Various minor changes to prepare for CRAN submission.


# stressaddition 2.7.0

* Fixed some spelling mistakes.
* `predict_mixture()` now also returns the various stresses.


# stressaddition 2.6.0

* The `curves` data frame in the output of `ecxsys()` now contains a column with the concentrations which are used for the plot functions in this package. This is useful for generating a nicer concentration axis.
* Changes to `ec()`:
    * Renamed `response_value` to `effect` in the output list.
    * `response_level` of 0 or 100 is now allowed. 0 returns the concentration 0 and 100 returns the concentration `Inf`. Previously this resulted in an error.
    * It is now possible to set the reference to a custom value, for example 100.


# stressaddition 2.5.0

* Fixed unintended behaviour in `plot_effect()` and `plot_stress()` where supplying an empty vector caused the four standard curves to show. Now setting `which` to an empty vector or `NULL` shows just the axes. The default value is NA.
* Renamed the `mixture_effect` column in the `predict_mixture` output data frame to `effect`.
* Remove the restriction that the concentration vectors in `predict_mixture()` must be the same length. The longer length must be a multiple of the shorter length because the shorter vector gets recycled to the longer length.


# stressaddition 2.4.0

* Improved `plot_effect()` and `plot_stress()`. You can now control whether the observed values (the points) should be plotted using the `which` argument.
* Renamed `sys_tox_not_fitted` and `sys_tox_env_not_fitted` to `sys_tox_observed` and `sys_tox_env_observed` in the output of `ecxsys()`.


# stressaddition 2.3.0

* `predict_mixture()` now accepts multiple values for the concentration of the second toxicant. Both concentration vectors must be the same length.
* `predict_mixture()` now returns a data frame with the concentrations and effects. Previously it was only a vector of effects.
* `predict_mixture()` received a new argument "effect_max" which scales the returned effect values.
* Renamed the arguments of `predict_mixture()` to use underscore letters a and b instad of 1 and 2. For example model_1 is now model_a.


# stressaddition 2.2.1

* Improved documentation of `predict_mixture()` and included example of symmetry.


# stressaddition 2.2.0

* `ec()` now raises an error if the curve does not cross the desired response level.
* `ecxsys()` gained a new argument `curves_concentration_max` which allows setting the maximum concentration of the predicted curves.


# stressaddition 2.1.1

* Restore the default behaviour of `plot_effect()` to also show `effect_tox` and `effect_tox_env`.


# stressaddition 2.1.0

* The functions `plot_effect()` and `plot_stress()` gained a `which` argument that controls which curves are plotted. Consequently, the `show_LL5_model` argument of `plot_effect()` was removed.
* Added arguments `xlab` and `ylab` to `plot_stress`.
* Added argument `main` to both plot functions.
* Changed some colors of the stress curves so they better match with the colors of related effect curves.
* Added `predict_mixture()` for the prediction of the effects of mixtures of two toxicants.
* Fixed documentation of `ecxsys()` and `predict_ecxsys()`.


# stressaddition 2.0.0

* Changed the order of arguments in `ecxsys()`.
* Removed `hormesis_index` argument from `ecxsys()`. Use `hormesis_concentration` instead.
* New function `predict_ecxsys()` replaces `fn()` from the `ecxsys()` output.
* Renamed the arguments in `ec()`.
* Made `ec()` more flexible. It now also accepts a data.frame with a concentration column and a column of response values.
* Added LL5 curves to the legend of `plot_effect()`.
* Replaced every occurrence of "simple" in variable names with "LL5".
* Replaced every occurrence of "sys_stress" in variable names with "sys" because the extra "stress" was redundant.
* Renamed `plot_system_stress()` to `plot_stress()` because it is planned to plot more stresses with this function in a future update.
* Changed the order of the columns in the output of `predict_ecxsys()`.
* Improved the internal structure of the package.
* Improved the tests.
* Improved the documentation.


# stressaddition 1.11.1

* First public version.
* Added a `NEWS.md` file to track changes to the package.

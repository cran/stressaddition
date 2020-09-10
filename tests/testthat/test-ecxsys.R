# Copyright (C) 2020  Helmholtz-Zentrum fuer Umweltforschung GmbH - UFZ
# See file inst/COPYRIGHTS for details.
#
# This file is part of the R package stressaddition.
#
# stressaddition is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


model <- ecxsys(
    concentration = c(0, 0.05, 0.5, 5, 30),
    hormesis_concentration = 0.5,
    survival_tox_observed = c(90, 81, 92, 28, 0),
    survival_tox_env_observed = c(29, 27, 33, 5, 0)
)


test_that("error when hormesis_concentration not in concentration", {
    errorm <- "hormesis_concentration must equal one of the concentration values."
    expect_error(
        ecxsys(
            concentration = c(0, 0.05, 0.5, 5, 30),
            hormesis_concentration = 0.4,
            survival_tox_observed = c(90, 81, 92, 28, 0)
        ),
        errorm,
        fixed = TRUE
    )
    expect_error(
        ecxsys(
            concentration = c(0, 0.05, 0.5, 5, 30),
            hormesis_concentration = 20,
            survival_tox_observed = c(90, 81, 92, 28, 0)
        ),
        errorm,
        fixed = TRUE
    )
})


test_that("error when hormesis_index <= 2 or >= (length(concentration))", {
    errorm <- paste(
        "hormesis_concentration must be greater than the lowest",
        "non-control concentration and less than the highest concentration"
    )
    expect_error(
        ecxsys(
            concentration = c(0, 0.05, 0.5, 5, 30),
            hormesis_concentration = 0,
            survival_tox_observed = c(85, 76, 94, 35, 0)
        ),
        errorm,
        fixed = TRUE
    )
    expect_error(
        ecxsys(
            concentration = c(0, 0.05, 0.5, 5, 30),
            hormesis_concentration = 0.05,
            survival_tox_observed = c(85, 76, 94, 35, 0)
        ),
        errorm,
        fixed = TRUE
    )
    expect_error(
        ecxsys(
            concentration = c(0, 0.05, 0.5, 5, 30),
            hormesis_concentration = 30,
            survival_tox_observed = c(85, 76, 94, 35, 0)
        ),
        errorm,
        fixed = TRUE
    )
})


test_that("min(concentration) == 0 is shifted the correct amount", {
    expect_equal(model$curves$concentration[1] * 10^5, 0.0001)
})


test_that("the discrete results have not changed", {
    expect_equal(
        round(model$survival_tox_LL5, 3),
        c(90, 89.745, 82.340, 26.787, 4.307)
    )
    expect_equal(
        round(model$survival_tox, 3),
        c(100, 99.455, 92, 28, 0.002)
    )
    expect_equal(
        round(model$stress_tox, 3),
        c(0, 0.093, 0.234, 0.618, 0.984)
    )
    expect_equal(
        round(model$sys_tox_observed, 3),
        c(0.254, 0.232, 0, 0, 0)
    )
    expect_equal(
        round(model$survival_tox_sys, 3),
        c(89.921, 81.890, 90.864, 28, 0.002)
    )
    expect_equal(round(model$stress_env, 3), 0.356)
    expect_equal(
        round(model$survival_tox_env_LL5, 3),
        c(29.667, 29.667, 29.521, 5.408, 0.787)
    )
    expect_equal(
        round(model$stress_tox_env, 3),
        c(0.356, 0.449, 0.59, 0.974, 1.339)
    )
    expect_equal(
        round(model$survival_tox_env, 3),
        c(76.366, 59.902, 33, 0.011, 0)
    )
    expect_equal(
        round(model$sys_tox_env_observed, 3),
        c(0.257, 0.175, 0, 0, 0)
    )
    expect_equal(
        round(model$survival_tox_env_sys, 3),
        c(29.376, 25.999, 30.167, 0.011, 0)
    )
})


test_that("the curves have not changed", {
    new_curves <- model$curves[c(1, 714, 810, 905, 1000), ]  # random indices
    new_curves <- round(new_curves, 3)
    rownames(new_curves) <- NULL
    reference_curves <- data.frame(
        concentration = c(0, 0.03, 0.305, 3.026, 30),
        survival_tox_LL5 = c(90, 89.882, 86.186, 40.426, 4.307),
        survival_tox_env_LL5 = c(29.667, 29.667, 29.655, 9.276, 0.787),
        survival_tox = c(100, 99.702, 95.459, 49.542, 0.002),
        stress_tox = c(0, 0.076, 0.191, 0.502, 0.984),
        sys_tox = c(0.255, 0.24, 0.057, 0, 0),
        stress_tox_sys = c(0.255, 0.316, 0.248, 0.502, 0.984),
        survival_tox_sys = c(89.907, 82.279, 90.675, 49.542, 0.002),
        stress_env = c(0.356, 0.356, 0.356, 0.356, 0.356),
        stress_tox_env = c(0.356, 0.432, 0.547, 0.858, 1.339),
        survival_tox_env = c(76.345, 63.034, 41.017, 1.932, 0),
        sys_tox_env = c(0.254, 0.205, 0.044, 0, 0),
        stress_tox_env_sys = c(0.61, 0.637, 0.591, 0.858, 1.339),
        survival_tox_env_sys = c(29.354, 24.841, 32.754, 1.932, 0),
        concentration_for_plots = c(0.000, 0.03, 0.305, 3.026, 30)
    )
    class(new_curves) <- class(reference_curves)
    expect_equal(new_curves, reference_curves)
})


test_that("the returned fn works the same way as internally", {
    # I don't know why it would fail but it doesn't hurt to test it.
    curves <- model$curves
    curves$concentration_for_plots <- NULL
    expect_identical(curves, predict_ecxsys(model, curves$concentration))
})


test_that("function arguments are returned unchanged", {
    args_reference <- list(
        concentration = c(0, 0.05, 0.5, 5, 30),
        hormesis_concentration = 0.5,
        survival_tox_observed = c(90, 81, 92, 28, 0),
        survival_tox_env_observed = c(29, 27, 33, 5, 0),
        survival_max = 100,
        curves_concentration_max = NULL,
        p = 3.2,
        q = 3.2
    )
    expect_identical(args_reference, model$args)
})


ecxsys(
    concentration = c(0, 0.05, 0.5, 5, 30),
    hormesis_concentration = 0.5,
    survival_tox_observed = c(90, 81, 92, 28, 0),
    survival_tox_env_observed = c(29, 27, 33, 5, 0)
)

test_that("results are independent of concentration shift", {
    model_2 <- ecxsys(
        concentration = c(0, 0.05, 0.5, 5, 30) * 2,
        hormesis_concentration = 0.5 * 2,
        survival_tox_observed = c(90, 81, 92, 28, 0),
        survival_tox_env_observed = c(29, 27, 33, 5, 0)
    )
    expect_equal(model$survival_tox_sys, model_2$survival_tox_sys)
    expect_equal(model$survival_tox_env_sys, model_2$survival_tox_env_sys)
    model_10 <- ecxsys(
        concentration = c(0, 0.05, 0.5, 5, 30) * 10,
        hormesis_concentration = 0.5 * 10,
        survival_tox_observed = c(90, 81, 92, 28, 0),
        survival_tox_env_observed = c(29, 27, 33, 5, 0)
    )
    # Concentration shifts by factors other than powers of 10 may affect
    # the result because of the way the zero concentration is "corrected".
    expect_equal(model$curves$survival_tox,
                 model_10$curves$survival_tox)
    expect_equal(model$curves$survival_tox_sys,
                 model_10$curves$survival_tox_sys)
    expect_equal(model$curves$survival_tox_env_sys,
                 model_10$curves$survival_tox_env_sys)
})


test_that("survival_tox_env_observed can be left out", {
    model_without_env <- ecxsys(
        concentration = c(0, 0.05, 0.5, 5, 30),
        hormesis_concentration = 0.5,
        survival_tox_observed = c(90, 81, 92, 28, 0)
    )
    expect_equal(model$survival_tox_sys, model_without_env$survival_tox_sys)
    expect_equal(model$curves$survival_tox_sys,
                 model_without_env$curves$survival_tox_sys)
})


test_that("sys model not converging produces a warning, not an error", {
    expect_warning(
        ecxsys(
            concentration = c(0, 0.1, 0.5, 1, 10, 33),
            hormesis_concentration = 0.5,
            survival_tox_observed = c(98, 98, 96, 76, 26, 0)
        ),
        paste(
            "Using a horizontal linear model for sys_tox_mod because the",
            "Weibull model did not converge."
        ),
        fixed = TRUE
    )
    expect_warning(
        ecxsys(
            concentration = c(0, 0.1, 0.5, 1, 10, 33),
            hormesis_concentration = 0.5,
            survival_tox_observed = c(90, 81, 92, 50, 18, 0),
            survival_tox_env_observed = c(75, 89, 54, 7, 0, 0)
        ),
        paste(
            "Using a horizontal linear model for sys_tox_env_mod because the",
            "Weibull model did not converge."
        ),
        fixed = TRUE
    )
})


test_that("error when curves_concentration_max is too low", {
    expect_error(
        ecxsys(
            concentration = c(0, 0.05, 0.5, 5, 30),
            hormesis_concentration = 0.5,
            survival_tox_observed = c(90, 81, 92, 28, 0),
            curves_concentration_max = 0.01
        ),
        "'curves_concentration_max' is too low.",
        fixed = TRUE
    )
})

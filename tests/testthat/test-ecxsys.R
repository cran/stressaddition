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


mod <- ecxsys(
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
    expect_equal(mod$curves$concentration[1] * 10^5, 0.0001)
})


test_that("the discrete results have not changed", {
    expect_equal(
        mod$survival_tox_LL5,
        c(90, 89.745092, 82.340325, 26.787104, 4.306868),
        tolerance = 1e-4
    )
    expect_equal(
        mod$survival_tox,
        c(100, 99.455327884, 92.000028403, 27.999995346, 0.002452598),
        tolerance = 1e-4
    )
    expect_equal(
        mod$stress_tox,
        c(0, 0.09296422, 0.23401450, 0.61804415, 0.98352101),
        tolerance = 1e-4
    )
    expect_equal(
        mod$sys_tox_observed,
        c(0.2541156, 0.2324304, 0, 0, 0),
        tolerance = 1e-4
    )
    expect_equal(
        mod$survival_tox_sys,
        c(89.920669034, 81.890300846, 90.863799559, 27.999995346, 0.002452598),
        tolerance = 1e-4
    )
    expect_equal(mod$stress_env, 0.3556369, tolerance = 1e-4)
    expect_equal(
        mod$survival_tox_env_LL5,
        c(29.6666667, 29.6666657, 29.5214959, 5.4076411, 0.7871201),
        tolerance = 1e-4
    )
    expect_equal(
        mod$stress_tox_env,
        c(0.3556369, 0.4486011, 0.5896514, 0.9736810, 1.3391579),
        tolerance = 1e-4
    )
    expect_equal(
        mod$survival_tox_env,
        c(76.36558967, 59.90195211, 33, 0.01079038, 0),
        tolerance = 1e-4
    )
    expect_equal(
        mod$sys_tox_env_observed,
        c(0.2566005, 0.1753250, 0, 0, 0),
        tolerance = 1e-4
    )
    expect_equal(
        mod$survival_tox_env_sys,
        c(29.37633973, 25.99861674, 30.16676004, 0.01079038, 0),
        tolerance = 1e-4
    )
})


test_that("the curves have not changed", {
    new_curves <- mod$curves[c(1, 714, 810, 905, 1000), ]  # random indices
    rownames(new_curves) <- NULL
    reference_curves <- data.frame(
        concentration = c(0.00000, 0.03004, 0.30512, 3.02551, 30.00000),
        survival_tox_LL5 = c(90.00000, 89.88245, 86.18591, 40.42560, 4.30687),
        survival_tox_env_LL5 = c(29.66667, 29.66667, 29.65530, 9.27616, 0.78712),
        survival_tox = c(100.00000, 99.70167, 95.45944, 49.54173, 0.00245),
        stress_tox = c(0.00013, 0.07631, 0.19093, 0.50236, 0.98352),
        sys_tox = c(0.25487, 0.24017, 0.05667, 0.00000, 0.00000),
        stress_tox_sys = c(0.25499, 0.31648, 0.24760, 0.50236, 0.98352),
        survival_tox_sys = c(89.90735, 82.27932, 90.67515, 49.54173, 0.00245),
        stress_env = c(0.35564, 0.35564, 0.35564, 0.35564, 0.35564),
        stress_tox_env = c(0.35576, 0.43195, 0.54657, 0.85800, 1.33916),
        survival_tox_env = c(76.34546, 63.03379, 41.01651, 1.93183, 0.00000),
        sys_tox_env = c(0.25443, 0.20496, 0.04445, 0.00000, 0.00000),
        stress_tox_env_sys = c(0.61020, 0.63691, 0.59102, 0.85800, 1.33916),
        survival_tox_env_sys = c(29.35449, 24.84147, 32.75385, 1.93182, 0),
        concentration_for_plots = c(0.0001, 0.03004, 0.30512, 3.02551, 30)
    )
    class(new_curves) <- class(reference_curves)
    expect_equal(new_curves, reference_curves, tolerance = 1e-3)
})


test_that("the returned fn works the same way as internally", {
    # I don't know why it would fail but it doesn't hurt to test it.
    curves <- mod$curves
    curves$concentration_for_plots <- NULL
    expect_identical(curves, predict_ecxsys(mod, curves$concentration))
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
    expect_identical(args_reference, mod$args)
})


ecxsys(
    concentration = c(0, 0.05, 0.5, 5, 30),
    hormesis_concentration = 0.5,
    survival_tox_observed = c(90, 81, 92, 28, 0),
    survival_tox_env_observed = c(29, 27, 33, 5, 0)
)

test_that("results are independent of concentration shift", {
    mod_2 <- ecxsys(
        concentration = c(0, 0.05, 0.5, 5, 30) * 2,
        hormesis_concentration = 0.5 * 2,
        survival_tox_observed = c(90, 81, 92, 28, 0),
        survival_tox_env_observed = c(29, 27, 33, 5, 0)
    )
    expect_equal(mod$survival_tox_sys, mod_2$survival_tox_sys)
    expect_equal(mod$survival_tox_env_sys, mod_2$survival_tox_env_sys)
    mod_10 <- ecxsys(
        concentration = c(0, 0.05, 0.5, 5, 30) * 10,
        hormesis_concentration = 0.5 * 10,
        survival_tox_observed = c(90, 81, 92, 28, 0),
        survival_tox_env_observed = c(29, 27, 33, 5, 0)
    )
    # Concentration shifts by factors other than powers of 10 may affect
    # the result because of the way the zero concentration is "corrected".
    expect_equal(mod$curves$survival_tox,
                 mod_10$curves$survival_tox)
    expect_equal(mod$curves$survival_tox_sys,
                 mod_10$curves$survival_tox_sys)
    expect_equal(mod$curves$survival_tox_env_sys,
                 mod_10$curves$survival_tox_env_sys)
})


test_that("survival_tox_env_observed can be left out", {
    mod_without_env <- ecxsys(
        concentration = c(0, 0.05, 0.5, 5, 30),
        hormesis_concentration = 0.5,
        survival_tox_observed = c(90, 81, 92, 28, 0)
    )
    expect_equal(mod$survival_tox_sys, mod_without_env$survival_tox_sys)
    expect_equal(mod$curves$survival_tox_sys,
                 mod_without_env$curves$survival_tox_sys)
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

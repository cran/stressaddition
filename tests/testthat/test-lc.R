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


# shared model for some of the tests
model <- ecxsys(
    concentration = c(0, 0.05, 0.5, 5, 30),
    hormesis_concentration = 0.5,
    survival_tox_observed = c(90, 81, 92, 28, 0),
    survival_tox_env_observed = c(29, 27, 33, 5, 0)
)


test_that("all input formats produce identical models", {
    # using the ecxsys() output or the curves therein directly:
    ec10_a <- lc(model, "survival_tox_sys", 10)
    ec10_b <- lc(model$curves, "survival_tox_sys", 10)

    # using the output of predict_ecxsys() with custom concentrations:
    conc <- 10^seq(-9, 1, length.out = 1000)
    curves <- predict_ecxsys(model, conc)
    ec10_c <- lc(curves, "survival_tox_sys", 10)

    # using a custom data frame:
    df_custom <- data.frame(
        concentration = curves$concentration,
        foo = curves$survival_tox_sys
    )
    ec10_d <- lc(df_custom, "foo", 10)

    expect_equal(ec10_a, ec10_b, tolerance = 1e-5)
    expect_equal(ec10_b, ec10_c, tolerance = 1e-5)
    expect_equal(ec10_c, ec10_d, tolerance = 1e-5)
})


test_that("lc values have not changed", {
    expect_equal(
        lc(model, "survival_tox_sys", 50),
        list(response = 44.95368, concentration = 3.375735),
        tolerance = 1e-4
    )
    expect_equal(
        lc(model, "survival_tox_sys", 10),
        list(response = 80.91662, concentration = 1.098648),
        tolerance = 1e-4
    )
    expect_equal(
        lc(model, "survival_tox", 100/3),
        list(response = 66.66667, concentration = 1.902125),
        tolerance = 1e-4
    )
    expect_equal(
        lc(model, "survival_tox_LL5", 42),
        list(response = 52.2, concentration = 2.054426),
        tolerance = 1e-4
    )
    expect_equal(
        lc(model, "survival_tox_env_sys", 50),
        list(response = 14.67725, concentration = 1.299516),
        tolerance = 1e-4
    )
    expect_equal(
        lc(model, "survival_tox_env_sys", 10),
        list(response = 26.41904, concentration = 0.0008571244),
        tolerance = 1e-4
    )
    expect_equal(
        lc(model, "survival_tox_env", 67.89),
        list(response = 24.51453, concentration = 0.7890892),
        tolerance = 1e-4
    )
    expect_equal(
        lc(model, "survival_tox_env_LL5", 3.14159),
        list(response = 28.73466, concentration = 0.7267524),
        tolerance = 1e-4
    )
})


test_that("lc warns when response_level is outside the range of the curve", {
    model <- ecxsys(
        concentration = c(0, 0.05, 0.5, 5),
        hormesis_concentration = 0.5,
        survival_tox_observed = c(90, 81, 92, 28)
    )
    expect_warning(
        lc(model, "survival_tox_sys", 90),
        "You could try using predict_ecxsys() to predict more values",
        fixed = TRUE
    )
})


test_that("reference argument works", {
    expect_equal(
        lc(model, "survival_tox_LL5", 50, reference = 100),
        list(response = 50, concentration = 2.208119),
        tolerance = 1e-4
    )
    expect_equal(
        lc(model, "survival_tox_LL5", 50, reference = 75),
        list(response = 37.5, concentration = 3.342715),
        tolerance = 1e-4
    )
})

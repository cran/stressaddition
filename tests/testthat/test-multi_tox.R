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


model_a <- ecxsys(
    concentration = c(0, 0.05, 0.5, 5, 30),
    hormesis_concentration = 0.5,
    survival_tox_observed = c(90, 81, 92, 28, 0),
    survival_tox_env_observed = c(29, 27, 33, 5, 0),
    survival_max = 101
)
model_b <- ecxsys(
    concentration = c(0, 0.1, 1, 10, 100, 1000),
    hormesis_concentration = 10,
    survival_tox_observed = c(26, 25, 24, 27, 5, 0),
    survival_max = 30
)


test_that("results have not changed", {
    # one concentration_b
    new <- multi_tox(
        model_a,
        model_b,
        c(0, 0.01, 0.1, 1, 7, 15),
        5,
        0.7
    )
    new <- round(new, 3)
    reference <- data.frame(
        concentration_a = c(0, 0.01, 0.1, 1, 7, 15),
        concentration_b = 5,
        survival = c(88.575, 84.361, 80.632, 56.731, 2.883, 0),
        stress_tox_sa = c(0.189, 0.244, 0.319, 0.511, 0.89, 1.07),
        stress_tox_ca = c(0.189, 0.192, 0.216, 0.358, 0.713, 0.886),
        stress_tox = c(0.189, 0.228, 0.288, 0.465, 0.837, 1.015),
        sys = c(0.078, 0.073, 0.04, 0, 0, 0),
        stress_total = c(0.267, 0.301, 0.328, 0.465, 0.837, 1.015)
    )
    expect_equal(new, reference)

    # diverse concentration_b
    new <- multi_tox(
        model_a,
        model_b,
        c(0, 0.01, 0.1, 1, 7, 15),
        c(0, 0.02, 0.2, 2, 14, 30),
        0.7
    )
    new <- round(new, 3)
    reference <- data.frame(
        concentration_a = c(0, 0.01, 0.1, 1, 7, 15),
        concentration_b = c(0, 0.02, 0.2, 2, 14, 30),
        survival = c(88.270, 79.962, 78.157, 65.8, 0.386, 0),
        stress_tox_sa = c(0, 0.076, 0.181, 0.451, 0.996, 1.294),
        stress_tox_ca = c(0, 0.057, 0.134, 0.337, 0.732, 0.906),
        stress_tox = c(0, 0.07, 0.167, 0.417, 0.917, 1.178),
        sys = c(0.27, 0.262, 0.177, 0, 0, 0),
        stress_total = c(0.27, 0.332, 0.344, 0.417, 0.917, 1.178)
    )
    expect_equal(new, reference)

    # diverse concentration_b and custom survival_max
    new <- multi_tox(
        model_a,
        model_b,
        c(0, 0.01, 0.1, 1, 7, 15),
        c(0, 0.02, 0.2, 2, 14, 30),
        0.7,
        42
    )
    new <- round(new, 3)
    reference <- data.frame(
        concentration_a = c(0, 0.01, 0.1, 1, 7, 15),
        concentration_b = c(0, 0.02, 0.2, 2, 14, 30),
        survival = c(37.073, 33.584, 32.826, 27.636, 0.162, 0),
        stress_tox_sa = c(0, 0.076, 0.181, 0.451, 0.996, 1.294),
        stress_tox_ca = c(0, 0.057, 0.134, 0.337, 0.732, 0.906),
        stress_tox = c(0, 0.07, 0.167, 0.417, 0.917, 1.178),
        sys = c(0.27, 0.262, 0.177, 0, 0, 0),
        stress_total = c(0.27, 0.332, 0.344, 0.417, 0.917, 1.178)
    )
    expect_equal(new, reference)
})


test_that("predictions are symmetric", {
    conc_a <- c(0, 10^seq(log10(0.001), log10(40), length.out = 50))
    conc_b <- 3.5
    sa_contrib <- 0.8
    survival_12 <- multi_tox(model_a, model_b, conc_a, conc_b, sa_contrib)$survival
    survival_21 <- multi_tox(model_b, model_a, conc_b, conc_a, sa_contrib)$survival
    expect_equal(survival_12, survival_21)
})


test_that("survival_tox_mod is a W1.2 model", {
    # This is a safeguard for if I decide to use something else than drc::W1.2
    # for survival_tox_mod. It is extremely unlikely that this ever happens but
    # better safe than sorry. Currently, the inverse only works for W1.2 models.
    # If this test throws errors then I probably forget to adjust the inverse
    # function accordingly.
    expect_s3_class(model_a$survival_tox_mod$fct, "Weibull-1")
    expect_equal(model_a$survival_tox_mod$fct$noParm, 2)
})

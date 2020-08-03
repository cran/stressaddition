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
    reference <- data.frame(
        concentration_a = c(0, 0.01, 0.1, 1, 7, 15),
        concentration_b = 5,
        survival = c(88.574578, 84.361552, 80.633762, 56.730550, 2.882718, 0),
        stress_tox_sa = c(0.1886787, 0.2436883, 0.3185115, 0.5111081, 0.8904295, 1.0700716),
        stress_tox_ca = c(0.18867873, 0.19199327, 0.21634282, 3.579444e-01, 7.126441e-01, 8.856914e-01),
        stress_tox = c(0.18867873, 0.22817978, 0.28786093, 4.651590e-01, 8.370939e-01, 1.014758),
        sys = c(0.07845536, 0.07312129, 0.04004696, 4.663355e-05, 0, 0),
        stress_total = c(0.26713409, 0.30130107, 0.32790789, 4.652056e-01, 8.370939e-01, 1.014758)
    )
    expect_equal(new, reference, tolerance = 1e-5)

    # diverse concentration_b
    new <- multi_tox(
        model_a,
        model_b,
        c(0, 0.01, 0.1, 1, 7, 15),
        c(0, 0.02, 0.2, 2, 14, 30),
        0.7
    )
    reference <- data.frame(
        concentration_a = c(0, 0.01, 0.1, 1, 7, 15),
        concentration_b = c(0, 0.02, 0.2, 2, 14, 30),
        survival = c(88.2698383, 79.96171270, 78.1574808, 6.579998e+01, 3.861678e-01, 0),
        stress_tox_sa = c(0, 0.07567745, 0.1807501, 4.510696e-01, 9.961584e-01, 1.294227),
        stress_tox_ca = c(0, 0.05654834, 0.1343429, 3.367461e-01, 7.321990e-01, 9.063311e-01),
        stress_tox = c(0, 0.06993872, 0.1668279, 4.167725e-01, 9.169706e-01, 1.177858),
        sys = c(0.2698033, 0.26248944, 0.1774490, 1.829786e-04, 0, 0),
        stress_total = c(0.2698033, 0.33242815, 0.3442769, 4.169555e-01, 9.169706e-01, 1.177858)
    )
    expect_equal(new, reference, tolerance = 1e-5)

    # diverse concentration_b and custom survival_max
    new <- multi_tox(
        model_a,
        model_b,
        c(0, 0.01, 0.1, 1, 7, 15),
        c(0, 0.02, 0.2, 2, 14, 30),
        0.7,
        42
    )
    reference <- data.frame(
        concentration_a = c(0, 0.01, 0.1, 1, 7, 15),
        concentration_b = c(0, 0.02, 0.2, 2, 14, 30),
        survival = c(37.0733321, 33.58391933, 32.8261419, 2.763599e+01, 1.621905e-01, 0),
        stress_tox_sa = c(0, 0.07567745, 0.1807501, 4.510696e-01, 9.961584e-01, 1.294227),
        stress_tox_ca = c(0, 0.05654834, 0.1343429, 3.367461e-01, 7.321990e-01, 9.063311e-01),
        stress_tox = c(0, 0.06993872, 0.1668279, 4.167725e-01, 9.169706e-01, 1.177858),
        sys = c(0.2698033, 0.26248944, 0.1774490, 1.829786e-04, 0, 0),
        stress_total = c(0.2698033, 0.33242815, 0.3442769, 4.169555e-01, 9.169706e-01, 1.177858)
    )
    expect_equal(new, reference, tolerance = 1e-5)
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

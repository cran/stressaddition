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


test_that("stress_to_survival handles bad input", {
    expect_equal(stress_to_survival(-3), 1)
    expect_equal(stress_to_survival(10), 0)
    expect_error(stress_to_survival("a"))
    expect_error(stress_to_survival(0.5, -3, 10))
    expect_error(stress_to_survival(0.5, 3, -10))
})

test_that("survival_to_stress handles bad input", {
    expect_equal(survival_to_stress(-15), 1)
    expect_equal(survival_to_stress(20), 0)
    expect_error(survival_to_stress("a"))
    expect_error(survival_to_stress(0.5, -3, 10))
    expect_error(survival_to_stress(0.5, 3, -10))
})

test_that("stress_to_survival is correct", {
    expect_equal(stress_to_survival(0.5), 0.5)
    expect_equal(round(stress_to_survival(0.1), 5), 0.99321)
})

test_that("survival_to_stress is correct", {
    expect_equal(survival_to_stress(0.5), 0.5)
    expect_equal(round(survival_to_stress(0.1), 5), 0.74588)
})

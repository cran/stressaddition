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


test_that("log ticks are correct", {
    x <- c(0.03, 0.3, 3, 30)
    reference <- list(
        major = c(0.01, 0.1, 1, 10, 100),
        minor = c(
            0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09,
            0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9,
            2, 3, 4, 5, 6, 7, 8, 9,
            20, 30, 40, 50, 60, 70, 80, 90
        ),
        major_labels = c("0", "0.1", "1", "10", "100"),
        minor_labels = c(
            "0.02", "0.03", "0.04", "0.05", "0.06", "0.07", "0.08", "0.09",
            "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9",
            "2", "3", "4", "5", "6", "7", "8", "9",
            "20", "30", "40", "50", "60", "70", "80", "90"
        )
    )
    expect_equal(reference, log10_ticks(x))

    reference$major_labels[1] <- "0.01"
    expect_equal(reference, log10_ticks(x, label_zero = FALSE))
})

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


#' Logarithmic axis tick marks
#'
#' Calculate the positions and labels of major and minor tick marks for a base
#' 10 logarithmic axis.
#'
#' @param x A vector of axis values. Can be arbitrarily long but only the
#'   minimum and maximum are necessary.
#' @param label_zero Whether or not to replace the smallest major label with
#'   "0". This defaults to \code{TRUE} and is useful for some types of plots
#'   used to display concentration-response data where the leftmost data point
#'   represents the control.
#'
#' @return A list with the positions and labels of the major and minor tick
#'   marks. The labels are formatted without trailing zeros using
#'   \code{formatC(labels, format = "fg")}.
#'
#' @examples
#' x <- c(0.01, 0.2, 3, 10, 50)
#' plot(x, c(5, 4, 2.5, 1, 0), xaxt = "n", log = "x")
#' ticks <- log10_ticks(x)
#' axis(1, at = ticks$major, labels = ticks$major_labels)
#' axis(1, at = ticks$minor, labels = FALSE, tcl = -0.25)
#'
#' @export
log10_ticks <- function(x, label_zero = TRUE) {
    stopifnot(min(x, na.rm = TRUE) > 0)
    x <- log10(x)
    major <- seq(
        floor(min(x, na.rm = TRUE)),
        ceiling(max(x, na.rm = TRUE))
    )
    major <- 10 ^ major
    n_between <- length(major) - 1
    minor <- integer(n_between * 8)
    for (i in 1:n_between) {
        a <- major[i]
        b <- major[i + 1]
        minor[seq(i * 8 - 7, i * 8)] <- seq(a + a, b - a, a)
    }

    major_labels <- formatC(major, format = "fg")
    if (label_zero) {
        major_labels[1] <- "0"
    }
    minor_labels <- formatC(minor, format = "fg")

    list(
        major = major,
        minor = minor,
        major_labels = major_labels,
        minor_labels = minor_labels
    )
}

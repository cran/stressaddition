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


#' Convert Between Stress and Survival
#'
#' Functions to convert survival to general stress or vice versa using the beta
#' distribution.
#'
#' These are simple wrappers around the beta distribution function
#' \code{\link[stats:Beta]{pbeta}} and the beta quantile function
#' \code{\link[stats:Beta]{qbeta}}.
#'
#' @name Stressconversion
#'
#' @param survival One or more survival values to convert to general stress.
#'   Should be a value between 0 and 1. Smaller or bigger values are treated as
#'   0 or 1 respectively.
#' @param stress One or more stress values to convert to survival.
#' @param p,q The shape parameters of the \code{\link[stats:Beta]{beta}}
#'   distribution. Default is 3.2.
#'
#' @return \code{stress_to_survival} returns a vector the same length as
#'   \code{stress} giving the survival caused by each amount of stress.
#'
#'   \code{survival_to_stress} returns a vector the same length as
#'   \code{survival} containing the stress values associated with each survival.
#'
#' @examples
#' stress <- 0.3
#' survival <- stress_to_survival(stress)
#' survival_to_stress(survival)
#'
NULL


#' @rdname Stressconversion
#' @export
survival_to_stress <- function(survival, p = 3.2, q = 3.2) {
    stopifnot(p >= 0, q >= 0)
    survival <- clamp(survival)
    qbeta(1 - survival, p, q)
}


#' @rdname Stressconversion
#' @export
stress_to_survival <- function(stress, p = 3.2, q = 3.2) {
    stopifnot(p >= 0, q >= 0)
    1 - pbeta(stress, p, q)
}

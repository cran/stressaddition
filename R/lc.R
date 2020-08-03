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


#' Lethal Concentrations
#'
#' Estimate the concentration to reach a certain mortality relative
#' to the control.
#'
#' If the response level occurs multiple times because of hormesis, which may
#' happen for low values of \code{response_level}, then the occurrence with the
#' smallest concentration is returned.
#'
#' This function only makes sense for curves which generally go down with
#' increasing concentration, i.e. all \code{survival_*} curves and also
#' \code{sys_tox} and \code{sys_tox_env}. Others are untested and may give
#' unexpected results, if any.
#'
#' @param model This can be one of three types of objects: Either the output of
#'   \code{\link{ecxsys}} or the output of \code{\link{predict_ecxsys}} or a
#'   data frame with a "concentration" column and a \code{response_name} column.
#'   In the case of the data frame the first row is assumed to be the control.
#'   See the examples.
#' @param response_name The name of the survival or stress for which you want to
#'   calculate the LC.
#' @param response_level The desired response level as a percentage between 0
#'   and 100. For example with the value 10 the function will return the LC10,
#'   i.e. the concentration where the response falls below 90 \% of the control
#'   response. In other words: where a mortality of 10 \% relative to the control
#'   is reached.
#' @param reference The reference value of the response, usually the control at
#'   concentration 0. This argument is optional. If it is missing, the first
#'   value of the response is used as control. This value determines what number
#'   \code{response_level} actually corresponds to. For example if
#'   response_level is 10 and reference is 45, then the function returns the
#'   concentration where the curve is reduced by 10\% relative to 45 = 40.5.
#' @param warn Logical. Should the function emit a warning if the calculation of
#'   the lethal concentration is not possible?
#'
#' @return A list containing the lethal concentration and the corresponding
#'   survival. The survival will be \code{NA} if its calculation is impossible
#'   using the supplied data.
#'
#' @examples # Calculate the LC10, the concentration where the survival falls
#' # below 90% of the survival in the control.
#'
#' model <- ecxsys(
#'     concentration = c(0, 0.05, 0.5, 5, 30),
#'     hormesis_concentration = 0.5,
#'     survival_tox_observed = c(90, 81, 92, 28, 0)
#' )
#'
#' # using the ecxsys() output or the curves therein directly:
#' lc(model, "survival_tox_sys", 10)
#' lc(model$curves, "survival_tox_sys", 10)
#'
#' # using the output of predict_ecxsys() with custom concentrations:
#' conc <- 10^seq(-9, 1, length.out = 1000)
#' curves <- predict_ecxsys(model, conc)
#' lc(curves, "survival_tox_sys", 10)
#'
#' # using a custom data frame:
#' df_custom <- data.frame(
#'     concentration = curves$concentration,
#'     foo = curves$survival_tox_sys
#' )
#' lc(df_custom, "foo", 10)
#'
#' # Calculate the LC50 relative to an survival of 100
#' # instead of relative to the control:
#' lc(model, "survival_tox_sys", 50, reference = 100)
#'
#' @export
lc <- function(model,
               response_name,
               response_level,
               reference,
               warn = TRUE) {
    if (inherits(model, "drc")) {
        stop("Please use drc::ED() for drc objects.")
    }

    stopifnot(
        is.character(response_name),
        length(response_name) == 1,
        response_name != "concentration",
        response_level >= 0,
        response_level <= 100
    )

    if (!inherits(model, c("ecxsys", "ecxsys_predicted"))) {
        if (is.data.frame(model)) {
            concentration <- model$concentration
            response <- model[, response_name]
        } else {
            stop("Invalid first argument.")
        }
    } else if (inherits(model, "ecxsys")) {
        concentration <- model$curves$concentration
        response <- model$curves[, response_name]
    } else if (inherits(model, "ecxsys_predicted")) {
        concentration <- model$concentration
        response <- model[, response_name]
    }

    if (missing(reference)) {
        reference_value <- response[1]
    } else {
        stopifnot(reference > 0)
        if (inherits(model, "ecxsys")) {
            stopifnot(reference <= model$args$survival_max)
        }
        reference_value <- reference
    }

    if (reference_value == 0) {
        # May happen with horizontal Sys curves.
        if (warn) {
            warning("Reference value is zero, calculation of LC is impossible.")
        }
        return(list(response = 0, concentration = NA))
    }

    if (response_level == 0) {
        return(list(response = reference_value, concentration = 0))
    } else if (response_level == 100) {
        return(list(response = 0, concentration = Inf))
    }

    response_value <- (1 - response_level / 100) * reference_value

    response_smaller <- response < response_value
    if (response_smaller[1]) {
        if (warn) {
            warning(
                "The first values of '",
                response_name,
                "' are smaller than ",
                100 - response_level,
                "% of the reference value, which makes determining the LC",
                response_level,
                " impossible."
            )
        }
        return(list(response = response_value, concentration = NA))
    }
    if (!any(response_smaller)) {
        if (warn) {
            warning(
                "The curve '",
                response_name,
                "' does not fall below ",
                100 - response_level,
                "% of the reference, which makes determining the LC",
                response_level,
                " impossible. You could try using predict_ecxsys() to predict ",
                "more values in a wider concentration range."
            )
        }
        return(list(response = response_value, concentration = NA))
    }
    if (response[1] == response_value) {
        return(list(response = response_value, concentration = 0))
    }

    below <- which(response < response_value)[1]
    above <- below - 1

    # linear interpolation
    dist <- (response_value - response[below]) / (response[above] - response[below])
    survival_concentration <- dist *
        (concentration[above] - concentration[below]) + concentration[below]

    list(response = response_value, concentration = survival_concentration)
}

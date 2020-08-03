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


#' Predict survival and stress
#'
#' Calculate the survivals and stresses of an ECx-SyS model at arbitrary
#' concentrations.
#'
#' @param model An ECx-SyS model as returned by \code{\link{ecxsys}}.
#' @param concentration A numeric vector of concentrations.
#'
#' @return A data frame (of class "ecxsys_predicted") with the following
#'   columns:
#'   \describe{
#'     \item{concentration}{The supplied concentrations.}
#'     \item{survival_tox_LL5}{The survival predicted by the five-parameter
#'     log-logistic model derived from the observations under toxicant stress
#'     but without environmental stress.}
#'     \item{survival_tox}{Modeled survival resulting from toxicant stress.}
#'     \item{survival_tox_sys}{Modeled survival resulting from toxicant
#'     and system stress.}
#'     \item{stress_tox}{The toxicant stress.}
#'     \item{sys_tox}{System stress under toxicant stress conditions
#'     without environmental stress.}
#'     \item{stress_tox_sys}{The sum of \code{stress_tox} and
#'     \code{sys_tox}.}
#'     \item{survival_tox_env_LL5}{The survival predicted by the five-parameter
#'     log-logistic model derived from the observations under toxicant stress
#'     with environmental stress.}
#'     \item{survival_tox_env}{Modeled survival resulting from toxicant and
#'     environmental stress.}
#'     \item{survival_tox_env_sys}{Modeled survival resulting from toxicant,
#'     environmental and system stress.}
#'     \item{stress_env}{Environmental stress.}
#'     \item{stress_tox_env}{The sum of toxicant and environmental stress.}
#'     \item{sys_tox_env}{System stress under toxicant and
#'     environmental stress conditions.}
#'     \item{stress_tox_env_sys}{The sum of \code{stress_tox_env} and
#'     \code{sys_tox_env}.}
#'   }
#'
#' @examples model <- ecxsys(
#'     concentration = c(0, 0.05, 0.5, 5, 30),
#'     hormesis_concentration = 0.5,
#'     survival_tox_observed = c(90, 81, 92, 28, 0),
#'     survival_tox_env_observed = c(29, 27, 33, 5, 0)
#' )
#' p <- predict_ecxsys(model, c(0.001, 0.01, 0.1, 1, 10))
#'
#' @export
predict_ecxsys <- function(model, concentration) {
    # This function returns all modeled values at the provided
    # concentrations.
    stopifnot(
        inherits(model, "ecxsys"),
        is.numeric(concentration)
    )
    p <- model$args$p
    q <- model$args$q
    survival_max <- model$args$survival_max
    out_df <- data.frame(
        concentration = concentration
    )

    out_df$survival_tox_LL5 <- predict(
        model$survival_tox_LL5_mod,
        data.frame(concentration = concentration)
    ) * survival_max

    if (model$with_env) {
        out_df$survival_tox_env_LL5 <- predict(
            model$survival_tox_env_LL5_mod,
            data.frame(concentration = concentration)
        ) * survival_max
    }

    survival_tox <- predict(
        model$survival_tox_mod,
        data.frame(concentration = concentration)
    )
    out_df$survival_tox <- survival_tox * survival_max

    stress_tox <- survival_to_stress(survival_tox, p, q)
    out_df$stress_tox <- stress_tox

    sys_tox <- predict(
        model$sys_tox_mod,
        data.frame(stress_tox = stress_tox)
    )
    out_df$sys_tox <- sys_tox

    stress_tox_sys <- stress_tox + sys_tox
    out_df$stress_tox_sys <- stress_tox_sys

    survival_tox_sys <- stress_to_survival(stress_tox_sys, p, q)
    out_df$survival_tox_sys <- survival_tox_sys * survival_max

    if (model$with_env) {
        out_df$stress_env <- model$stress_env

        stress_tox_env <- stress_tox + model$stress_env
        out_df$stress_tox_env <- stress_tox_env

        out_df$survival_tox_env <- stress_to_survival(
            stress_tox_env, p, q
        ) * survival_max

        sys_tox_env <- predict(
            model$sys_tox_env_mod,
            data.frame(stress_tox = stress_tox)
        )
        out_df$sys_tox_env <- sys_tox_env

        stress_tox_env_sys <- stress_tox_env +
            sys_tox_env
        out_df$stress_tox_env_sys <- stress_tox_env_sys

        survival_tox_env_sys <- stress_to_survival(
            stress_tox_env_sys, p, q
        )
        out_df$survival_tox_env_sys <- survival_tox_env_sys * survival_max
    }

    class(out_df) <- c("ecxsys_predicted", class(out_df))
    out_df
}

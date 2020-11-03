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


#' Predict the survival of binary toxicant mixtures
#'
#' The Multi-TOX model predicts the effects of binary toxicant mixtures based on
#' three-phasic concentration-response relationships. See the publication for
#' details.
#'
#' The predictions are symmetric, i.e. it does not matter which of the toxicant
#' models is a or b as long as the concentration arguments are supplied in the
#' same order.
#'
#' This method is only suitable for experiments without or with low
#' environmental stress. Any environmental stress supplied as arguments to
#' \code{\link{ecxsys}} in \code{model_a} or \code{model_b} is ignored.
#'
#' @param model_a,model_b The ecxsys models of the toxicants.
#' @param concentration_a,concentration_b The concentrations of the toxicants in
#'   the mixture. Both vectors must either be the same length or the longer
#'   length must be a multiple of the shorter length. That's because the shorter
#'   concentration vector gets recycled to the length of the longer one.
#' @param sa_contribution The proportion of stress addition contributing to the
#'   calculation of the toxicant stress in the mixture. Must be between 0 and 1
#'   where 1 stands for 100 \% stress addition.
#' @param survival_max Controls the scaling of the result. This represents the
#'   maximum value the survival could possibly reach. For survival data in
#'   percent this should be 100 (the default).
#'
#' @return A data frame with columns of the supplied concentrations and the
#'   corresponding mixture survival and stresses.
#'
#' @examples
#' # Using a data set which is included in this package. See ?multiple_stress
#' ms <- multiple_stress
#' esfen <- ms[ms$food == 1 & ms$prochloraz == 0, ]
#' proch <- ms[ms$food == 1 & ms$esfenvalerate == 0, ]
#'
#' model_esfen <- ecxsys(
#'     concentration = esfen$esfenvalerate,
#'     survival_tox_observed = esfen$survival,
#'     hormesis_concentration = 0.1
#' )
#' model_proch <- ecxsys(
#'     concentration = proch$prochloraz,
#'     survival_tox_observed = proch$survival,
#'     hormesis_concentration = 100
#' )
#'
#' # Predict the survival at 8 different esfenvalerate concentrations
#' # but keep the prochloraz concentration constant at 32:
#' mt <- multi_tox(
#'     model_esfen,
#'     model_proch,
#'     c(0, 0.0001, 0.001, 0.01, 0.1, 0.316, 1, 3.16),
#'     32,
#'     sa_contribution = 0.8
#' )
#' mt[1:3]  # The concentrations and survival of the 8 mixtures.
#'
#' # Predict the survival at 4 different combinations
#' # of esfenvalerate and prochloraz:
#' mt <- multi_tox(
#'     model_esfen,
#'     model_proch,
#'     c(0.1, 0.2, 0.3, 0.4),
#'     c(0, 1, 32, 100),
#'     sa_contribution = 0.8
#' )
#' mt[1:3]  # The concentrations and survival of the 4 mixtures.
#'
#'
#' @references \href{https://doi.org/10.1186/s12302-020-00394-7}{Liess, M.,
#'   Henz, S. & Shahid, N. Modeling the synergistic effects of
#'   toxicant mixtures. Environ Sci Eur 32, 119 (2020).}
#'
#' @export
multi_tox <- function(model_a,
                      model_b,
                      concentration_a,
                      concentration_b,
                      sa_contribution = 0.5,
                      survival_max = 100) {
    stopifnot(
        inherits(model_a, "ecxsys"),
        inherits(model_b, "ecxsys"),
        is.numeric(concentration_a),
        is.numeric(concentration_b),
        length(concentration_a) > 0,
        length(concentration_b) > 0,
        all(!is.na(concentration_a)),
        all(!is.na(concentration_b)),
        sa_contribution >= 0,
        sa_contribution <= 1,
        model_a$args$p == model_b$args$p,
        model_a$args$q == model_b$args$q
    )

    predicted_model_a <- predict_ecxsys(model_a, concentration_a)
    predicted_model_b <- predict_ecxsys(model_b, concentration_b)

    # tox stress ----------------------------------------------------------
    stress_tox_sa <- predicted_model_a$stress_tox + predicted_model_b$stress_tox

    # Convert the model_b concentration into an equivalent model_a concentration
    # and vice versa.
    concentration_b_equivalent <- W1.2_inverse(
        model_a$survival_tox_mod,
        predicted_model_b$survival_tox / model_b$args$survival_max
    )
    survival_tox_ca_a <- predict(
        model_a$survival_tox_mod,
        data.frame(concentration = concentration_a + concentration_b_equivalent)
    )
    stress_tox_ca_a <- survival_to_stress(survival_tox_ca_a)

    concentration_a_equivalent <- W1.2_inverse(
        model_b$survival_tox_mod,
        predicted_model_a$survival_tox / model_a$args$survival_max
    )
    survival_tox_ca_b <- predict(
        model_b$survival_tox_mod,
        data.frame(concentration = concentration_b + concentration_a_equivalent)
    )
    stress_tox_ca_b <- survival_to_stress(survival_tox_ca_b)

    stress_tox_ca <- (stress_tox_ca_a + stress_tox_ca_b) / 2

    # sys -----------------------------------------------------------------
    sys_a <- predict(model_a$sys_tox_mod, data.frame(stress_tox = stress_tox_ca))
    sys_b <- predict(model_b$sys_tox_mod, data.frame(stress_tox = stress_tox_ca))
    sys_total <- (sys_a + sys_b) / 2

    # combined stress and result ------------------------------------------
    ca_contribution <- 1 - sa_contribution
    stress_tox_total <- stress_tox_sa * sa_contribution + stress_tox_ca * ca_contribution
    stress_total <- stress_tox_total + sys_total
    survival <- stress_to_survival(stress_total) * survival_max

    data.frame(
        concentration_a = concentration_a,
        concentration_b = concentration_b,
        survival = survival,
        stress_tox_sa = stress_tox_sa,
        stress_tox_ca = stress_tox_ca,
        stress_tox = stress_tox_total,
        sys = sys_total,
        stress_total = stress_total
    )
}


W1.2_inverse <- function(mod, x) {
    # Using drc::ED() with respLev = 0 does not work, which is why I use
    # this function instead.
    # x = 1 - respLev / 100
    b <- mod$fit$par[1]
    e <- mod$fit$par[2]
    exp(log(-log(x)) / b + log(e))
}

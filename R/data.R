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


# Documentation for all included data sets.


#' Survival of Daphnia magna exposed to multiple stressors
#'
#' A data set of mixture toxicity experiments. Individuals of Daphnia magna were
#' exposed to combinations of food stress and multiple concentrations of the
#' pesticides esfenvalerate and prochloraz. The survival was recorded at 21 days
#' after contamination.
#'
#' @format A data frame with 58 rows and 4 variables:
#' \describe{
#'   \item{food}{The amount of food in percent.}
#'   \item{esfenvalerate}{The concentration of esfenvalerate in µg/L.}
#'   \item{prochloraz}{The concentration of prochloraz in µg/L.}
#'   \item{survival}{The mean survival in percent.}
#' }
#'
#' @source
#'   \href{https://doi.org/10.1021/acs.est.9b04293}{Shahid, N., Liess, M.,
#'     Knillmann, S., 2019. Environmental Stress Increases Synergistic Effects
#'     of Pesticide Mixtures on Daphnia magna. Environ. Sci. Technol. 53,
#'     12586–12593.}
#'
#'   \href{https://doi.org/10.1186/s12302-020-00394-7}{Liess, M., Henz, S. &
#'   Shahid, N. Modeling the synergistic effects of
#'   toxicant mixtures. Environ Sci Eur 32, 119 (2020).}
"multiple_stress"

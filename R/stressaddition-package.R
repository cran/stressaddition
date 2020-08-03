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


# The help file for the package, accessible via ?stressaddition and
# ?`stressaddition-package`. Some people recommend using "@keywords internal"
# here to exclude this page from the help index. But I'm not going to do that
# because I want to keep it on the index.

# "_PACKAGE" automatically populates most sections in this package
# documentation, so I don't need to manually fill in authors, title or
# description.


#' @details This R package contains the definitions of the
#'   \link[=ecxsys]{ECx-SyS} model and the \link[=multi_tox]{Multi-TOX} model.
#'   See the publications linked below for more information including equations.
#'
#'   Author contributions: M. Liess conceived the ECx-SyS and Multi-TOX models.
#'   S. Henz developed this R package. N. Shahid contributed to the optimization
#'   of the Multi-TOX model and provided the \link{multiple_stress} data set.
#'
#' @references \href{https://doi.org/10.1038/s41598-019-51645-4}{Liess, M.,
#'   Henz, S. & Knillmann, S. Predicting low-concentration effects of
#'   pesticides. Sci Rep 9, 15248 (2019).}
#'
#'   Liess, M., Henz, S., Shahid, N., 2020. Modelling the synergistic effects of
#'   toxicant mixtures. Manuscript submitted for publication.
#'
#' @import stats
#' @import graphics
#' @import grDevices
"_PACKAGE"

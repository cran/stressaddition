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


#' @rdname plot_ecxsys
#' @export
plot_stress <- function(model,
                        which = NA,
                        show_legend = FALSE,
                        xlab = "concentration",
                        ylab = "stress",
                        main = NULL) {
    stopifnot(inherits(model, "ecxsys"))

    curve_names <- names(model$curves)
    valid_names <- c(
        curve_names[startsWith(curve_names, "stress") | startsWith(curve_names, "sys")],
        "sys_tox_observed", "sys_tox_env_observed"  # the observed points
    )
    if (length(which) == 1 && is.na(which)) {
        which <- c("sys_tox", "sys_tox_observed")
        if (model$with_env) {
            which <- c(which, "sys_tox_env", "sys_tox_env_observed")
        }
    } else if ("all" %in% which) {
        if (length(which) > 1) {
            stop("'all' must not be combined with other curve names.")
        }
        which <- valid_names
    } else if (!model$with_env && any(grepl("env", which, fixed = TRUE))) {
        warning("'which' contains names with 'env' but the model was built ",
                "without environmental stress.")
        which <- which[which %in% valid_names]
    } else if (any(!which %in% valid_names)) {
        warning("Argument 'which' contains invalid names.")
        which <- which[which %in% valid_names]
    }

    curves <- model$curves
    ticks <- log10_ticks(curves$concentration_for_plots)
    point_concentration <- c(
        curves$concentration_for_plots[1],
        model$args$concentration[-1]
    )

    if (is.null(which)) {
        ymax = 1
    } else {
        which_lines <- which[!endsWith(which, "observed")]
        if (length(which_lines) == 0) {
            ymax <- 1
        } else {
            ymax <- max(curves[, which_lines], 1, na.rm = TRUE)
            # No need to include the observed stress in the call to max() no
            # matter if those are in "which" or not because these vectors are
            # clamped to [0, 1] anyway.
        }
    }

    plot(
        NA,
        NA,
        xlim = range(curves$concentration_for_plots, na.rm = TRUE),
        ylim = c(0, ymax),
        log = "x",
        xlab = xlab,
        ylab = ylab,
        main = main,
        xaxt = "n",
        yaxt = "n",
        bty = "L"
    )

    # The lines are drawn in this order to ensure that dotted and dashed lines
    # are on top of solid lines for better visibility.
    if ("sys_tox_observed" %in% which) {
        points(
            point_concentration,
            model$sys_tox_observed,
            pch = 16,
            col = "steelblue3"
        )
    }
    if ("stress_tox_sys" %in% which) {
        lines(
            curves$concentration_for_plots,
            curves$stress_tox_sys,
            col = "blue"
        )
    }
    if ("stress_tox" %in% which) {
        lines(
            curves$concentration_for_plots,
            curves$stress_tox,
            col = "deepskyblue",
            lty = 2
        )
    }
    if ("sys_tox" %in% which) {
        lines(
            curves$concentration_for_plots,
            curves$sys_tox,
            col = "steelblue3"
        )
    }
    if (model$with_env) {
        if ("sys_tox_env_observed" %in% which) {
            points(
                point_concentration,
                model$sys_tox_env_observed,
                pch = 16,
                col = "violetred"
            )
        }
        if ("stress_tox_env_sys" %in% which) {
            lines(
                curves$concentration_for_plots,
                curves$stress_tox_env_sys,
                col = "red"
            )
        }
        if ("stress_env" %in% which) {
            lines(
                curves$concentration_for_plots,
                curves$stress_env,
                col = "forestgreen",
                lty = 3
            )
        }
        if ("stress_tox_env" %in% which) {
            lines(
                curves$concentration_for_plots,
                curves$stress_tox_env,
                col = "orange",
                lty = 2
            )
        }
        if ("sys_tox_env" %in% which) {
            lines(
                curves$concentration_for_plots,
                curves$sys_tox_env,
                col = "violetred"
            )
        }
    }

    # The setting of col = NA and col.ticks = par("fg") is to prevent ugly line
    # thickness issues when plotting as a png with type = "cairo" and at a low
    # resolution.
    axis(1, at = ticks$major, labels = ticks$major_labels,
         col = NA, col.ticks = par("fg"))
    axis(1, at = ticks$minor, labels = FALSE, tcl = -0.25,
         col = NA, col.ticks = par("fg"))
    plotrix::axis.break(1, breakpos = model$axis_break_conc)
    axis(2, col = NA, col.ticks = par("fg"), las = 1)

    if (show_legend) {
        legend_df <- data.frame(
            name = c( "sys_tox_observed", "stress_tox", "sys_tox",
                      "stress_tox_sys", "sys_tox_env_observed", "stress_env",
                      "stress_tox_env", "sys_tox_env", "stress_tox_env_sys"),
            text = c("sys (tox, observed)", "tox", "sys (tox)", "tox + sys",
                     "sys (tox + env, observed)", "env", "tox + env",
                     "sys (tox + env)", "tox + env + sys"),
            pch = c(16, NA, NA, NA, 16, NA, NA, NA, NA),
            lty = c(0, 2, 1, 1, 0, 3, 2, 1, 1),
            col = c("steelblue3", "deepskyblue", "steelblue3", "blue",
                    "violetred", "forestgreen", "orange", "violetred", "red"),
            stringsAsFactors = FALSE
        )
        legend_df <- legend_df[legend_df$name %in% which, ]
        if (nrow(legend_df) > 0) {
            legend(
                "topright",
                legend = legend_df$text,
                pch = legend_df$pch,
                lty = legend_df$lty,
                col = legend_df$col
            )
        }
    }
    NULL  # suppress all possible return values
}

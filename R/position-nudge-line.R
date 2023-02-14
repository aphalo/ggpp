#' Nudge labels away from a line
#'
#' \code{position_nudge_line()} is generally useful for adjusting the starting
#' position of labels or text to be repelled while preserving the original
#' position as the start of the segments. The difference compared to
#' \code{\link{position_nudge_center}} is that the nudging is away from from a
#' line or curve fitted to the data points or supplied as coefficients. While
#' \code{position_nudge_center()} is most useful for "round-shaped", vertically-
#' or horizontally elongated clouds of points, \code{position_nudge_line()} is
#' most suitable when observations follow a linear or curvilinear relationship
#' between \emph{x} and \emph{y} values. In contrast to
#' \code{\link[ggplot2]{position_nudge}}, \code{position_nudge_line()} returns
#' in `data` both the original coordinates and the nudged coordinates.
#'
#' @family position adjustments
#'
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in \code{data}.
#' @param xy_relative Nudge relative to \emph{x} and \emph{y} data expanse, ignored unless
#'   \code{x} and \code{y} are both \code{NA}s.
#' @param abline a vector of length two giving the intercept and slope.
#' @param method One of \code{"spline"}, \code{"lm"} or \code{"auto"}.
#' @param formula A model formula for \code{\link{lm}} when \code{method =
#'   "lm"}. Ignored otherwise.
#' @param direction One of \code{"none"}, or \code{"split"}.
#' @param line_nudge A positive multiplier >= 1, increasing nudging away from
#'   the curve or line compared to nudging from points.
#' @param kept.origin One of \code{"original"} or \code{"none"}.
#'
#' @details The default amount of nudging is 3% of the spread of the data along
#'   \emph{x} and \emph{y} axes, which in most cases is good. In most cases it is best to
#'   apply nudging along a direction perpendicular to the line or curve, if this
#'   is the aim, passing an argument to only one of \code{x}, \code{y} or
#'   \code{xy_relative} will be enough. When \code{direction = "split"} nudging
#'   is away from an implicit line or curve on either side with positive
#'   nudging. The line or curve can be smooth spline or linear regression fitted
#'   on-the-fly to the data points, or a straight line defined by its
#'   coefficients passed to \code{abline}. The fitting is well defined only if
#'   the observations fall roughly on a curve or straight line that is monotonic
#'   in \code{y}. By means of \code{line_nudge} one can increment nudging away
#'   from the line or curve compared to away from the points, which is useful
#'   for example to keep labels outside of a confidence band. Direction defaults
#'   to \code{"split"} when \code{line_nudge} > 1, and otherwise to
#'   \code{"none"}.
#'
#' @note For \code{method = "lm"} only model formulas corresponding to
#'   polynomials with no missing terms are supported. If using\code{\link{poly}}
#'   in the model formula, \code{raw = TRUE} is required.
#'
#'   In practice, \code{x} and \code{y} should have the same sign for nudging to
#'   work correctly.
#'
#'   This position is most useful when labeling points conforming a cloud along
#'   an arbitrary curve or line.
#'
#' @seealso \code{\link[ggplot2]{position_nudge}},
#'  \code{\link[ggrepel]{position_nudge_repel}}.
#'
#' @return A \code{"Position"} object.
#'
#' @export
#'
#' @examples
#'
#' set.seed(16532)
#' df <- data.frame(
#'   x = -10:10,
#'   y = (-10:10)^2,
#'   yy = (-10:10)^2 + rnorm(21, 0, 4),
#'   yyy = (-10:10) + rnorm(21, 0, 4),
#'   l = letters[1:21]
#' )
#'
#' # Setting the nudging distance
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line())
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text_s(position = position_nudge_line())
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line(xy_relative = -0.03))
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line(x = 0.6, y = 3.2))
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line(x = -0.6, y = -4))
#'
#' # Other curves, using defaults
#'
#' ggplot(df, aes(x, -y, label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line())
#'
#' ggplot(subset(df, x >= 0), aes(y, sqrt(y), label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line())
#'
#' # Points scattered near a curve or line, we use 'direction = "split"'
#'
#' ggplot(df, aes(x)) +
#'   geom_line(aes(y = y), linetype = "dotted") +
#'   geom_point(aes(y = yy)) +
#'   geom_text(aes(y = yy, label = l),
#'             position = position_nudge_line(direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(y, yy)) +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_point() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(direction = "split"))
#'
#' # increasing the nudging for labels near the line
#'
#' ggplot(subset(df, x >= 0), aes(y, yy)) +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_point() +
#'   geom_text(aes(label = l),
#'               position = position_nudge_line(line_nudge = 2,
#'                                              direction = "split"))
#'
#' # fitting a linear model instead of the default spline
#'
#' ggplot(subset(df, x >= 0), aes(y, yy)) +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_point() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(method = "lm",
#'                                            direction = "split"))
#'
#' ggplot(subset(df, x >= 0), aes(x, x^2)) +
#'   stat_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE)) +
#'   geom_point() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(method = "lm",
#'                                            formula = y ~ poly(x, 2, raw = TRUE)))
#'
position_nudge_line <- function(x = NA_real_,
                                y = NA_real_,
                                xy_relative = c(0.03, 0.03),
                                abline = NULL,
                                method = NULL,
                                formula = y ~ x,
                                direction = NULL,
                                line_nudge = 1,
                                kept.origin = "original") {

  # Ensure error message is triggered early
  if (!kept.origin %in% c("original", "none")) {
    stop("Invalid 'kept.origin': ", kept.origin,
         "expected: `\"original\" or \"none\"")
  }

  # set defaults
  if (!is.null(abline)) {
    method <- "abline"
  } else {
    abline <- rep(NA_real_, 2) # to ensure that a list member is created
  }

  if (is.null(method)) {
    method <- "auto" # decided later based on nrow(data)
  }

  if (method == "linear") {
    method <- "lm"
  }

  if (is.null(direction)) {
    if (line_nudge > 1) {
      direction <- "split"
    } else {
      direction <- "none"
    }
  }

  if (length(xy_relative) == 1) {
    xy_relative <- rep(xy_relative, 2)
  }

  stopifnot(length(xy_relative) == 2)

  ggplot2::ggproto(
    NULL,
    PositionNudgeLine,
    x = x,
    y = y,
    xy_relative = xy_relative,
    abline = abline,
    method = method,
    formula = formula,
    direction = direction,
    line_nudge = line_nudge,
    kept.origin = kept.origin
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudgeLine <-
  ggplot2::ggproto(
    "PositionNudgeLine",
    Position,
    x = 0,
    y = 0,
    xy_relative = c(0.03, 0.03),
    abline = rep(NA_real_, 2),
    method = "spline",
    formula = y ~ x,
    direction = "none",
    line_nudge = 1,

    setup_params = function(self, data) {
      list(x = self$x,
           y = self$y,
           xy_relative = self$xy_relative,
           abline = self$abline,
           method = self$method,
           formula = self$formula,
           kept.origin = self$kept.origin,
           direction = self$direction,
           line_nudge = self$line_nudge
      )
    },

    compute_panel = function(data, params, scales) {
      x_orig <- data$x
      y_orig <- data$y
      # set parameter defaults that depend on the scale
      x_range <- scales$x$dimension()
      y_range <- scales$y$dimension()
      x_spread <- x_range[2] - x_range[1]
      y_spread <- y_range[2] - y_range[1]
      xy.range.ratio <- x_spread / y_spread

      if (all(is.na(params$x)) & all(is.na(params$y))) {
        params$x <- params$xy_relative[1] * x_spread
        params$y <- params$xy_relative[2] * y_spread
      } else if (xor(all(is.na(params$x)), all(is.na(params$y)))) {
        if (is.na(params$x)) {
          params$x <- params$y * xy.range.ratio
        } else {
          params$y <- params$x / xy.range.ratio
        }
      }

      if (params$method == "auto") {
        if (nrow(data) < 5) {
          params$method <- "lm"
        } else {
          params$method <- "spline"
        }
      }

      # compute lines or curves and their derivatives
      if (params$method == "abline") {
        if (is.numeric(params$abline) && length(params$abline) == 2) {
          curve <- params$abline[1] + params$abline[2] * data$x
          # ensure same length in all cases
          sm.deriv <- rep(params$abline[2], nrow(data))
        } else {
          stop("'abline' should be a numeric vector of length 2")
        }
      } else if (params$method %in% c("lm", "spline")) {
        # we need to handle grouping by ourselves as compute_group does not work
        curve <- sm.deriv <- numeric(nrow(data))
        for (group in unique(data$group)) {
          in.grp <- data$group == group
          if (nrow(data[in.grp, ]) < 4 || params$method == "lm") {
            mf <- stats::lm(formula = params$formula, data = data[in.grp, ])
            curve[in.grp] <- stats::predict(mf)
            coef.poly <- polynom::polynomial(stats::coefficients(mf))
            deriv.poly <- stats::deriv(coef.poly)
            sm.deriv[in.grp] <- stats::predict(deriv.poly, data[in.grp, "x"])
            if (params$method != "lm") {
              message("Fitting a linear regression as n < 4")
            }
          } else if (params$method == "spline") {
            sm.spline <- stats::smooth.spline(data[in.grp, "x"], data[in.grp, "y"])
            curve[in.grp] <- stats::predict(sm.spline, x = data[in.grp, "x"], deriv = 0)$y
            sm.deriv[in.grp] <- stats::predict(sm.spline, x = data[in.grp, "x"], deriv = 1)$y
          }
        }
      } else {
        stop("Method \"", params$method, "\"not recognized")
      }

      # compute x and y nudge for each point
      # By changing the sign we ensure consistent positions in opposite slopes
      angle.rotation <- ifelse(sm.deriv > 0, -0.5 * pi, +0.5 * pi)
      # scaling is needed to compute the angle on the plot
      angle <- atan2(sm.deriv * xy.range.ratio, 1) + angle.rotation
      x_nudge <- params$x * cos(angle) * ifelse(sm.deriv > 0, -1, +1)
      y_nudge <- params$y * sin(angle) * ifelse(sm.deriv > 0, -1, +1)

      if (params$direction == "split") {
        # sign depends on position relative to the line or curve
        x_nudge <- ifelse(data$y >= curve, x_nudge, -x_nudge)
        y_nudge <- ifelse(data$y >= curve, y_nudge, -y_nudge)
      } else if (params$direction != "none") {
        warning("Ignoring unrecognized direction \"", params$direction, "\".")
      }

      if (params$line_nudge > 1) {
        # nudging further away from line or curve than from points
        adj_y_nudge <- y_nudge * params$line_nudge - (data$y - curve)
        adj_x_nudge <- x_nudge * adj_y_nudge / y_nudge
        y_nudge <- ifelse(sign(y_nudge) == sign(adj_y_nudge) &
                            abs(y_nudge) < abs(adj_y_nudge),
                          adj_y_nudge,
                          y_nudge)
        x_nudge <- ifelse(sign(y_nudge) == sign(adj_y_nudge) &
                            abs(x_nudge) >= abs(adj_x_nudge),
                          adj_x_nudge,
                          x_nudge)
      }
      # transform only the dimensions for which new coordinates exist
      if (any(params$x != 0)) {
        if (any(params$y != 0)) {
          data <- ggplot2::transform_position(data, function(x) x + x_nudge, function(y) y + y_nudge)
        } else {
          data <- ggplot2::transform_position(data, function(x) x + x_nudge, NULL)
        }
      } else if (any(params$y != 0)) {
        data <- ggplot2::transform_position(data, NULL, function(y) y + y_nudge)
      }
      if (params$kept.origin == "original") {
        data$x_orig <- x_orig
        data$y_orig <- y_orig
      }

      data
    }

  )

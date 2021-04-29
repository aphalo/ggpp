#' Nudge labels away from a line
#'
#' `position_nudge_line` is generally useful for adjusting the starting
#' position of labels or text to be repelled while preserving the original
#' position as the start of the segments. The difference compared to
#' [position_nudge_center()] is that the nudging is away from from a line or
#' curve fitted to the data points or supplied as coefficients. While
#' [position_nudge_center()] is most useful for "round-shaped", vertically- or
#' horizontally elongated clouds of points, [position_nudge_line()] is most
#' suitable when observations follow a linear or curvilinear relationship
#' between _x_ and _y_ values. In contrast to [ggplot2::position_nudge],
#' `position_nudge_line()` returns in `data` both the original
#' coordinates and the nudged coordinates.
#'
#' @family position adjustments
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`.
#' @param xy_relative Nudge relative to _x_ and _y_ data expanse, ignored
#'   unless `x` and `y` are both `NA`s.
#' @param abline a vector of length two giving the intercept and slope.
#' @param method One of `"spline"`, `"lm"` or `"auto"`.
#' @param formula A model formula for [lm()] when `method = "lm"`. Ignored
#'   otherwise.
#' @param direction One of "none", or "split".
#' @param line_nudge A positive multiplier >= 1, increasing nudging
#'   away from the curve or line compared to nudging from points.
#' @details The default ammount of nudging is 3% of the spread of the data along
#'   _x_ and _y_ axes, which in most cases is good. In most cases it is best to
#'   apply nudging along a direction perpendicular to the line or curve, if this
#'   is the aim, passing an argument to only one of `x`, `y` or `xy_relative`
#'   will be enough. When `direction = "split"` nudging is away from an implicit
#'   line or curve on either side with positive nudging. The line of curve can
#'   be smooth spline or linear regression fitted on-the-fly to the data points,
#'   or a straight line defined by its coefficients passed to `abline`. The
#'   fitting is well defined only if the observations fall roughly on a curve or
#'   straight line that is monotonic in `y`. By means of `line_nudge` one can
#'   increment nudging away from the line or curve compared to away from the
#'   points, which is useful for example to keep labels outside of a confidence
#'   band. Direction defaults to `"split"` when `line_nudge > 1`, and otherwise
#'   to `"none"`.
#'
#' @note For `method = "lm"` only model formulas corresponding to polynomials
#'   with no missing terms are supported. If using [poly()], `raw = TRUE` is
#'   required.
#'
#'   In practice, `x` and `y` should have the same sign for nudging to work
#'   correctly.
#'
#'   This position is most useful when labeling points conforming a cloud along
#'   an arbitrary curve or line.
#'
#' @seealso [ggplot::position_nudge()], [ggrepel::position_nudge_repel()].
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
#'   geom_text(position = position_nudge_line(xy_relative = -0.03))
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line(x = 0.6))
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line(y = 3.2))
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
#' ggplot(df, aes(x, y - 40, label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line())
#'
#' ggplot(subset(df, x >= 0), aes(y, sqrt(y), label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line())
#'
#' # nudging outwards and downwards from a curve
#'
#' ggplot(subset(df, x >= 0), aes(y, sqrt(y), label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line(xy_relative = -0.03))
#'
#' # an arbitrary straight line
#'
#' ggplot(df, aes(x, x * 2 + 5, label = l)) +
#'   geom_abline(intercept = 5, slope = 2, linetype = "dotted") +
#'   geom_point() +
#'   geom_text(position = position_nudge_line(abline = c(5, 2)))
#'
#' # Points scattered near a curve or line, we use 'direction = "split"'
#'
#' ggplot(subset(df, x >= 0), aes(x, yyy)) +
#'   stat_smooth(method = "lm", formula = y ~ x) +
#'   geom_point() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(direction = "split"))
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
#'             position = position_nudge_line(line_nudge = 2,
#'                                            direction = "split"))
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
#' ggplot(subset(df, x >= 0), aes(x, x^2)) +
#'   stat_smooth(method = "lm", formula = y ~ x + I(x^2)) +
#'   geom_point() +
#'   geom_text(aes(label = l),
#'             position = position_nudge_line(method = "lm",
#'                                            formula = y ~ x + I(x^2)))
#'
#' # grouping is supported
#'
#' df <- data.frame(x = rep(1:10, 2),
#'                  y = c(1:10, 10:1),
#'                  group = rep(c("a", "b"), c(10, 10)),
#'                  l = "+")
#'
#' ggplot(df, aes(x, y, label = l, color = group)) +
#'   geom_line(linetype = "dotted") +
#'   geom_text() +
#'   geom_text(position = position_nudge_line()) +
#'   geom_text(position = position_nudge_line(xy_relative = -0.03))
#'
#' # one needs to ensure that grouping is in effect in the geoms with nudging
#'
#' ggplot(df, aes(x, y, label = l, color = group, group = group)) +
#'   geom_line(linetype = "dotted") +
#'   geom_text() +
#'   geom_text(color = "red",
#'             position = position_nudge_line()) +
#'   geom_text(color = "blue",
#'             position = position_nudge_line(xy_relative = -0.03)) +
#'   coord_equal()
#'
#' # facets are also supported
#'
#' ggplot(df, aes(x, y, label = l)) +
#'   geom_line(linetype = "dotted") +
#'   geom_text() +
#'   geom_text(position = position_nudge_line(xy_relative = c(0.06, 0.03)),
#'             color = "red") +
#'   geom_text(position = position_nudge_line(xy_relative = -c(0.06, 0.03)),
#'             color = "blue") +
#'   facet_wrap(~group) +
#'   coord_equal(ratio = 1.5)
#'
position_nudge_line <- function(x = NA_real_,
                                y = NA_real_,
                                xy_relative = c(0.03, 0.03),
                                abline = NULL,
                                method = NULL,
                                formula = y ~ x,
                                direction = NULL,
                                line_nudge = 1) {
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
    line_nudge = line_nudge
  )
}

#' @rdname ggpextra-ggproto
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
            mf <- lm(formula = params$formula, data = data[in.grp, ])
            curve[in.grp] <- predict(mf)
            coef.poly <- polynom::polynomial(coef(mf))
            deriv.poly <- deriv(coef.poly)
            sm.deriv[in.grp] <- predict(deriv.poly, data[in.grp, "x"])
            if (params$method != "lm") {
              message("Fitting a linear regression as n < 4")
            }
          } else if (params$method == "spline") {
            sm.spline <- smooth.spline(data[in.grp, "x"], data[in.grp, "y"])
            curve[in.grp] <- predict(sm.spline, x = data[in.grp, "x"], deriv = 0)$y
            sm.deriv[in.grp] <- predict(sm.spline, x = data[in.grp, "x"], deriv = 1)$y
          }
        }
      } else {
        stop("Method \"", params$method, "\"not recognized")
      }

      # compute x and y nudge for each point
      # By changing the sign we ensure consistent positions in opposite slopes
      angle.rotation <- ifelse(sm.deriv > 0, -0.5 * pi, +0.5 * pi)
      # scaling is needed to conpute the angle on the plot
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
          data <- transform_position(data, function(x) x + x_nudge, function(y) y + y_nudge)
        } else {
          data <- transform_position(data, function(x) x + x_nudge, NULL)
        }
      } else if (any(params$y != 0)) {
        data <- transform_position(data, NULL, function(y) y + y_nudge)
      }
      data$x_orig <- x_orig
      data$y_orig <- y_orig
      data
    }
  )

#' @rdname position_dodgenudge_to
#'
#' @export
#'
position_dodge2nudge_to <-
  function(width = 1,
           preserve = c("total", "single"),
           padding = 0.1,
           reverse = FALSE,
           x = NULL,
           y = NULL,
           x.action = c("none", "spread"),
           y.action = c("none", "spread"),
           x.distance = "equal",
           y.distance = "equal",
           x.expansion = 0,
           y.expansion = 0,
           kept.origin = c("dodged", "original", "none")) {

    stopifnot("'x' must be NULL or of mode numeric" = length(x) == 0 ||
                (!anyNA(x) && mode(x) == "numeric"))
    stopifnot("'y' must be NULL or of mode numeric" = length(y) == 0 ||
                (!anyNA(y) && mode(y) == "numeric"))

    # this works as long as nudge and mapped variable are of the same class
    # ggplot2's behaviour has been in the past and seems to be again to expect
    # numeric seconds for POSIXct and numeric days for Date time shifts
    if (lubridate::is.instant(x)) {
      x <- as.numeric(x)
    }
    if (lubridate::is.instant(y)) {
      y <- as.numeric(y)
    }

    ggplot2::ggproto(NULL, PositionDodge2AndNudgeTo,
                     x = x,
                     y = y,
                     x.action = rlang::arg_match(x.action),
                     y.action = rlang::arg_match(y.action),
                     x.distance = x.distance,
                     y.distance = y.distance,
                     x.expansion = rep_len(x.expansion, 2),
                     y.expansion = rep_len(y.expansion, 2),
                     kept.origin = rlang::arg_match(kept.origin),
                     width = width,
                     preserve = rlang::arg_match(preserve),
                     padding = padding,
                     reverse = reverse
    )
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionDodge2AndNudgeTo <-
  ggplot2::ggproto(
    "PositionDodge2AndNudgeTo",
    Position,
    x = NULL,
    y = NULL,

    setup_params = function(self, data) {
      list(x = self$x,
           y = self$y,
           x.action = self$x.action,
           y.action = self$y.action,
           x.distance = self$x.distance,
           y.distance = self$y.distance,
           x.expansion = self$x.expansion,
           y.expansion = self$y.expansion,
           x.reorder = !is.null(self$x) && length(self$x) > 1 && length(self$x) < nrow(data),
           y.reorder = !is.null(self$y) && length(self$y) > 1 && length(self$y) < nrow(data),
           kept.origin = self$kept.origin,
           width = self$width,
           preserve = self$preserve,
           padding = self$padding,
           reverse = self$reverse
      )
    },

    compute_layer = function(self, data, params, layout) {
      x_orig <- data$x
      y_orig <- data$y
      if (!is.na(params$width)) {
        # operate on the dodged positions
        data = ggplot2::ggproto_parent(ggplot2::PositionDodge2, self)$compute_layer(data, params, layout)
      }
      x_dodged <- data$x
      y_dodged <- data$y

      # compute/convert x nudges
      if (!length(params$x)) {
        # set default x
        if (params$x.action == "none") {
          params$x <- rep_len(0, nrow(data))
        } else if (params$x.action == "spread") {
          params$x <- range(x_dodged)
        }
      } else if (is.numeric(params$x)) {
        # check user supplied x
        if (length(params$x) > nrow(data)) {
          warning("Argument 'x' longer than data: some values dropped!")
        }
        if (params$x.action == "none") {
          # recycle or trim x as needed
          if (params$x.reorder) {
            params$x <- rep_len(params$x, nrow(data))[order(order(data$x))] - x_dodged
          } else {
            params$x <- rep_len(params$x, nrow(data)) - x_dodged
          }
        } else if (params$x.action == "spread") {
          params$x <- range(params$x)
        }
      }

      if (params$x.action == "spread") {
        # apply x.expansion to x
        x.spread <- diff(params$x)
        params$x[1] <- params$x[1] - params$x.expansion[1] * x.spread
        params$x[2] <- params$x[2] + params$x.expansion[2] * x.spread
        if (params$x.distance == "equal") {
          # evenly spaced sequence of positions ordered as in data
          params$x <- seq(from = params$x[1],
                          to = params$x[2],
                          length.out = nrow(data))[order(order(data$x))] - x_dodged
        }
        # other strategies to distribute positions could be added here
      }

      # compute/convert y nudges
      if (!length(params$y)) {
        # set default y
        if (params$y.action == "none") {
          params$y <- rep_len(0, nrow(data))
        } else if (params$y.action == "spread") {
          params$y <- range(y_dodged)
        }
      } else if (is.numeric(params$y)) {
        # check user supplied y
        if (length(params$y) > nrow(data)) {
          warning("Argument 'y' longer than data: some values dropped!")
        }
        if (params$y.action == "none") {
          # recycle or trim y as needed
          if (params$y.reorder) {
            params$y <- rep_len(params$y, nrow(data))[order(order(data$y))] - y_dodged
          } else {
            params$y <- rep_len(params$y, nrow(data)) - y_dodged
          }
        } else if (params$y.action == "spread") {
          params$y <- range(params$y)
        }
      }

      if (params$y.action == "spread") {
        y.spread <- diff(params$y)
        params$y[1] <- params$y[1] - params$y.expansion[1] * y.spread
        params$y[2] <- params$y[2] + params$y.expansion[2] * y.spread
        if (params$y.distance == "equal") {
          # evenly spaced sequence ordered as in data
          params$y <- seq(from = params$y[1],
                          to = params$y[2],
                          length.out = nrow(data))[order(order(data$y))] - y_dodged
        }
        # other strategies could be added here
      }

      # As in 'ggplot2' we apply the nudge to xmin, xmax, xend, ymin, ymax, and yend.
      # Transform the dimensions for which not all nudges are zero
      if (any(params$x != 0)) {
        if (any(params$y != 0)) {
          data <- transform_position(data, function(x) x + params$x, function(y) y + params$y)
        } else {
          data <- transform_position(data, function(x) x + params$x, NULL)
        }
      } else if (any(params$y != 0)) {
        data <- transform_position(data, NULL, function(y) y + params$y)
      }
      # add original position
      if (params$kept.origin == "dodged" && !is.na(params$width)) {
        data$x_orig <- x_dodged
        data$y_orig <- y_dodged
      } else if (params$kept.origin == "original") {
        data$x_orig <- x_orig
        data$y_orig <- y_orig
      }

      data
    },

    compute_panel = function(self, data, params, scales) {
      ggplot2::ggproto_parent(PositionDodge2, self)$compute_panel(data, params, scales)
    }
  )

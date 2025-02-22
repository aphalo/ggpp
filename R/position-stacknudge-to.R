#' Stack plus nudge labels to new positions
#'
#' Functions \code{position_stacknudge_to()} and \code{position_fillnudge_to()}
#' are meant to complement \code{position_stack()} and \code{position_fill()}
#' from 'ggplot2', adding as a second action that of \code{position_nudge_to()}.
#' These positions are generally useful for adjusting the position of labels or
#' text. As with other position functions in this package, the original
#' positions are preserved to allow the text or labels to be linked back to
#' their original position with a segment or arrow.
#'
#' @family position adjustments
#'
#' @param vjust Vertical adjustment for geoms that have a position (like points
#'   or lines), not a dimension (like bars or areas). Set to 0 to align with the
#'   bottom, 0.5 for the middle, and 1 (the default) for the top.
#' @param reverse If TRUE, will reverse the default stacking order. This is
#'   useful if you're rotating both the plot and legend.
#' @param x,y Coordinates of the destination position. A vector of mode
#'   \code{numeric}, that is extended if needed, to the same length as rows
#'   there are in \code{data}. The default, \code{NULL}, leaves the original
#'   coordinates unchanged after dodging.
#' @param x.action,y.action character string, one of \code{"none"}, or
#'   \code{"spread"}. With \code{"spread"} distributing the positions
#'   within the range of argument \code{x} or \code{y}, if non-null, or the
#'   range the variable mapped to \emph{x} or \code{y}, otherwise.
#' @param x.distance,y.distance character or numeric Currently only
#'   \code{"equal"} is implemented.
#' @param x.expansion,y.expansion numeric vectors of length 1 or 2, as a
#'   fraction of width of the range.
#' @param kept.origin One of \code{"original"}, \code{"stacked"} or
#'   \code{"none"}.
#'
#' @details These positions apply sequentially two actions, in the order they
#'   appear in their names. The applied stacking is similar to that by
#'   \code{\link[ggplot2]{position_stack}} and
#'   \code{\link[ggplot2]{position_fill}} while nudging is different to that
#'   by \code{\link[ggplot2]{position_nudge}} and equal to that applied by
#'   \code{\link{position_nudge_to}}.
#'
#'   The nudged to \code{x} and/or \code{y} values replace the original
#'   ones in \code{data}, while the original or the stacked coordinates are
#'   returned in \code{x_orig} and \code{y_orig}. Nudge values supported are
#'   those of \emph{mode} numeric, thus including dates and times when they
#'   match the mapped data.
#'
#'   If the length of \code{x} and/or \code{y} is more than one but less than
#'   rows are present in the data, the vector is both recycled and reordered so
#'   that the nudges are applied sequentially based on the data values. If their
#'   length matches the number of rows in data, they are assumed to be already
#'   in data order.
#'
#'   When applying stacking, the return of original positions instead of the
#'   stacked ones is achieved by passing \code{origin = "original"} instead of
#'   the default of \code{origin = "stacked"}.
#'
#' @note Irrespective of the action, the ordering of rows in \code{data} is
#'   preserved.
#'
#' @return A \code{"Position"} object.
#'
#' @seealso \code{\link{position_nudge_to}},
#'   \code{\link[ggplot2]{position_stack}}.
#'
#' @examples
#'
#' cols.df <- data.frame(x1 = c(1, 2, 1, 3),
#'                        x2 = c("a", "a", "b", "b"),
#'                        grp = c("A", "B", "C", "D"))
#'
#'  ggplot(data = cols.df, aes(x2, x1, group = grp)) +
#'  geom_col(aes(fill = grp), width=0.5, position = "stack") +
#'    geom_vline(xintercept = 0) +
#'    geom_text_s(
#'      aes(label = x1),
#'      position = position_stacknudge_to(x = c(1.4, 1.4, 1.6, 1.6))) +
#'    theme(legend.position = "none")
#'
#'  ggplot(data = cols.df, aes(x2, x1, group = grp)) +
#'  geom_col(aes(fill = grp), width=0.5, position = "fill") +
#'    geom_vline(xintercept = 0) +
#'    geom_text_s(
#'      aes(label = grp),
#'      position = position_fillnudge_to(x = c(1.4, 1.4, 1.6, 1.6))) +
#'    theme(legend.position = "none")
#'
#'  ggplot(data = cols.df, aes(x2, x1, group = grp)) +
#'  geom_col(aes(fill = grp), width=0.5, position = "fill") +
#'    geom_vline(xintercept = 0) +
#'    geom_text_s(
#'      aes(label = x1),
#'      position = position_fillnudge_to(vjust = 0.5,
#'                                       x = c(1.4, 1.4, 1.6, 1.6))) +
#'    theme(legend.position = "none")
#'
#'  ggplot(data = cols.df, aes(x2, x1, group = grp)) +
#'  geom_col(aes(fill = grp), width=0.5, position = "fill") +
#'    geom_vline(xintercept = 0) +
#'    geom_text_s(
#'      aes(label = x1), vjust = 0.5,
#'      position = position_fillnudge_to(vjust = 0.5,
#'                                       x = c(1.4, 1.4, 1.6, 1.6),
#'                                       y = c(0.85, 0.35, 0.85, 0.35))) +
#'    theme(legend.position = "none")
#'
#' @export
#'
position_stacknudge_to <-
  function(vjust = 1,
           reverse = FALSE,
           x = NULL,
           y = NULL,
           x.action = c("none", "spread"),
           y.action = c("none", "spread"),
           x.distance = "equal",
           y.distance = "equal",
           x.expansion = 0,
           y.expansion = 0,
           kept.origin = c("stacked", "original", "none")) {

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

    ggplot2::ggproto(NULL, PositionStackNudgeTo,
                     x = x,
                     y = y,
                     x.action = rlang::arg_match(x.action),
                     y.action = rlang::arg_match(y.action),
                     x.distance = x.distance,
                     y.distance = y.distance,
                     x.expansion = rep_len(x.expansion, 2),
                     y.expansion = rep_len(y.expansion, 2),
                     kept.origin = rlang::arg_match(kept.origin),
                     vjust = vjust,
                     reverse = reverse
    )
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionStackNudgeTo <-
  ggplot2::ggproto(
    "PositionStackNudgeTo",
    ggplot2::PositionStack,
    fill = FALSE,
    x = NULL,
    y = NULL,

    setup_params = function(self, data) {
      c(list(x = self$x,
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
             vjust = self$vjust,
             reverse = self$reverse),
        ggplot2::ggproto_parent(ggplot2::PositionStack, self)$setup_params(data))
    },

    setup_data = function(self, data, params) {
      data <- flip_data(data, params$flipped_aes)
      if (is.null(params$var)) {
        return(data)
      }

      data$ymax <- switch(params$var,
                          y = data$y,
                          ymax = as.numeric(ifelse(data$ymax == 0, data$ymin, data$ymax))
      )

      data <- remove_missing(
        data,
        vars = c("x", "xmin", "xmax", "y"),
        name = "position_stack"
      )
      flip_data(data, params$flipped_aes)
    },

    compute_layer = function(self, data, params, layout) {
      x_orig <- x_stacked <- data$x
      y_orig <- data$y

      # operate on the stacked positions (updated in August 2020)
      data = ggplot2::ggproto_parent(ggplot2::PositionStack, self)$compute_layer(data, params, layout)
      y_stacked <- data$y

      # compute/convert x nudges
      if (!length(params$x)) {
        # set default x
        if (params$x.action == "none") {
          params$x <- rep_len(0, nrow(data))
        } else if (params$x.action == "spread") {
          params$x <- range(x_stacked)
        }
      } else if (is.numeric(params$x)) {
        # check user supplied x
        if (length(params$x) > nrow(data)) {
          warning("Argument 'x' longer than data: some values dropped!")
        }
        if (params$x.action == "none") {
          # recycle or trim x as needed
          if (params$x.reorder) {
            params$x <- rep_len(params$x, nrow(data))[order(order(data$x))] - x_stacked
          } else {
            params$x <- rep_len(params$x, nrow(data)) - x_stacked
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
                          length.out = nrow(data))[order(order(data$x))] - x_stacked
        }
        # other strategies to distribute positions could be added here
      }

      # compute/convert y nudges
      if (!length(params$y)) {
        # set default y
        if (params$y.action == "none") {
          params$y <- rep_len(0, nrow(data))
        } else if (params$y.action == "spread") {
          params$y <- range(y_stacked)
        }
      } else if (is.numeric(params$y)) {
        # check user supplied y
        if (length(params$y) > nrow(data)) {
          warning("Argument 'y' longer than data: some values dropped!")
        }
        if (params$y.action == "none") {
          # recycle or trim y as needed
          if (params$y.reorder) {
            params$y <- rep_len(params$y, nrow(data))[order(order(data$y))] - y_stacked
          } else {
            params$y <- rep_len(params$y, nrow(data)) - y_stacked
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
                          length.out = nrow(data))[order(order(data$y))] - y_stacked
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
      if (params$kept.origin == "stacked") {
        data$x_orig <- x_stacked
        data$y_orig <- y_stacked
      } else if (params$kept.origin == "original") {
        data$x_orig <- x_orig
        data$y_orig <- y_orig
      }
      data
    },

    compute_panel = function(self, data, params, scales) {
      ggplot2::ggproto_parent(PositionStack, self)$compute_panel(data, params, scales)
    }
  )

#' @rdname position_stacknudge_to
#'
#' @export
#'
position_fillnudge_to <-
  function(vjust = 1,
           reverse = FALSE,
           x = NULL,
           y = NULL,
           x.action = c("none", "spread"),
           y.action = c("none", "spread"),
           x.distance = "equal",
           y.distance = "equal",
           x.expansion = 0,
           y.expansion = 0,
           kept.origin = c("stacked", "original", "none")) {

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

    ggplot2::ggproto(NULL, PositionFillNudgeTo,
                     x = x,
                     y = y,
                     x.action = rlang::arg_match(x.action),
                     y.action = rlang::arg_match(y.action),
                     x.distance = x.distance,
                     y.distance = y.distance,
                     x.expansion = rep_len(x.expansion, 2),
                     y.expansion = rep_len(y.expansion, 2),
                     kept.origin = rlang::arg_match(kept.origin),
                     vjust = vjust,
                     reverse = reverse
    )
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionFillNudgeTo <-
  ggplot2::ggproto("PositionFillNudgeTo", PositionStackNudgeTo,
                   fill = TRUE
  )


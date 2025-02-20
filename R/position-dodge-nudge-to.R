#' Nudge labels to new positions
#'
#' \code{position_dodgenudge_to()} is generally useful for adjusting the
#' position of labels or text, both on a discrete or continuous scale.
#' \code{position_dodgenudge_to()} and \code{position_nudge_to()} differ from
#' \code{\link[ggplot2]{position_nudge}} in that the coordinates of the new
#' position are given directly, rather than as a displacement from the original
#' location. It optionally sets an even spacing among positions within a range.
#' In \code{position_dodgenudge_to()} this nudging can be combined with dodging.
#' As with other position functions in this package, the original positions are
#' preserved to allow the text or labels to be linked back to their original
#' position with a segment or arrow.
#'
#' @family position adjustments
#'
#' @param width Dodging width, when different to the width of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. See the examples.
#' @param preserve Should dodging preserve the total width of all elements at a
#'   position, or the width of a single element?.
#' @param x,y Coordinates of the destination position. A vector of mode
#'   \code{numeric}, that is extended if needed, to the same length as rows
#'   there are in \code{data}. The default, \code{NULL}, leaves the original
#'   coordinates unchanged after dodging.
#' @param x.action,y.action character string, one of \code{"none"}, or
#'   \code{"spread"}. With \code{"spread"} distributing the positions
#'   within the range of argument \code{x} or \code{y}, if non-null, or the
#'   range the variable mapped to \emph{x} or \code{y}, otherwise.
#' @param x.distance,y.distance character or numeric Currently only \code{"equal"} is
#'   implemented.
#' @param x.expansion,y.expansion numeric vectors of length 1 or 2, as a
#'   fraction of width of the range.
#' @param kept.origin One of \code{"original"}, \code{"dodged"} or
#'   \code{"none"}.
#'
#' @details The nudged to \code{x} and/or \code{y} values replace the original ones in
#'   \code{data}, while the original or the dodged coordinates are returned in \code{x_orig}
#'   and \code{y_orig}. Nudge values supported are those of \emph{mode} numeric,
#'   thus including dates and times when they match the mapped data.
#'
#'   If the length of \code{x} and/or \code{y} is more than one but less than
#'   rows are present in the data, the vector is both recycled and reordered so
#'   that the nudges are applied sequentially based on the data values. If their
#'   length matches the number of rows in data, they are assumed to be already
#'   in data order.
#'
#'   The applied dodge is identical to that by
#'   \code{\link[ggplot2]{position_dodge}} while nudging is different to that by
#'   \code{\link[ggplot2]{position_nudge}}.
#'
#' There are two possible uses for these functions. First, without using dodging
#' they can be used to obtain aligned labels when the labelled objects are not
#' aligned. This is the most common use.
#'
#' The second use is to label dodged bars, boxplots or points with labels
#' aligned. In this case, it is mandatory to use
#' the same argument to \code{width} when passing \code{position_dodge()} to
#' \code{geom_col()} and \code{position_dodgenudge_to()} to \code{geom_text()} or
#' \code{geom_label()} or their repulsive equivalents. Otherwise the arrows or
#' segments will fail to connect to the labels. In other words dodging is
#' computed twice. Dodge is identical to that obtained with the same arguments
#' in \code{\link[ggplot2]{position_dodge}} as \code{position_dodgenudge_to()}
#' simply calls the same code from package 'ggplot2' ahead of applying
#' nudging.
#'
#' When applying dodging, the return of original positions instead of the dodged
#' ones is achieved by passing \code{origin = "original"} instead of the default
#' of \code{origin = "dodged"}.
#'
#' @note Irrespective of the action, the ordering of rows in \code{data} is
#'   preserved.
#'
#' @return A \code{"Position"} object.
#'
#' @seealso \code{\link[ggplot2]{position_nudge}},
#'   \code{\link[ggplot2]{position_dodge}},
#'   \code{\link[ggrepel]{position_nudge_repel}}.
#'
#' @export
#'
#' @examples
#' df <- data.frame(
#'   x = c(1,3,2,5,4,2.5),
#'   y = c(2, 1, 2.5, 1.8, 2.8, 1.5),
#'   label = c("abc","cd","d","c","bcd","a")
#' )
#'
#' # default does nothing
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text(position = position_nudge_to())
#'
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text(position = position_dodgenudge_to())
#'
#' # a single y (or x) value nudges all observations to this data value
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text(position = position_nudge_to(y = 3))
#'
#' # with a suitable geom, segments or arrows can be added
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position = position_nudge_to(y = 3))
#'
#' # alternating in y value order because y has fewer values than rows in data
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position = position_nudge_to(y = c(3, 0)))
#'
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position = position_nudge_to(y = c(0, 3)))
#'
#' # in data row order because y has as many values as rows in data
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position = position_nudge_to(y = rep_len(c(0, 3), 6)))
#'
#' # spread the values at equal distance within the available space
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'     position_nudge_to(y = 3, x.action = "spread"))
#'
#' # spread the values at equal distance within the expanded available space
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'     position_nudge_to(y = 3, x.action = "spread", x.expansion = 0.1))
#'
#' # spread the values at equal distance within the contracted available space
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'     position_dodgenudge_to(y = 3, x.action = "spread", x.expansion = -0.1))
#'
#' # spread the values at equal distance within the range given by x
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'     position_nudge_to(y = 3, x = c(2,4), x.action = "spread"),
#'     hjust = "center")
#'
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'     position_nudge_to(y = 3, x = c(0,6), x.action = "spread"),
#'     hjust = "center")
#'
position_dodgenudge_to <-
  function(width = 1,
           preserve = c("total", "single"),
           x = NULL,
           y = NULL,
           x.action = c("none", "spread"),
           y.action = c("none", "spread"),
           x.distance = "equal",
           y.distance = "equal",
           x.expansion = 0,
           y.expansion = 0,
           kept.origin = c("original", "none")) {
    preserve <- rlang::arg_match(preserve)
    kept.origin <- rlang::arg_match(kept.origin)
    x.action <- rlang::arg_match(x.action)
    y.action <- rlang::arg_match(y.action)
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

    ggplot2::ggproto(NULL, PositionDodgeNudgeTo,
                     x = x,
                     y = y,
                     x.action = x.action,
                     y.action = y.action,
                     x.distance = x.distance,
                     y.distance = y.distance,
                     x.expansion = rep_len(x.expansion, 2),
                     y.expansion = rep_len(y.expansion, 2),
                     kept.origin = kept.origin,
                     width = width,
                     preserve = rlang::arg_match(preserve)
    )
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionDodgeNudgeTo <-
  ggplot2::ggproto(
    "PositionDodgeNudgeTo",
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
           kept.origin = self$kept.origin
      )
    },

    compute_layer = function(self, data, params, layout) {
      # operate on the dodged positions
      data = ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$compute_layer(data, params, layout)

      x_dodged <- data$x
      y_dodged <- data$y
      x_orig <- data$x
      y_orig <- data$y

      # compute/convert x nudges
      if (!length(params$x)) {
        # set default x
        if (params$x.action == "none") {
          params$x <- rep_len(0, nrow(data))
        } else if (params$x.action == "spread") {
          params$x <- range(x_orig)
        }
      } else if (is.numeric(params$x)) {
        # check user supplied x
        if (length(params$x) > nrow(data)) {
          warning("Argument 'x' longer than data: some values dropped!")
        }
        if (params$x.action == "none") {
          # recycle or trim x as needed
          if (params$x.reorder) {
            params$x <- rep_len(params$x, nrow(data))[order(order(data$x))] - x_orig
          } else {
            params$x <- rep_len(params$x, nrow(data)) - x_orig
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
                          length.out = nrow(data))[order(order(data$x))] - x_orig
        }
        # other strategies to distribute positions could be added here
      }

      # compute/convert y nudges
      if (!length(params$y)) {
        # set default y
        if (params$y.action == "none") {
          params$y <- rep_len(0, nrow(data))
        } else if (params$y.action == "spread") {
          params$y <- range(y_orig)
        }
      } else if (is.numeric(params$y)) {
        # check user supplied y
        if (length(params$y) > nrow(data)) {
          warning("Argument 'y' longer than data: some values dropped!")
        }
        if (params$y.action == "none") {
          # recycle or trim y as needed
          if (params$y.reorder) {
            params$y <- rep_len(params$y, nrow(data))[order(order(data$y))] - y_orig
          } else {
            params$y <- rep_len(params$y, nrow(data)) - y_orig
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
                          length.out = nrow(data))[order(order(data$y))] - y_orig
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
      if (params$kept.origin == "dodged") {
        data$x_orig <- x_dodged
        data$y_orig <- y_dodged
      } else if (params$kept.origin == "original") {
        data$x_orig <- x_orig
        data$y_orig <- y_orig
      }

      data
    },

    compute_panel = function(self, data, params, scales) {
      ggplot2::ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)
    }
  )

#' @rdname position_dodgenudge_to
#'
#' @export
#'
# for backwards compatibility with arguments passed by position
position_nudge_to <-
  function(x = NULL,
           y = NULL,
           x.action = c("none", "spread"),
           y.action = c("none", "spread"),
           x.distance = "equal",
           y.distance = "equal",
           x.expansion = 0,
           y.expansion = 0,
           kept.origin = c("original", "none")) {

    position_dodgenudge_to(width = 1,
                           preserve = "total",
                           x = x,
                           y = y,
                           x.action = x.action,
                           y.action = y.action,
                           x.distance = x.distance,
                           y.distance = y.distance,
                           x.expansion = x.expansion,
                           y.expansion = y.expansion,
                           kept.origin = kept.origin
    )
  }

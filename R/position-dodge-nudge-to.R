#' Dodge plus nudge labels to new positions
#'
#' Functions \code{position_dodgenudge_to()} and
#' \code{position_dodge2nudge_to()} are meant to complement
#' \code{position_dodge()} and \code{position_dodge2()} from 'ggplot2', adding
#' as a second action that of \code{position_nudge_to()}. These positions are
#' generally useful for adjusting the position of labels or text. As with other
#' position functions in this package, the original positions are preserved to
#' allow the text or labels to be linked back to their original position with a
#' segment or arrow.
#'
#' @family position adjustments
#'
#' @param width Dodging width, when different to the width of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. See the examples.
#' @param preserve Should dodging preserve the total width of all elements at a
#'   position, or the width of a single element?.
#' @param reverse logical If TRUE, will reverse the default dodging order.
#' @param padding Padding between elements at the same position. Elements are
#'   shrunk by this proportion to allow space between them. Defaults to 0.1.
#' @param reverse If TRUE, will reverse the default dodging order.
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
#' @details These positions apply sequentially two actions, in the order they
#'   appear in their names. The applied dodge is similar to that by
#'   \code{\link[ggplot2]{position_dodge}} and
#'   \code{\link[ggplot2]{position_dodge2}} while nudging is different to that
#'   by \code{\link[ggplot2]{position_nudge}} and equal to that applied by
#'   \code{\link{position_nudge_to}}.
#'
#'   The dodged and nudged to \code{x} and/or \code{y} values replace the
#'   original ones in \code{data}, while the original or the dodged coordinates
#'   are returned in \code{x_orig} and \code{y_orig}. Nudge values supported are
#'   those of \emph{mode} numeric, thus including dates and times when they
#'   match the mapped data.
#'
#'   If the length of \code{x} and/or \code{y} is more than one but less than
#'   rows are present in the data, the vector is both recycled and reordered so
#'   that the nudges are applied sequentially based on the data values. If their
#'   length matches the number of rows in data, they are assumed to be already
#'   in data order.
#'
#'   The intended use is to label dodged bars, boxplots or points with labels
#'   aligned. In this case, it is mandatory to use the same argument to
#'   \code{width} when passing \code{position_dodge()} to \code{geom_col()} and
#'   \code{position_dodgenudge_to()} to \code{geom_text()}, \code{geom_label()},
#'   \code{geom_text_s()}, \code{geom_label_s()} or their repulsive equivalents
#'   from package 'ggrepel'. Otherwise the arrows or segments will fail to
#'   connect to the labels.
#'
#'   When applying dodging, the return of original positions instead of the
#'   dodged ones is achieved by passing \code{origin = "original"} instead of
#'   the default of \code{origin = "dodged"}.
#'
#' @note Irrespective of the action, the ordering of rows in \code{data} is
#'   preserved.
#'
#' @return A \code{"Position"} object.
#'
#' @seealso \code{\link{position_nudge_to}},
#'   \code{\link[ggplot2]{position_dodge}},
#'   \code{\link[ggplot2]{position_dodge2}}.
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(
#'   x = c(1,3,2,5,4,2.5),
#'   y = c(2, 3, 2.5, 1.8, 2.8, 1.5),
#'   grp = c("A", "A", "A", "B", "B", "B"),
#'   grp.inner = c("a", "b", "c", "a", "b", "c"),
#'   label = c("abc","cd","d","c","bcd","a")
#' )
#'
#' # default is no nudging
#' ggplot(df, aes(grp, y, label = label, fill = label)) +
#'   geom_col(position = position_dodge(width = 0.92)) +
#'   geom_text(position = position_dodgenudge_to(width = 0.92),
#'             vjust = -0.2) +
#'   theme(legend.position = "none")
#'
#' ggplot(df, aes(grp, y, label = label, fill = grp.inner)) +
#'   geom_col(position = position_dodge(width = 0.92)) +
#'   geom_text(position = position_dodgenudge_to(width = 0.92),
#'             vjust = -0.2)
#'
#' ggplot(df, aes(grp, y, label = label, fill = label)) +
#'   geom_col(position = position_dodge2(width = 0.92)) +
#'   geom_text(position = position_dodge2nudge_to(width = 0.92),
#'             vjust = -0.2) +
#'   theme(legend.position = "none")
#'
#' ggplot(df, aes(grp, y, label = label, fill = grp.inner)) +
#'   geom_col(position = position_dodge2(width = 0.92)) +
#'   geom_text(position = position_dodge2nudge_to(width = 0.92),
#'             vjust = -0.2)
#'
#' # nudging all labels to a given y value
#' ggplot(df, aes(grp, y, label = label, fill = grp.inner)) +
#'   geom_col(position = position_dodge(width = 0.92)) +
#'   geom_text(position = position_dodgenudge_to(width = 0.92, y = 0.8))
#'
#' ggplot(df, aes(grp, y, label = label, fill = grp.inner)) +
#'   geom_col(position = position_dodge2(width = 0.92)) +
#'   geom_text(position = position_dodge2nudge_to(width = 0.92, y = 0.8))
#'
#' ggplot(df, aes(grp, y, label = label, fill = grp.inner)) +
#'   geom_col(position = position_dodge(width = 0.92)) +
#'   geom_text(position = position_dodgenudge_to(width = 0.92, y = 0.8))
#'
#' ggplot(df[-1, ], aes(grp, y, label = label, fill = grp.inner)) +
#'   geom_col(position = position_dodge(width = 0.92)) +
#'   geom_text(position = position_dodgenudge_to(width = 0.92, y = 0.8))
#'
#' ggplot(df[-1, ], aes(grp, y, label = label, fill = grp.inner)) +
#'   geom_col(position = position_dodge(width = 0.92, preserve = "total")) +
#'   geom_text(position = position_dodgenudge_to(width = 0.92, y = 0.8,
#'                                               preserve = "total"))
#'
#' ggplot(df[-1, ], aes(grp, y, label = label, fill = grp.inner)) +
#'   geom_col(position = position_dodge(width = 0.92, preserve = "single")) +
#'   geom_text(position = position_dodgenudge_to(width = 0.92, y = 0.8,
#'                                               preserve = "single"))
#'
#' ggplot(df[-1, ], aes(grp, y, label = label, fill = grp.inner)) +
#'   geom_col(position = position_dodge2(width = 0.92, preserve = "total")) +
#'   geom_text(position = position_dodge2nudge_to(width = 0.92, y = 0.8,
#'                                               preserve = "total"))
#'
#' ggplot(df[-1, ], aes(grp, y, label = label, fill = grp.inner)) +
#'   geom_col(position = position_dodge2(width = 0.92, preserve = "single")) +
#'   geom_text(position = position_dodge2nudge_to(width = 0.92, y = 0.8,
#'                                               preserve = "single"))
#'
position_dodgenudge_to <-
  function(width = 1,
           preserve = c("total", "single"),
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

    ggplot2::ggproto(NULL, PositionDodgeNudgeTo,
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
                     reverse = reverse,
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
           kept.origin = self$kept.origin,
           width = self$width,
           reverse = self$reverse,
           preserve = self$preserve
      )
    },

    compute_layer = function(self, data, params, layout) {
      x_orig <- data$x
      y_orig <- data$y
      if (!is.na(params$width)) {
        # operate on the dodged positions
        data = ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$compute_layer(data, params, layout)
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
      ggplot2::ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)
    }
  )

#' Nudge labels to new positions
#'
#' \code{position_nudge_to()} differs from \code{\link[ggplot2]{position_nudge}}
#' in that the coordinates of the new position are given directly, rather than
#' as a displacement from the original location. It optionally sets an even
#' spacing among positions within a range. As with other position functions in
#' this package, the original positions are preserved to allow the text or
#' labels to be linked back to their original position with a segment or arrow.
#'
#' @family position adjustments
#'
#' @param x,y Coordinates of the destination position. A vector of mode
#'   \code{numeric}, that is extended if needed, to the same length as rows
#'   there are in \code{data}. The default, \code{NULL}, leaves the original
#'   coordinates unchanged.
#' @param x.action,y.action character string, one of \code{"none"}, or
#'   \code{"spread"}. With \code{"spread"} distributing the positions
#'   within the range of argument \code{x} or \code{y}, if non-null. Otherwise,
#'   using the range the variable mapped to \emph{x} or \code{y}.
#' @param x.distance,y.distance character or numeric Currently only
#'   \code{"equal"} is implemented, indicating equal spacing between the
#'   spread positions.
#' @param x.expansion,y.expansion numeric vectors of length 1 or 2, as a
#'   fraction of width of the range used to spread positions.
#' @param kept.origin One of \code{"original"} or \code{"none"}.
#'
#' @details The nudged to
#'   \code{x} and/or \code{y} values replace the original ones in
#'   \code{data}, while the original coordinates are returned in
#'   \code{x_orig} and \code{y_orig}. Nudge values supported are those of
#'   \emph{mode} numeric, thus including dates and times when they match the
#'   mapped data.
#'
#'   If the length of \code{x} and/or \code{y} is more than one but less than
#'   the rows present in the \code{data}, the vector is both recycled and
#'   reordered so that the nudges are applied sequentially based on the data
#'   values. If their length matches the number of rows in \code{data}, they are
#'   assumed to be already in \code{data} order.
#'
#'   Irrespective of the action, the ordering of rows in \code{data} is
#'   preserved.
#'
#' @note The current implementation DOES NOT support flipping geoms with the
#' \code{orientation} argument or implicitly by the mapping. It DOES NOT
#' apply scale transformations when spreading the positions.
#'
#' @return A \code{"Position"} object.
#'
#' @seealso \code{\link[ggplot2]{position_nudge}},
#'   \code{\link[ggrepel]{position_nudge_repel}}.
#'
#' @export
#'
#' @examples
#' # The examples below exemplify the features of position_nudge_to().
#' # Please see the vignette for examples of use cases.
#'
#' df <- data.frame(
#'   x = c(1,3,2,5,4,2.5),
#'   y = c(2, 3, 2.5, 1.8, 2.8, 1.5),
#'   grp = c("A", "A", "A", "B", "B", "B"),
#'   grp.inner = c("a", "b", "c", "a", "b", "c"),
#'   label = c("abc","cd","d","c","bcd","a")
#' )
#'
#' # default is no nudging
#' ggplot(df, aes(label, y, label = y)) +
#'   geom_col() +
#'   geom_text(position = position_nudge_to(),
#'             vjust = -0.2)
#'
#' # a single y (or x) value nudges all observations to this data value
#' ggplot(df, aes(label, y, label = y)) +
#'   geom_col() +
#'   geom_label(position = position_nudge_to(y = 1))
#'
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text(position = position_nudge_to(y = 3.2))
#'
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_line() +
#'   geom_text(position = position_nudge_to(y = 0.1))
#'
#' # with a suitable geom, segments or arrows can be added
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position = position_nudge_to(y = 2.25))
#'
#' # alternating in y value order because y has fewer values than rows in data
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position = position_nudge_to(y = c(3, 0.1)))
#'
#' # in data row order with as many nudge y values as rows in data
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position = position_nudge_to(y = c(1.8, 2.3, 1.3, 2.8, 3, 0.1)))
#'
#' # spread the values at equal distance within the available space
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'                 position_nudge_to(y = 4, x.action = "spread"))
#'
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'                 position_nudge_to(y = 4, x.action = "spread")) +
#' scale_x_log10()
#'
#' # spread the values at equal distance within the expanded available space
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'                 position_nudge_to(y = 4, x.action = "spread", x.expansion = 0.1))
#'
#' # spread the values at equal distance within the range given by x
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'                 position_nudge_to(y = 4, x = c(1.5, 4), x.action = "spread"))
#'
#' # currently if scale transformations are used, the x and/or y arguments must
#' # be transformed. WARNING: This will change in the near future!!
#'
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'                 position_nudge_to(y = 4, x = log10(c(1.5, 4)), x.action = "spread")) +
#' scale_x_log10()
#'
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position =
#'                 position_nudge_to(y = log10(4), x.action = "spread")) +
#' scale_y_log10()
#'
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

    position_dodgenudge_to(width = NA_real_, # used as flag to disable dodging
                           preserve = "total",
                           x = x,
                           y = y,
                           x.action = rlang::arg_match(x.action),
                           y.action = rlang::arg_match(y.action),
                           x.distance = x.distance,
                           y.distance = y.distance,
                           x.expansion = x.expansion,
                           y.expansion = y.expansion,
                           kept.origin = rlang::arg_match(kept.origin)
    )
  }

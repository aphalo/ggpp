#' Nudge labels to new positions
#'
#' \code{position_nudge_to()} is generally useful for adjusting the position of
#' labels or text, both on a discrete or continuous scale.
#' \code{position_nudge_to()} differs from \code{\link[ggplot2]{position_nudge}}
#' in that the coordinates of the new position are given directly, rather than
#' as a displacement from the original location. It optionally sets an even
#' distance among positions. As other position functions in this package, it
#' preserves the original position to allow the text to be linked back to its
#' original position with a segment or arrow.
#'
#' @family position adjustments
#'
#' @param x,y Coordinates of the destination position. A vector of mode
#'   \code{numeric}, that is extended if needed, to the same length as rows
#'   there are in \code{data}. The default, \code{NULL}, leaves the original
#'   coordinates unchanged.
#' @param x.action,y.action character string, one of \code{"none"}, or
#'   \code{"spread"}. With \code{"spread"} evenly distributing the positions
#'   within the range of argument \code{x} or \code{y}, if non-null, or the
#'   range the variable mapped to \emph{x} or \code{y}, otherwise.
#' @param kept.origin One of \code{"original"} or \code{"none"}.
#'
#' @details The nudged to \code{x} and/or \code{y} values replace the original ones in
#'   \code{data}, while the original coordinates are returned in \code{x_orig}
#'   and \code{y_orig}. Values supported are those of \emph{mode} numeric,
#'   thus including dates and times.
#'
#'   If the length of \code{x} and/or \code{y} is more than one but less than
#'   rows are present in the data, the vector is both recycled and reordered so
#'   that the nudges are applied sequentially based on the data values. If their
#'   length matches the number of rows in data, they are assumed to be already
#'   in data order.
#'
#' @note Irrespective of the action, the ordering of rows in \code{data} is
#'   preserved.
#'
#' @return A \code{"Position"} object.
#'
#' @seealso \code{\link[ggplot2]{position_nudge}},
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
position_nudge_to <-
  function(x = NULL,
           y = NULL,
           x.action = c("none", "spread"),
           y.action = c("none", "spread"),
           kept.origin = c("original", "none")) {
    kept.origin <- rlang::arg_match(kept.origin)
    x.action <- rlang::arg_match(x.action)
    y.action <- rlang::arg_match(y.action)
    stopifnot("'x' must be NULL or of mode numeric" = is.null(x) || mode(x) == "numeric")
    stopifnot("'y' must be NULL or of mode numeric" = is.null(y) || mode(y) == "numeric")

    # this works as long as nudge and mapped variable are of the same class
    # ggplot2's behaviour has been in the past and seems to be again to expect
    # numeric seconds for POSIXct and numeric days for Date time shifts
    if (lubridate::is.instant(x)) {
      x <- as.numeric(x)
    }
    if (lubridate::is.instant(y)) {
      y <- as.numeric(y)
    }

    ggplot2::ggproto(NULL, PositionNudgeTo,
                     x = x,
                     y = y,
                     x.action = x.action,
                     y.action = y.action,
                     kept.origin = kept.origin
    )
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionNudgeTo <-
  ggplot2::ggproto(
    "PositionNudgeTo",
    Position,
    x = NULL,
    y = NULL,

    setup_params = function(self, data) {
      list(x = self$x,
           y = self$y,
           x.action = self$x.action,
           y.action = self$y.action,
           x.reorder = !is.null(self$x) && length(self$x) > 1 && length(self$x) < nrow(data),
           y.reorder = !is.null(self$y) && length(self$y) > 1 && length(self$y) < nrow(data),
           kept.origin = self$kept.origin
      )
    },

    compute_layer = function(self, data, params, layout) {
      x_orig <- data$x
      y_orig <- data$y

      # compute/convert x nudges
      if (is.null(params$x)) {
        if (params$x.action == "none") {
          params$x <- rep_len(0, nrow(data))
        } else if (params$x.action == "spread") {
          params$x <- range(x_orig)
        }
      } else if (is.numeric(params$x)) {
        if (length(params$x) > nrow(data)) {
          warning("Argument 'x' longer than data: some values dropped!")
        }
        if (params$x.action == "none") {
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
        # evenly spaced sequence ordered as in data
        params$x <- seq(from = params$x[1],
                        to = params$x[2],
                        length.out = nrow(data))[order(order(data$x))] - x_orig
      }

      # compute/convert y nudges
      if (is.null(params$y)) {
        if (params$y.action == "none") {
          params$y <- rep_len(0, nrow(data))
        } else if (params$y.action == "spread") {
          params$y <- range(y_orig)
        }
      } else if (is.numeric(params$y)) {
        if (params$y.action == "none") {
          if (length(params$y) > nrow(data)) {
            warning("Argument 'y' longer than data: some values dropped!")
          }
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
        # evenly spaced sequence ordered as in data
        params$y <- seq(from = params$y[1],
                        to = params$y[2],
                        length.out = nrow(data))[order(order(data$y))] - y_orig
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
      if (params$kept.origin == "original") {
        data$x_orig <- x_orig
        data$y_orig <- y_orig
      }

      data
    }
  )

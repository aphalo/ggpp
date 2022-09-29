#' Nudge labels to new positions
#'
#' \code{position_nudge_to()} is generally useful for adjusting the position of
#' labels or text, both on a discrete or continuous scale.
#' \code{position_nudge_to()} differs from \code{\link[ggplot2]{position_nudge}}
#' in that the coordinates of the new position are given directly, rather than
#' as a displacement from the original location. As other position functions in
#' this package, it preserves the original position to allow the text to be
#' linked back to its original position with a segment or arrow.
#'
#' @family position adjustments
#'
#' @param x,y Coordinates of the destination position. A numeric vector of
#'   length 1, or of the same length as rows there are in \code{data}. The
#'   default, \code{NULL}, leaves the original coordinates unchanged.
#' @param kept.origin One of \code{"original"} or \code{"none"}.
#'
#' @details The nudged \code{x} or \code{y} replace the original ones in
#'   \code{data}, while the original coordinates are returned in \code{x_orig}
#'   and \code{y_orig}.
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
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text(position = position_nudge_to(y = 3)
#'   )
#'
#' ggplot(df, aes(x, y, label = label)) +
#'   geom_point() +
#'   geom_text_s(position = position_nudge_to(y = 3),
#'                    vjust = -0.2)
#'
position_nudge_to <-
  function(x = NULL,
           y = NULL,
           kept.origin = "original") {

    # Ensure error message is triggered early
    if (!kept.origin %in% c("original", "none")) {
      stop("Invalid 'kept.origin': ", kept.origin,
           "expected: `\"original\" or \"none\"")
    }

    ggplot2::ggproto(NULL, PositionNudgeTo,
                     x = x,
                     y = y,
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
           kept.origin = self$kept.origin
      )
    },

    compute_layer = function(self, data, params, layout) {
      x_orig <- data$x
      y_orig <- data$y
      # compute nudges from user-supplied final positions
      # so that we respect expectations and apply same nudge
      # to xmin, xmax, xend, ymin, ymax, and yend.
      if (is.null(params$x)) {
        params$x <- 0
      } else {
        if (length(params$x == 1L)) {
          params$x <- rep(params$x, nrow(data))
        }
        params$x <- params$x - data$x
      }
      if (is.null(params$y)) {
        params$y <- 0
      } else {
        if (length(params$y == 1L)) {
          params$y <- rep(params$y, nrow(data))
        }
        params$y <- params$y - data$y
      }
      # transform only the dimensions for which new coordinates exist
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

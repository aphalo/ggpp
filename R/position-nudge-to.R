#' Nudge labels to new positions
#'
#' `position_nudge_to()` is generally useful for adjusting the position of
#' labels or text, both on a discrete or continuous scale. This version from
#' package 'ggpmisc' differs from [ggplot2::position_nudge] in that the
#' coordinates of the new position is given directly, rather than as a
#' displacement from the original location. As other position functions in
#' this package, it preserves the original position to allow the text to
#' be linked back to its original position with a segment or arrow.
#'
#' @family position adjustments
#' @param x,y Coordinates of the destination position. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`.
#'   The default, `NULL`, leaves the original coordinates unchanged.
#'
#' @details The new `x` or `y` replace the original ones, while the original
#'   coordinates are returned in `x_orig` and `y_orig`.
#'
#' @seealso [ggplot::position_nudge()], [ggrepel::position_nudge_repel()].
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
#'   geom_text_linked(position = position_nudge_to(y = 3),
#'                    vjust = -0.2)
#'
position_nudge_to <-
  function(x = NULL,
           y = NULL) {
    ggplot2::ggproto(NULL, PositionNudgeTo,
                     x = x,
                     y = y
    )
  }

#' @rdname ggpextra-ggproto
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
           y = self$y)
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
      data$x_orig <- x_orig
      data$y_orig <- y_orig
      data
    }
  )

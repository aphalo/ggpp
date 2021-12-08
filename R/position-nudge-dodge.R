#' Combined positions dodge and nudge
#'
#' `position_dodge_and_nudge()` is useful when labelling plots such as grouped
#' bars, columns, etc. In contrast to [ggplot2::position_nudge],
#' `position_dodge_and_nudge()` returns in `data` both the original coordinates
#' and the nudged coordinates.
#'
#' This position function is backwards compatible with [ggplot2::position_nudge]
#' but extends it by adding support for stacking and for the repulsive
#' geometries from package 'ggrepel'.
#'
#' The wrapper `position_nudge_keep()` with exactly the same signature and
#' behaviour as [ggplot2::position_nudge] provides an easier to remember name
#' when the desire is only to have access to both the original and nudged
#' coordinates.
#'
#' @family position adjustments
#'
#' @param width Dodging width, when different to the width of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. See the examples.
#' @param preserve Should dodging preserve the total width of all elements at a
#'   position, or the width of a single element?.
#' @param padding Padding between elements at the same position. Elements are
#'   shrunk by this proportion to allow space between them. Defaults to 0.1.
#' @param reverse If TRUE, will reverse the default stacking order. This is
#'   useful if you're rotating both the plot and legend.
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`,
#' @param direction One of "none", "split", "split.x" or "split.y". A value of
#'   "none" replicates the behavior of [ggplot2::position_nudge]. At the moment
#'   "split" changes the sign of the nudge at zero, which is suiatble for column
#'   plots with negative slices.
#'
#' @seealso [ggplot2::position_nudge()], [ggrepel::position_nudge_repel()].
#'
#' @return A \code{"Position"} object.
#'
#' @export
#'
#' @author Michał Krassowski \url{http://linkedin.com/in/michal-krassowski/} and
#'   Pedro J. Aphalo.
#'
#' @examples
#' df <- data.frame(x1 = c(1, 2, 1, 3, -1),
#'                  x2 = c("a", "a", "b", "b", "b"),
#'                  grp = c("some long name", "other name", "some name",
#'                          "another name", "some long name"))
#'
#' # Add labels to a horizontal column plot (stacked by default)
#' ggplot(data = df, aes(x1, x2, group = grp)) +
#'   geom_col(aes(fill = grp), width = 1,
#'            position = position_dodge()) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(
#'     aes(label = grp),
#'     position = position_dodge_and_nudge(x = 0.09,
#'                                         direction = "split"),
#'     angle = 90) +
#'   theme(legend.position = "none")
#'
#' ggplot(data = df, aes(x2, x1, group = grp)) +
#'   geom_col(aes(fill = grp), width = 0.75,
#'            position = position_dodge(width = 0.75)) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(
#'     aes(label = grp),
#'     position = position_dodge_and_nudge(y = 0.1,
#'                                         direction = "split",
#'                                         width = 0.75)) +
#'   theme(legend.position = "none")
#'
position_dodge_and_nudge <- function(width = 1,
                                     preserve = c("total", "single"),
                                     x = 0,
                                     y = 0,
                                     direction = "none") {
  ggplot2::ggproto(NULL, PositionDodgeAndNudge,
          x = x,
          y = y,
          .fun_x = switch(direction,
                        none = function(x) {1},
                        split = sign,
                        split.y = function(x) {1},
                        split.x = sign,
                        center = sign,
                        function(x) {1}),
          .fun_y = switch(direction,
                          none = function(x) {1},
                          split = sign,
                          split.x = function(x) {1},
                          split.y = sign,
                          center = sign,
                          function(x) {1}),
          width = width,
          preserve = match.arg(preserve)
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @noRd
PositionDodgeAndNudge <-
  ggplot2::ggproto("PositionDodgeAndNudge", ggplot2::PositionDodge,
          x = 0,
          y = 0,

          setup_params = function(self, data) {
            c(
              list(nudge_x = self$x, nudge_y = self$y,
                   .fun_x = self$.fun_x, .fun_y = self$.fun_y),
              ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$setup_params(data)
            )
          },

          compute_layer = function(self, data, params, layout) {
            # operate on the dodged positions
            data = ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$compute_layer(data, params, layout)

            x_orig <- data$x
            y_orig <- data$y
            # transform only the dimensions for which non-zero nudging is requested
            if (any(params$nudge_x != 0)) {
              if (any(params$nudge_y != 0)) {
                data <- ggplot2::transform_position(data,
                                                    function(x) x + params$nudge_x * params$.fun_x(x),
                                                    function(y) y + params$nudge_y * params$.fun_y(y))
              } else {
                data <- ggplot2::transform_position(data,
                                                    function(x) x + params$nudge_x * params$.fun_x(x),
                                                    NULL)
              }
            } else if (any(params$nudge_y != 0)) {
              data <- ggplot2::transform_position(data,
                                                  function(x) x,
                                                  function(y) y + params$nudge_y * params$.fun_y(y))
            }
            # add original position
            data$x_orig <- x_orig
            data$y_orig <- y_orig

            data
          },

          compute_panel = function(self, data, params, scales) {
            ggplot2::ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)
          }
  )

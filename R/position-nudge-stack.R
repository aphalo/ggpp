#' Combined positions stack and nudge
#'
#' `position_stack_and_nudge()` is useful when labelling plots such as stacked
#' bars, stacked columns, stacked lines, etc. In contrast to
#' [ggplot2::position_nudge], `position_stack_and_nudge()` returns in `data`
#' both the original coordinates and the nudged coordinates.
#'
#' This position function is backwards compatible with [ggplot2::position_nudge]
#' but extends it by adding support for stacking and for the repulsive geometries
#' from package 'ggrepel'.
#'
#' The wrapper `position_nudge_keep()` with exactly the same signature and
#' behaviour as [ggplot2::position_nudge] provides an easier to remember name
#' when the desire is only to have access to both the original and nudged
#' coordinates.
#'
#' @family position adjustments
#'
#' @param vjust Vertical adjustment for geoms that have a position (like points
#'   or lines), not a dimension (like bars or areas). Set to 0 to align with the
#'   bottom, 0.5 for the middle, and 1 (the default) for the top.
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
#' @author Michał Krassowski \url{http://linkedin.com/in/michal-krassowski/}
#'   (revised by Pedro J. Aphalo)
#'
#' @examples
#' df <- data.frame(x1 = c(1, 2, 1, 3, -1),
#'                  x2 = c("a", "a", "b", "b", "b"),
#'                  grp = c("some long name", "other name", "some name",
#'                          "another name", "some long name"))
#'
#' # Add labels to a horizontal column plot (stacked by default)
#' ggplot(data = df, aes(x1, x2, group = grp)) +
#'   geom_col(aes(fill = grp), width=0.5) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(
#'     aes(label = grp),
#'     position = position_stack_and_nudge(vjust = 0.5, y = 0.3)) +
#'   theme(legend.position = "none")
#'
#' # Add labels to a vertical column plot (stacked by default)
#' ggplot(data = df, aes(x2, x1, group = grp)) +
#'   geom_col(aes(fill = grp), width=0.5) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(
#'     aes(label = grp),
#'     position = position_stack_and_nudge(vjust = 0.5, x = -0.3),
#'     angle = 90) +
#'   theme(legend.position = "none")
#'
#' # Add label at a fixed distance from the top of each column slice
#' ggplot(data = df, aes(x2, x1, group = grp)) +
#'   geom_col(aes(fill = grp), width=0.5) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(
#'     aes(label = grp),
#'     position = position_stack_and_nudge(vjust = 1, y = -0.2)) +
#'   theme(legend.position = "none")
#'
#' # Use geom_text_linked(), geom_text_repel() or geom_label_repel() to link
#' # label to labelled segment or object with an arrow
#' ggplot(data = df, aes(x1, x2, group = grp)) +
#'   geom_col(aes(fill = grp), width=0.5) +
#'   geom_vline(xintercept = 0) +
#'   geom_text_linked(
#'     aes(label = grp),
#'     position = position_stack_and_nudge(vjust = 0.5, y = 0.4),
#'     vjust = "bottom") +
#'   theme(legend.position = "none")
#'
position_stack_and_nudge <- function(vjust = 1,
                                     reverse = FALSE,
                                     x = 0,
                                     y = 0,
                                     direction = "none") {
  ggplot2::ggproto(NULL, PositionStackAndNudge,
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
          vjust = vjust,
          reverse = reverse
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @noRd
PositionStackAndNudge <-
  ggplot2::ggproto("PositionStackAndNudge", ggplot2::PositionStack,
          x = 0,
          y = 0,

          setup_params = function(self, data) {
            c(
              list(nudge_x = self$x, nudge_y = self$y,
                   .fun_x = self$.fun_x, .fun_y = self$.fun_y),
              ggplot2::ggproto_parent(ggplot2::PositionStack, self)$setup_params(data)
            )
          },

          compute_layer = function(self, data, params, layout) {
            # operate on the stacked positions (updated in August 2020)
            data = ggplot2::ggproto_parent(ggplot2::PositionStack, self)$compute_layer(data, params, layout)

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
            ggplot2::ggproto_parent(PositionStack, self)$compute_panel(data, params, scales)
          }
  )

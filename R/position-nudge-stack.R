#' Combined positions stack and nudge
#'
#' \code{position_stacknudge()} is useful when labelling plots such as stacked
#' bars, stacked columns, stacked lines, etc. In contrast to
#' \code{\link[ggplot2]{position_nudge}}, \code{position_stacknudge()} returns
#' in \code{data} both the original coordinates and the nudged coordinates.
#'
#' This position function is backwards compatible with
#' \code{\link[ggplot2]{position_nudge}} but extends it by adding support for
#' stacking and for geometries that make use of the original position to draw
#' connecting segments or arrows.
#'
#' The wrapper \code{position_nudge_keep()} with exactly the same signature and
#' behaviour as \code{\link[ggplot2]{position_nudge}} provides an easier to
#' remember name when the desire is only to have access to both the original and
#' nudged coordinates.
#'
#' @family position adjustments
#'
#' @param vjust Vertical adjustment for geoms that have a position (like points
#'   or lines), not a dimension (like bars or areas). Set to 0 to align with the
#'   bottom, 0.5 for the middle, and 1 (the default) for the top.
#' @param reverse If TRUE, will reverse the default stacking order. This is
#'   useful if you're rotating both the plot and legend.
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in \code{data},
#' @param direction One of \code{"none"}, \code{"split"}, \code{"split.x"} or
#'   \code{"split.y"}. A value of \code{"none"} replicates the behavior of
#'   \code{\link[ggplot2]{position_nudge}}. At the moment \code{"split"} changes
#'   the sign of the nudge at zero, which is suitable for column plots with
#'   negative slices.
#' @param kept.origin One of \code{"original"}, \code{"stacked"} or
#'   \code{"none"}.
#'
#' @seealso \code{\link[ggplot2]{position_nudge}},
#'   \code{\link[ggplot2]{position_stack}},
#'   \code{\link[ggrepel]{position_nudge_repel}}.
#'
#' @return A \code{"Position"} object.
#'
#' @export
#'
#' @author Michał Krassowski, edited by Pedro J. Aphalo.
#'
#' @source \url{https://github.com/slowkow/ggrepel/issues/161}.
#'
#' @examples
#'
#' df <- data.frame(x1 = c("a", "a", "b", "b", "b"),
#'                  x2 = c(1, 2, 1, 3, -1),
#'                  grp = c("some long name", "other name", "some name",
#'                          "another name", "some long name"))
#'
#' # Add labels to a horizontal column plot (stacked by default)
#' ggplot(data = df, aes(x1, x2, group = grp)) +
#'   geom_col(aes(fill = grp), width=0.5) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(
#'     aes(label = grp),
#'     position = position_stacknudge(vjust = 0.5, y = 0.3)) +
#'   theme(legend.position = "none")
#'
#' # Add labels to a vertical column plot (stacked by default)
#' ggplot(data = df, aes(x2, x1, group = grp)) +
#'   geom_col(aes(fill = grp), width=0.5) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(
#'     aes(label = grp),
#'     position = position_stacknudge(vjust = 0.5, x = -0.3),
#'     angle = 90) +
#'   theme(legend.position = "none")
#'
#' # Add labels to a vertical column plot (stacked by default)
#' ggplot(data = subset(df, x1 >= 0), aes(x1, x2, group = grp)) +
#'   geom_col(aes(fill = grp), width=0.5, position = position_fill()) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(
#'     aes(label = grp),
#'     position = position_fillnudge(vjust = 0.5, x = -0.3),
#'     angle = 90) +
#'   theme(legend.position = "none")
#'
#' # Add label at a fixed distance from the top of each column slice
#' ggplot(data = df, aes(x1, x2, group = grp)) +
#'   geom_col(aes(fill = grp), width=0.5) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(
#'     aes(label = grp),
#'     position = position_stacknudge(vjust = 1, y = -0.2)) +
#'   theme(legend.position = "none")
#'
#' # Use geom_text_s(), geom_text_repel() or geom_label_repel() to link
#' # label to labelled segment or object with an arrow
#' ggplot(data = df, aes(x2, x1, group = grp)) +
#'   geom_col(aes(fill = grp), width=0.5) +
#'   geom_vline(xintercept = 0) +
#'   geom_text_s(
#'     aes(label = grp),
#'     position = position_stacknudge(vjust = 0.5, y = 0.35),
#'     vjust = "bottom") +
#'   theme(legend.position = "none")
#'
position_stacknudge <-
  function(vjust = 1,
           reverse = FALSE,
           x = 0,
           y = 0,
           direction = c("none", "split", "split.x", "split.y"),
           kept.origin = c("stacked", "original", "none")) {

    direction <- match.arg(direction)
    kept.origin <- match.arg(kept.origin)

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
                     kept.origin = kept.origin,
                     vjust = vjust,
                     reverse = reverse
    )
  }

#' @export
#' @rdname position_stacknudge
position_fillnudge <-
  function(vjust = 1,
           reverse = FALSE,
           x = 0,
           y = 0,
           direction = c("none", "split", "split.x", "split.y"),
           kept.origin = c("stacked", "original", "none")) {

  direction <- match.arg(direction)
  kept.origin <- match.arg(kept.origin)

  ggplot2::ggproto(NULL, PositionFillAndNudge,
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
                   kept.origin = kept.origin,
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
                            .fun_x = self$.fun_x, .fun_y = self$.fun_y,
                            kept.origin = self$kept.origin),
                       ggplot2::ggproto_parent(ggplot2::PositionStack, self)$setup_params(data)
                     )
                   },

                   compute_layer = function(self, data, params, layout) {
                     x_orig <- data$x
                     y_orig <- data$y

                     # operate on the stacked positions (updated in August 2020)
                     data = ggplot2::ggproto_parent(ggplot2::PositionStack, self)$compute_layer(data, params, layout)
                     x_stacked <- data$x
                     y_stacked <- data$y

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

#' @rdname position_stacknudge
#'
#' @export
#'
position_stack_keep <-
  function(vjust = 1,
           reverse = FALSE) {
    position_stacknudge(vjust = vjust,
                        reverse = reverse,
                        x = 0,
                        y = 0,
                        direction = "none",
                        kept.origin = "original")
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionFillAndNudge <-
  ggplot2::ggproto("PositionFillAndNudge", PositionStackAndNudge,
                   fill = TRUE
  )

#' @rdname position_stacknudge
#'
#' @export
#'
position_fill_keep <-
  function(vjust = 1,
           reverse = FALSE) {
    position_fillnudge(vjust = vjust,
                       reverse = reverse,
                       x = 0,
                       y = 0,
                       direction = "none",
                       kept.origin = "original")
  }

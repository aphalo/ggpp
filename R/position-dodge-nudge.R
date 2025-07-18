#' Combined positions dodge and nudge
#'
#' \code{position_dodgenudge()} combines into one function the action of
#' \code{\link[ggplot2]{position_dodge}} and
#' \code{\link[ggplot2]{position_nudge}} and \code{position_dodge2nudge()}
#' combines into one function the action of
#' \code{\link[ggplot2]{position_dodge2}} and
#' \code{\link[ggplot2]{position_nudge}}. They are useful when labelling plots
#' such as grouped bars, columns, etc. and when adding dodged text labels linked
#' to observations plotted without dodge. It can replace other position
#' functions as they are backwards compatible. Like all other position functions
#' in 'ggpp' and 'ggrepel' they preserve the initial position to allow drawing
#' of segments or arrow linking the original position to the displaced one.
#'
#' @details The applied dodge is identical to that by
#'   \code{\link[ggplot2]{position_dodge}} or
#'   \code{\link[ggplot2]{position_dodge2}} while nudging is similar to that by
#'   \code{\link[ggplot2]{position_nudge}}.
#'
#' There are two possible uses for these functions. First they can be used to
#' label dodged bars or boxplots. In this case, it is mandatory to use the same
#' argument to \code{width} when passing \code{position_dodge()} to
#' \code{geom_col()} and \code{position_dodgenudge()} to \code{geom_text()} or
#' \code{geom_label()} or their repulsive equivalents. Otherwise the arrows or
#' segments will fail to connect to the labels. In other words dodging is
#' computed twice. Dodge is identical to that obtained with the same arguments
#' in \code{\link[ggplot2]{position_dodge}} as \code{position_dodgenudge()} last
#' function simply calls the same code from package 'ggplot2' ahead of applying
#' nudging.
#'
#' The second use is to dodge labels to be connected to elements that have not
#' been dodged.
#'
#' When applying dodging, the return of original positions instead of the dodged
#' ones is achieved by passing \code{origin = "original"} instead of the default
#' of \code{origin = "dodged"}.
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
#' @param reverse If TRUE, will reverse the default stacking order. This is
#'   useful if you're rotating both the plot and legend.
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in `data`,
#'   with nudge values in data rows order.
#' @param direction One of \code{"none"}, \code{"split"}, \code{"split.x"} or
#'   \code{"split.y"}. A value of \code{"none"} replicates the behavior of
#'   \code{\link[ggplot2]{position_nudge}}. At the moment \code{"split"} changes
#'   the sign of the nudge at zero, which is suitable for column plots with
#'   negative slices.
#' @param kept.origin One of \code{"original"}, \code{"dodged"} or
#'   \code{"none"}.
#'
#' @seealso \code{\link[ggplot2]{position_nudge}},
#' \code{\link[ggrepel]{position_nudge_repel}}.
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
#' df <- data.frame(x1 = c(1, 2, 1, 3, -1),
#'                  x2 = c("a", "a", "b", "b", "b"),
#'                  grp = c("some long name", "other name", "some name",
#'                          "another name", "some long name"))
#'
#' # Add labels to a horizontal column plot (stacked by default)
#' ggplot(data = df, aes(x1, x2, group = grp)) +
#'   geom_col(aes(fill = grp), width = 0.8,
#'            position = position_dodge()) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(
#'     aes(label = grp),
#'     position = position_dodgenudge(x = 0.09, direction = "split", width = 0.8),
#'     angle = 90, size = 3) +
#'   theme(legend.position = "none")
#'
#' ggplot(data = df, aes(x2, x1, group = grp)) +
#'   geom_col(aes(fill = grp), width = 0.75,
#'            position = position_dodge(width = 0.75)) +
#'   geom_vline(xintercept = 0) +
#'   geom_text(aes(label = grp),
#'             position = position_dodgenudge(y = 0.1,
#'                                            direction = "split",
#'                                            width = 0.75),
#'             size = 3) +
#'   theme(legend.position = "none")
#'
position_dodgenudge <-
  function(width = 1,
           preserve = c("total", "single"),
           reverse = FALSE,
           x = 0,
           y = 0,
           direction = c("none", "split", "split.x", "split.y", "center"),
           kept.origin = c("dodged", "original", "none")) {

    preserve <- rlang::arg_match(preserve)
    direction <- rlang::arg_match(direction)
    kept.origin <- rlang::arg_match(kept.origin)

    ggplot2::ggproto(NULL, PositionDodgeAndNudge,
                     x = x,
                     y = y,
                     .fun_x = switch(direction,
                                     none = function(x) {1},
                                     split = sign,
                                     split.y = function(x) {1},
                                     split.x = sign,
                                     center = sign),
                     .fun_y = switch(direction,
                                     none = function(x) {1},
                                     split = sign,
                                     split.x = function(x) {1},
                                     split.y = sign,
                                     center = sign),
                     kept.origin = kept.origin,
                     width = width,
                     preserve = preserve,
                     reverse = reverse
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
                       list(nudge_x = self$x,
                            nudge_y = self$y,
                            .fun_x = self$.fun_x,
                            .fun_y = self$.fun_y,
                            x.reorder = !is.null(self$x) && length(self$x) > 1 && length(self$x) < nrow(data),
                            y.reorder = !is.null(self$y) && length(self$y) > 1 && length(self$y) < nrow(data),
                            kept.origin = self$kept.origin),
                       ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$setup_params(data)
                     )
                   },

                   compute_layer = function(self, data, params, layout) {

                     if (length(params$nudge_x) > nrow(data)) {
                       warning("Argument 'x' longer than data: some values dropped!")
                     }
                     if (length(params$nudge_y) > nrow(data)) {
                       warning("Argument 'y' longer than data: some values dropped!")
                     }
                     x_orig <- data$x
                     y_orig <- data$y
                     # operate on the dodged positions
                     data = ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$compute_layer(data, params, layout)

                     x_dodged <- data$x
                     y_dodged <- data$y

                     if (params$x.reorder) {
                       params$nudge_x <- rep_len(params$nudge_x, nrow(data))[order(order(data$x))]
                     } else {
                       params$nudge_x <- rep_len(params$nudge_x, nrow(data))
                     }
                     if (params$y.reorder) {
                       params$nudge_y <- rep_len(params$nudge_y, nrow(data))[order(order(data$y))]
                     } else {
                       params$nudge_y <- rep_len(params$nudge_y, nrow(data))
                     }

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

#' @rdname position_dodgenudge
#'
#' @export
#'
position_dodge_keep <- function(width = 1,
                                preserve = c("total", "single"),
                                reverse = FALSE,
                                kept.origin = "original") {
  position_dodgenudge(width = width,
                      preserve = preserve,
                      reverse = reverse,
                      x = 0,
                      y = 0,
                      direction = "none",
                      kept.origin = kept.origin)
}

#' Combined positions jitter and nudge
#'
#' \code{position_jitternudge()} combines into one function the action of
#' \code{\link[ggplot2]{position_jitter}} and
#' \code{\link[ggplot2]{position_nudge}}. It is useful when labels to jittered
#' plots and when adding jitter to text labels linked to points plotted without
#' jitter. It can replace other position functions as it is backwards
#' compatible. Like all other position functions in 'ggpp' and 'ggrepel' it
#' preserves the initial position to allow drawing of segments or arrow linking
#' the original position to the displaced one.
#'
#' @details Jitter with \code{position_jitternudge()} is identical to that with
#'   \code{\link[ggplot2]{position_jitter}} while nudging is enhanced compared
#'   to \code{\link[ggplot2]{position_nudge}} by taking into use cases specific
#'   to the combination of jitter and nudge.
#'
#'   There are two posible uses for this function. First it can be used to label
#'   jittered points in a plot. In this case, it is mandatory to use the same
#'   arguments to \code{width}, \code{height} and \code{seed} when passing
#'   \code{position_jitter()} to \code{geom_point()} and
#'   \code{position_jitternudge()} to \code{geom_text()} or to
#'   \code{geom_label()} or their repulsive equivalents. Otherwise the arrows or
#'   segments will fail to connect to the labels. In other words jittering is
#'   computed twice. Jitter should be identical with the same arguments as
#'   \code{position_jitternudge()} as this last function calls the same code
#'   imported from package 'ggplot2'.
#'
#'   The second use is to jitter labels to be connected to points that have not
#'   been jittered. The return of original positions instead of the jittered
#'   ones is achieved by passing \code{origin = "original"} to override the
#'   default \code{origin = "jittered"}.
#'
#' @family position adjustments
#'
#' @param width,height Amount of vertical and horizontal jitter. The jitter is
#'   added in both positive and negative directions, so the total spread is
#'   twice the value specified here. If omitted, defaults to 40% of the
#'   resolution of the data: this means the jitter values will occupy 80% of the
#'   implied bins. Categorical data is aligned on the integers, so a width or
#'   height of 0.5 will spread the data so it's not possible to see the
#'   distinction between the categories.
#' @param seed A random seed to make the jitter reproducible. Useful if you need
#'   to apply the same jitter twice, e.g., for a point and a corresponding
#'   label. The random seed is reset after jittering. If \code{NA} (the default
#'   value), the seed is initialised with a random value; this makes sure that
#'   two subsequent calls start with a different seed. Use \code{NULL} to use
#'   the current random seed and also avoid resetting (the behaviour of ggplot
#'   2.2.1 and earlier).
#' @param x,y Amount of vertical and horizontal distance to move. A numeric
#'   vector of length 1, or of the same length as rows there are in \code{data},
#'   with nudge values in data rows order.
#' @param direction One of \code{"as.is"}, \code{"alternate"}, \code{"split"},
#'   \code{"split.x"} or \code{"split.y"}. A value of \code{"none"} replicates
#'   the behavior of \code{\link[ggplot2]{position_nudge}}. \code{"split"}
#'   changes the sign of the nudge depending on the direction of the random
#'   jitter applied to each individual observation, which is suitable for
#'   nudging labels outward of the jittered data.
#' @param nudge.from One of \code{"original"}, \code{"jittered"},
#'   \code{"original.y"} (or \code{"jittered.x"}), \code{"original.x"} (or
#'   \code{"jittered.y"}). A value of \code{"original"} applies the nudge before
#'   jittering the observations, while \code{"jittered"} applies the nudging
#'   after jittering.
#' @param kept.origin One of \code{"original"}, \code{"jittered"} or
#'   \code{"none"}.
#'
#' @note When \code{direction = "split"} is used together with no jitter, the
#'   split to left and right, or up and down is done at random.
#'
#' @seealso \code{\link[ggplot2]{position_jitter}},
#' \code{\link[ggplot2]{position_nudge}},
#'  \code{\link[ggrepel]{position_nudge_repel}}.
#'
#' @return A \code{"Position"} object. The layer function within it returns a
#'   data frame, with the jittered + nudged values in columns \code{x} and
#'   \code{y} and by default the jittered values with no nudging as
#'   \code{x_orig} and \code{y_orig}. With \code{nudge.from = "original"} the
#'   original values with no jitter and no nudge applied are returned as
#'   \code{x_orig} and \code{y_orig}.
#'
#' @export
#'
#' @author Michał Krassowski, edited by Pedro J. Aphalo.
#'
#' @source \url{https://github.com/slowkow/ggrepel/issues/161}.
#'
#' @examples
#'
#' jitter <- position_jitter(width = 0.2, height = 2, seed = 123)
#'
#' jitter_nudge <- position_jitternudge(width = 0.2, height = 2,
#'                                      seed = 123, x = 0.1,
#'                                      direction = "split",
#'                                      nudge.from = "jittered")
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point(position = jitter) +
#'   geom_text_s(position = jitter_nudge)
#'
#' jitter_nudge <- position_jitternudge(width = 0.2, height = 2,
#'                                      seed = 123, x = 0.35,
#'                                      direction = "split",
#'                                      nudge.from = "original.x")
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point(position = jitter) +
#'   geom_text_s(position = jitter_nudge)
#'
#' jitter <- position_jitter(width = 0, height = 2, seed = 123)
#'
#' jitter_nudge <- position_jitternudge(width = 0, height = 2,
#'                                      seed = 123, x = 0.4,
#'                                      direction = "split",
#'                                      nudge.from = "original.x")
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point(position = jitter) +
#'   geom_text_s(position = jitter_nudge)
#'
#' jitter_nudge <- position_jitternudge(width = 0, height = 2,
#'                                      seed = 123, x = 0.4,
#'                                      direction = "alternate",
#'                                      nudge.from = "original.x")
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point(position = jitter) +
#'   geom_text_s(position = jitter_nudge)
#'
#' # No nudge, show how points have moved with jitter
#'
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point() +
#'   geom_point_s(position =
#'                position_jitter_keep(width = 0.3, height = 2, seed = 123),
#'                color = "red",
#'                arrow = grid::arrow(length = unit(0.4, "lines")))
#'
position_jitternudge <-
  function(width = NULL,
           height = NULL,
           seed = NA,
           x = 0,
           y = 0,
           direction = c("as.is", "alternate", "split"),
           nudge.from = c("original", "original.x", "original.y",
                          "jittered", "jittered.y", "jittered.x"),
           kept.origin = c("jittered", "original", "none")) {

    direction <- rlang::arg_match(direction)
    nudge.from <- rlang::arg_match(nudge.from)
    kept.origin <- rlang::arg_match(kept.origin)

    fixed.direction <-
    function(x) {1}
  conditional.direction <-
    function(x) {
      # positive or negative jitter
      s <- sign(x)
      if (any(s == 0)) {
        # split with no jitter is at random
        r <- sample(c(-1, 1), size = length(s), replace = TRUE)
        ifelse(s == 0, r, s)
      } else {
        s
      }
    }

  alternate.direction <-
    function(x) {
      rep_len(c(1, -1), length.out = length(x))
    }

  ggplot2::ggproto(NULL, PositionJitterAndNudge,
                   x = x,
                   y = y,
                   .fun_x = switch(direction,
                                   as.is = fixed.direction,
                                   none = fixed.direction,
                                   split = conditional.direction,
                                   split.x = conditional.direction,
                                   split.y = fixed.direction,
                                   alternate = alternate.direction,
                                   alternate.x = alternate.direction,
                                   alternate.y = fixed.direction,
                                   fixed.direction),
                   .fun_y = switch(direction,
                                   as.is = fixed.direction,
                                   none = fixed.direction,
                                   split = conditional.direction,
                                   split.x = fixed.direction,
                                   split.y = conditional.direction,
                                   alternate = alternate.direction,
                                   alternate.x = fixed.direction,
                                   alternate.y = alternate.direction,
                                   fixed.direction),
                   nudge.from = nudge.from,
                   kept.origin = kept.origin,
                   width = width,
                   height = height,
                   seed = seed
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @noRd
PositionJitterAndNudge <-
  ggplot2::ggproto("PositionDodgeAndNudge", ggplot2::PositionJitter,
                   x = 0,
                   y = 0,

                   setup_params = function(self, data) {
                     c(
                       list(nudge_x = self$x, nudge_y = self$y,
                            .fun_x = self$.fun_x, .fun_y = self$.fun_y,
                            nudge.from = self$nudge.from, kept.origin = self$kept.origin),
                       ggplot2::ggproto_parent(ggplot2::PositionJitter, self)$setup_params(data)
                     )
                   },

                   compute_layer = function(self, data, params, layout) {
                     x_orig <- data$x
                     y_orig <- data$y

                     # operate on the dodged positions
                     data = ggplot2::ggproto_parent(ggplot2::PositionJitter, self)$compute_layer(data, params, layout)

                     x_jittered <- data$x
                     y_jittered <- data$y
                     if (params$nudge.from %in% c("original", "original.x", "jittered.y")) {
                       data$x <- x_orig
                     }
                     if (params$nudge.from %in% c("original", "original.y", "jittered.x")) {
                       data$y <- y_orig
                     }

                     # transform only the dimensions for which non-zero nudging is requested
                     if (any(params$nudge_x != 0)) {
                       if (any(params$nudge_y != 0)) {
                         data <- ggplot2::transform_position(data,
                                                             function(x) x + params$nudge_x * params$.fun_x(x_jittered - x_orig),
                                                             function(y) y + params$nudge_y * params$.fun_y(y_jittered - y_orig))
                       } else {
                         data <- ggplot2::transform_position(data,
                                                             function(x) x + params$nudge_x * params$.fun_x(x_jittered - x_orig),
                                                             NULL)
                       }
                     } else if (any(params$nudge_y != 0)) {
                       data <- ggplot2::transform_position(data,
                                                           function(x) x,
                                                           function(y) y + params$nudge_y * params$.fun_y(y_jittered - y_orig))
                     }
                     # add origin position (for connecting arrow or segment)
                     if (params$kept.origin == "jittered") {
                       data$x_orig <- x_jittered
                       data$y_orig <- y_jittered
                     } else if (params$kept.origin == "original") {
                       data$x_orig <- x_orig
                       data$y_orig <- y_orig
                     }

                     data
                   },

                   compute_panel = function(self, data, params, scales) {
                     ggplot2::ggproto_parent(PositionJitter, self)$compute_panel(data, params, scales)
                   }
  )

#' @rdname position_jitternudge
#'
#' @export
#'
position_jitter_keep <- function(width = NULL,
                                 height = NULL,
                                 seed = NA) {
  position_jitternudge(width = width,
                       height = height,
                       seed = seed,
                       x = 0,
                       y = 0,
                       direction = "as.is",
                       nudge.from = "jittered",
                       kept.origin = "original")
}

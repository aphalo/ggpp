#' @rdname position_dodge_and_nudge
#'
#' @export
#'
position_dodge2_and_nudge <- function(width = 1, preserve = c("total", "single"),
                                      x = 0, y = 0) {
  ggplot2::ggproto(NULL, PositionDodgeAndNudge,
          x = x,
          y = y,
          width = width,
          preserve = match.arg(preserve)
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @noRd
PositionDodgeAndNudge <-
  ggplot2::ggproto("PositionDodgeAndNudge", ggplot2::PositionDodge2,
          x = 0,
          y = 0,

          setup_params = function(self, data) {
            c(
              list(nudge_x = self$x, nudge_y = self$y),
              ggplot2::ggproto_parent(ggplot2::PositionDodge2, self)$setup_params(data)
            )
          },

          compute_layer = function(self, data, params, layout) {
            # operate on the dodged positions
            data = ggplot2::ggproto_parent(ggplot2::PositionDodge2, self)$compute_layer(data, params, layout)

            x_orig <- data$x
            y_orig <- data$y
            # transform only the dimensions for which non-zero nudging is requested
            if (any(params$nudge_x != 0)) {
              if (any(params$nudge_y != 0)) {
                data <- ggplot2::transform_position(data,
                                                    function(x) x + params$nudge_x * sign(x),
                                                    function(y) y + params$nudge_y * sign(y))
              } else {
                data <- ggplot2::transform_position(data,
                                                    function(x) x + params$nudge_x * sign(x),
                                                    NULL)
              }
            } else if (any(params$nudge_y != 0)) {
              data <- ggplot2::transform_position(data,
                                                  function(x) x,
                                                  function(y) y + params$nudge_y * sign(y))
            }
            # add original position
            data$x_orig <- x_orig
            data$y_orig <- y_orig

            data
          },

          compute_panel = function(self, data, params, scales) {
            ggplot2::ggproto_parent(PositionDodge2, self)$compute_panel(data, params, scales)
          }
  )

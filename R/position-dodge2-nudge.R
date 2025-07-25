#' @rdname position_dodgenudge
#'
#' @export
#'
position_dodge2nudge <-
  function(width = 1,
           preserve = c("total", "single"),
           padding = 0.1,
           reverse = FALSE,
           x = 0,
           y = 0,
           direction = c("none", "split", "split.x", "split.y", "center"),
           kept.origin = c("dodged", "original", "none")) {

    preserve <- rlang::arg_match(preserve)
    direction <- rlang::arg_match(direction)
    kept.origin <- rlang::arg_match(kept.origin)

    fun_one <- function(x) {1}

    ggplot2::ggproto(NULL, PositionDodge2AndNudge,
                     x = x,
                     y = y,
                     .fun_x = switch(direction,
                                     none = fun_one,
                                     split = sign,
                                     split.y = fun_one,
                                     split.x = sign,
                                     center = sign),
                     .fun_y = switch(direction,
                                     none = fun_one,
                                     split = sign,
                                     split.x = fun_one,
                                     split.y = sign,
                                     center = sign),
                     kept.origin = kept.origin,
                     width = width,
                     preserve = rlang::arg_match(preserve),
                     padding = padding,
                     reverse = reverse
    )
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @noRd
PositionDodge2AndNudge <-
  ggplot2::ggproto("PositionDodge2AndNudge", ggplot2::PositionDodge2,
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
                       ggplot2::ggproto_parent(ggplot2::PositionDodge2, self)$setup_params(data)
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
                     data = ggplot2::ggproto_parent(ggplot2::PositionDodge2, self)$compute_layer(data, params, layout)

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
                     ggplot2::ggproto_parent(PositionDodge2, self)$compute_panel(data, params, scales)
                   }
  )

#' @rdname position_dodgenudge
#'
#' @export
#'
position_dodge2_keep <- function(width = 1,
                                 preserve = c("total", "single"),
                                 kept.origin = "original") {
  position_dodge2nudge(width = width,
                       preserve = preserve,
                       x = 0,
                       y = 0,
                       direction = "none",
                       kept.origin = kept.origin)
}


#' Key glyphs for legends
#'
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`, where
#' `*` stands for the name of the respective key glyph. The key glyphs can be
#' customized for individual geoms by providing a geom with the `key_glyph`
#' argument (see [`layer()`] or examples below.)
#'
#' @return A grid grob.
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @examples
#' p <- ggplot(economics, aes(date, psavert, color = "savings rate"))
#' # key glyphs can be specified by their name
#' p + geom_line(key_glyph = "timeseries")
#'
#' # key glyphs can be specified via their drawing function
#' p + geom_line(key_glyph = draw_key_rect)
#' @name ggpp_draw_key
NULL

#' @export
#' @rdname ggpp_draw_key
draw_key_text_s <- function(data, params, size) {
  data  <- replace_null(unclass(data), label = "a", angle = 0)
  hjust <- compute_just(data$hjust %||% 0.5)
  vjust <- compute_just(data$vjust %||% 0.5)
  just  <- rotate_just(data$angle, hjust, vjust)
  grob  <- titleGrob(
    data$label,
    x = unit(just$hjust, "npc"), y = unit(just$vjust, "npc"),
    angle = data$angle,
    hjust = hjust,
    vjust = vjust,
    gp = gpar(
      col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
      fontfamily = data$family   %||% "",
      fontface   = data$fontface %||% 1,
      fontsize   = (data$size %||% 3.88) * .pt
    ),
    margin = margin(0.1, 0.1, 0.1, 0.1, unit = "lines"),
    margin_x = TRUE, margin_y = TRUE
  )
  attr(grob, "width")  <- convertWidth(grobWidth(grob),   "cm", valueOnly = TRUE)
  attr(grob, "height") <- convertHeight(grobHeight(grob), "cm", valueOnly = TRUE)
  grob
}

#' @export
#' @rdname ggpp_draw_key
draw_key_label_s <- function(data, params, size) {
  data <- replace_null(unclass(data), label = "a", angle = 0)
  params$label.size <- params$label.size %||% 0.25
  hjust <- compute_just(data$hjust %||% 0.5)
  vjust <- compute_just(data$vjust %||% 0.5)
  just  <- rotate_just(data$angle, hjust, vjust)
  padding <- rep(params$label.padding %||% unit(0.25, "lines"), length.out = 4)
  descent <- font_descent(
    family = data$family %||% "",
    face = data$fontface %||% 1,
    size = data$size %||% 3.88
  )
  grob <- labelGrob(
    data$label,
    x = unit(just$hjust, "npc"),
    y = unit(just$vjust, "npc") + descent,
    angle = data$angle,
    just = c(hjust, vjust),
    padding = padding,
    r = params$label.r %||% unit(0.15, "lines"),
    text.gp = gpar(
      col = data$colour %||% "black",
      fontfamily = data$family   %||% "",
      fontface   = data$fontface %||% 1,
      fontsize   = (data$size %||% 3.88) * .pt
    ),
    rect.gp = gpar(
      col  = if (isTRUE(all.equal(params$label.size, 0))) NA else data$colour,
      fill = alpha(data$fill %||% "white", data$alpha),
      lwd  = params$label.size * .pt
    )
  )
  angle  <- deg2rad(data$angle %||% 0)
  text   <- grob$children[[2]]
  width  <- convertWidth(grobWidth(text),   "cm", valueOnly = TRUE)
  height <- convertHeight(grobHeight(text), "cm", valueOnly = TRUE)
  x <- c(0, 0, width, width)
  y <- c(0, height, height, 0)
  attr(grob, "width")  <- diff(range(x * cos(angle) - y * sin(angle)))
  attr(grob, "height") <- diff(range(x * sin(angle) + y * cos(angle)))
  grob
}

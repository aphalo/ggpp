#' @export
#' @rdname geom_text_npc
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
geom_label_npc <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      parse = FALSE,
                      nudge_x = 0,
                      nudge_y = 0,
                      label.padding = grid::unit(0.25, "lines"),
                      label.r = grid::unit(0.15, "lines"),
                      label.size = 0.25,
                      na.rm = FALSE,
                      show.legend = FALSE,
                      inherit.aes = FALSE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.",
           call. = FALSE)
    }
    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabelNpc,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpextra-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLabelNpc <- ggplot2::ggproto("GeomLabelNpc", ggplot2::Geom,
  required_aes = c("npcx", "npcy", "label"),

  default_aes = ggplot2::aes(
    colour = "black", fill = "white", size = 3.88, angle = 0, hjust = "inward",
    vjust = "inward", alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE,
                        label.padding = grid::unit(0.25, "lines"),
                        label.r = grid::unit(0.15, "lines"),
                        label.size = 0.25) {

    data$npcx <- compute_npcx(data$npcx)
    data$npcy <- compute_npcy(data$npcy)

    ranges <- coord$backtransform_range(panel_params)

    data$x <- ranges$x[1] + data$npcx * (ranges$x[2] - ranges$x[1])
    data$y <- ranges$y[1] + data$npcy * (ranges$y[2] - ranges$y[1])

    ggplot2::GeomLabel$draw_panel(data = data,
                                  panel_params = panel_params,
                                  coord = coord,
                                  parse = parse,
                                  na.rm = na.rm,
                                  label.padding = label.padding,
                                  label.r = label.r,
                                  label.size = label.size)
  },

  draw_key =  function(...) {
    grid::nullGrob()
  }
)

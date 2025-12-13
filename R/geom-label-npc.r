#' @export
#' @rdname geom_text_npc
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
#'
geom_label_npc <- function(mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           ...,
                           parse = FALSE,
                           nudge_x = 0,
                           nudge_y = 0,
                           label.padding = grid::unit(0.25, "lines"),
                           label.r = grid::unit(0.15, "lines"),
                           label.size = 0.25,
                           size.unit = "mm",
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = FALSE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position) && position != "identity") {
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
      size.unit = size.unit,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLabelNpc <- ggplot2::ggproto("GeomLabelNpc", ggplot2::Geom,
  required_aes = c("npcx", "npcy", "label"),

  default_aes = ggplot2::aes(
    colour = "black",
    fill = rgb(1, 1, 1, alpha = 0.75), # "white", but occluded data are visible
    family = "",
    size = 3.87,
    angle = 0,
    hjust = "inward",
    vjust = "inward",
    alpha = NA,
    fontface = 1,
    lineheight = 1.2,
    linewidth = 0.5,
    linetype  = "solid"
  ),

  draw_panel = function(self, data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE,
                        label.padding = unit(0.25, "lines"),
                        label.r = unit(0.15, "lines"),
                        label.size = 0.25,
                        size.unit = "mm") {

    data$npcx <- compute_npcx(data$npcx)
    data$npcy <- compute_npcy(data$npcy)

    ranges <- coord$backtransform_range(panel_params)

    data$x <- ranges$x[1] + data$npcx * (ranges$x[2] - ranges$x[1])
    data$y <- ranges$y[1] + data$npcy * (ranges$y[2] - ranges$y[1])

    lab <- data$label

    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y, data$x, data$angle)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x, data$y, data$angle)
    }
    if (!inherits(label.padding, "margin")) {
      label.padding <- rep(label.padding, length.out = 4)
    }

    size.unit <- resolve_text_unit(size.unit)

    grobs <- lapply(1:nrow(data), function(i) {
      row <- data[i, , drop = FALSE]
      labelGrob(lab[i],
                x = grid::unit(row$x, "native"),
                y = grid::unit(row$y, "native"),
                just = c(row$hjust, row$vjust),
                padding = label.padding,
                r = label.r,
                angle = row$angle,
                text.gp = grid::gpar(
                  col = row$colour,
                  fontsize = row$size * size.unit,
                  fontfamily = row$family,
                  fontface = row$fontface,
                  lineheight = row$lineheight
                ),
                rect.gp = grid::gpar(
                  col = if (isTRUE(all.equal(label.size, 0))) NA else row$colour,
                  fill = ggplot2::alpha(row$fill, row$alpha),
                  lwd = label.size * .pt,
                  lty = row$linetype
                )
      )
    })
    class(grobs) <- "gList"

    ggname("geom_label", grid::grobTree(children = grobs))
  },

  draw_key =  function(...) {
    grid::nullGrob()  # geom meant to be used for annotations
  }
)

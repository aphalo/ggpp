#' @export
#' @rdname geom_text_s
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#' @param label.size Size of label border, in mm.
#'
geom_label_s <- function(mapping = NULL,
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
                         add.segments = TRUE,
                         arrow = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }
    # We do not keep the original positions if they will not be used
    position <-
      position_nudge_center(nudge_x, nudge_y,
                            kept.origin = ifelse(add.segments,
                                                 "original", "none"))
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabelS,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      label.padding = label.padding,
      label.r = label.r,
      label.size = label.size,
      add.segments = add.segments,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomLabelS <-
  ggplot2::ggproto("GeomLabelS", ggplot2::Geom,
                   required_aes = c("x", "y", "label"),

                   default_aes = ggplot2::aes(
                     colour = "black",
                     fill = "white",
                     size = 3.88,
                     angle = 0,
                     hjust = 0.5,
                     vjust = 0.5,
                     alpha = NA,
                     family = "",
                     fontface = 1,
                     lineheight = 1.2,
                     segment.linetype = 1,
                     segment.colour = "grey33",
                     segment.size = 0.5,
                     segment.alpha = 1
                   ),

                   draw_panel = function(data, panel_params, coord, #panel_scales,
                                         parse = FALSE,
                                         na.rm = FALSE,
                                         add.segments = TRUE,
                                         arrow = NULL,
                                         label.padding = unit(0.25, "lines"),
                                         label.r = unit(0.15, "lines"),
                                         label.size = 0.25) {

                     add.segments <- add.segments && all(c("x_orig", "y_orig") %in% colnames(data))

                     data$label <- as.character(data$label)
                     data <- subset(data, !is.na(label) & label != "")
                     if (nrow(data) == 0L) {
                       return(nullGrob())
                     }

                     lab <- data$label
                     if (parse) {
                       lab <- parse_safe(lab)
                     }

                     data <- coord$transform(data, panel_params)
                     if (add.segments) {
                       data_orig <- data.frame(x = data$x_orig, y = data$y_orig)
                       data_orig <- coord$transform(data_orig, panel_params)
                     }

                     if (is.character(data$vjust)) {
                       data$vjust <-
                         compute_just2d(data = data,
                                        coord = coord,
                                        panel_params = panel_params,
                                        just = data$vjust,
                                        a = "y", b = "x")
                     }
                     if (is.character(data$hjust)) {
                       data$hjust <-
                         compute_just2d(data = data,
                                        coord = coord,
                                        panel_params = panel_params,
                                        just = data$hjust,
                                        a = "x", b = "y")
                     }

                     label.grobs <- lapply(1:nrow(data), function(i) {
                       row <- data[i, , drop = FALSE]
                       labelGrob(lab[i],
                                 x = unit(row$x, "native"),
                                 y = unit(row$y, "native"),
                                 just = c(row$hjust, row$vjust),
                                 padding = label.padding,
                                 r = label.r,
                                 text.gp = gpar(
                                   col = row$colour,
                                   fontsize = row$size * .pt,
                                   fontfamily = row$family,
                                   fontface = row$fontface,
                                   lineheight = row$lineheight
                                 ),
                                 rect.gp = gpar(
                                   col = if (isTRUE(all.equal(label.size, 0))) NA else row$colour,
                                   fill = alpha(row$fill, row$alpha),
                                   lwd = label.size * .pt
                                 )
                       )
                     })
                     class(label.grobs) <- "gList"

                     if(add.segments) {
                       # create the grobs
                       segment.grobs <-
                         grid::segmentsGrob(
                           x1 = data$x,
                           y1 = data$y,
                           x0 = data_orig$x,
                           y0 = data_orig$y,
                           arrow = arrow,
                           gp = grid::gpar(col = alpha(data$segment.colour,
                                                       data$segment.alpha)))
                       all.grobs <- gList(segment.grobs, label.grobs)
                     } else {
                       all.grobs <- label.grobs
                     }

                     grid::grobTree(children = all.grobs, name = "geom.label.s.panel")

                   },

                   draw_key = draw_key_text
  )

labelGrob <- function(label, x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
                      just = "center", padding = grid::unit(0.25, "lines"), r = grid::unit(0.1, "snpc"),
                      default.units = "npc", name = NULL,
                      text.gp = grid::gpar(), rect.gp = grid::gpar(fill = "white"), vp = NULL) {

  if (length(label) != 1) {
    rlang::abort("label must be of length 1")
  }

  if (!grid::is.unit(x))
    x <- grid::unit(x, default.units)
  if (!grid::is.unit(y))
    y <- grid::unit(y, default.units)

  grid::gTree(label = label, x = x, y = y, just = just, padding = padding, r = r,
        name = name, text.gp = text.gp, rect.gp = rect.gp, vp = vp, cl = "labelgrob")
}

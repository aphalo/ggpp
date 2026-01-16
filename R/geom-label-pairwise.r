#' @rdname geom_text_pairwise
#' @include ggp2-margins.R utilities.R ggpp-legend-draw.R
#'
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#'
#' @aesthetics GeomLabelPairwise
#'
#' @export
#'
geom_label_pairwise <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           ...,
           parse = FALSE,
           nudge_x = 0,
           nudge_y = 0,
           default.colour = NULL,
           default.color = default.colour,
           colour.target = "all",
           color.target = colour.target,
           default.alpha = NA,
           alpha.target = "segment",
           label.padding = grid::unit(0.25, "lines"),
           label.r = grid::unit(0.15, "lines"),
           segment.linewidth = 0.5,
           arrow = NULL,
           size.unit = "mm",
           na.rm = FALSE,
           show.legend = FALSE,
           inherit.aes = FALSE) {

    colour.target <-
      rlang::arg_match(color.target,
                       values = c("all", "text", "box", "box.line",
                                  "segment", "none"),
                       multiple = TRUE)
    alpha.target <-
      rlang::arg_match(alpha.target,
                       values = c("all", "text", "box", "box.line", "box.fill",
                                  "segment", "none"),
                       multiple = TRUE)

    if (!missing(nudge_x) || !missing(nudge_y)) {
      if (!missing(position) && position != "identity") {
        rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
      }
      # by default we do not keep the original positions
      position <- ggplot2::position_nudge(nudge_x, nudge_y)
    }

    ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLabelPairwise,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      default.colour = default.color,
      colour.target = colour.target,
      default.alpha = default.alpha,
      alpha.target = alpha.target,
      label.padding = label.padding,
      label.r = label.r,
      segment.linewidth = segment.linewidth,
      arrow = arrow,
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
GeomLabelPairwise <-
  ggplot2::ggproto("GeomLabelPairwise", ggplot2::Geom,
                   required_aes = c("xmin", "xmax", "y", "label"),

                   default_aes = ggplot2::aes(
                     x = NA_real_,
                     colour = "black",
                     fill = rgb(1, 1, 1, alpha = 0.75), # "white", but occluded data are visible
                     family = "",
                     size = 3.87,
                     angle = 0,
                     hjust = 0.5,
                     vjust = 0.5,
                     alpha = NA,
                     fontface = 1,
                     lineheight = 1.2,
                     linewidth = 0.5,
                     linetype  = "solid"
                   ),

                   draw_panel = function(data, panel_params, coord, #panel_scales,
                                         parse = FALSE,
                                         na.rm = FALSE,
                                         size.unit = "mm",
                                         default.colour = NULL,
                                         colour.target = "all",
                                         default.alpha = NA,
                                         alpha.target = "fill",
                                         segment.linewidth = 0.5,
                                         arrow = NULL,
                                         label.padding = unit(0.25, "lines"),
                                         label.r = unit(0.15, "lines")) {

                     default.colour <- check_default_colour(default.colour)

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
                     if (all(c("x_orig", "y_orig") %in% colnames(data))) {
                       data_orig <- data.frame(x = data$x_orig, y = data$y_orig)
                       data_orig <- coord$transform(data_orig, panel_params)
                       data$x_orig <- data_orig$x
                       data$y_orig <- data_orig$y
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

                     size.unit <- resolve_text_unit(size.unit)

                     if (!inherits(label.padding, "margin")) {
                       label.padding <- rep(label.padding, length.out = 4)
                     }

                     # loop needed as gpar is not vectorized
                     all.grobs <- grid::gList()

                     for (row.idx in 1:nrow(data)) {
                       row <- data[row.idx, , drop = FALSE]
                       if (is.na(row$x)) {
                         row$x <- (row$xmin + row$xmax) / 2
                       }
                       text.alpha <-
                         ifelse(any(alpha.target %in% c("all", "text")),
                                row$alpha, default.alpha)
                       segment.alpha <-
                         ifelse(any(alpha.target %in% c("all", "segment")),
                                row$alpha, default.alpha)
                       box.colour.alpha <-
                         ifelse(any(alpha.target %in% c("all", "box", "box.line")),
                                row$alpha, default.alpha)
                       box.fill.alpha <-
                         ifelse(any(alpha.target %in% c("all", "box", "box.fill")),
                                row$alpha, default.alpha)
                       user.grob <- labelGrob(lab[row.idx],
                                              x = unit(row$x, "native"),
                                              y = unit(row$y, "native"),
                                              just = c(row$hjust, row$vjust),
                                              padding = label.padding,
                                              r = label.r,
                                              angle = row$angle,
                                              text.gp = gpar(
                                                col = ifelse(any(colour.target %in% c("all", "text")),
                                                             ggplot2::alpha(row$colour, text.alpha),
                                                             ggplot2::alpha(default.colour, text.alpha)),
                                                fontsize = row$size * size.unit,
                                                fontfamily = row$family,
                                                fontface = row$fontface,
                                                lineheight = row$lineheight
                                              ),
                                              rect.gp = gpar(
                                                col = if (row$linewidth == 0) NA else # lwd = 0 is invalid in 'grid'
                                                  ifelse(any(colour.target %in% c("all", "box")),
                                                         ggplot2::alpha(row$colour, box.colour.alpha),
                                                         ggplot2::alpha(default.colour, box.colour.alpha)),
                                                fill = alpha(row$fill, box.fill.alpha),
                                                lwd = (if (row$linewidth == 0) 1 else row$linewidth) * .pt, # mm -> points (as in 'ggplot2')
                                                # lwd = (if (row$linewidth == 0) 0.5 else row$linewidth) * ggplot2::.stroke, # mm -> stroke (correct)
                                                lty = row$linetype
                                              )
                       )

                       # give unique name to each grob
                       user.grob$name <- paste("label.pw.grob", row$group, row.idx, sep = ".")

                       if (!is.null(arrow) && !inherits(arrow, "arrow")) {
                         shape <- arrow[[1]]
                         arrow <- NULL
                       } else {
                         shape <- NULL
                       }

                       segment.grob <-
                         grid::segmentsGrob(x0 = row$xmin,
                                            y0 = row$y,
                                            x1 = row$xmax,
                                            y1 = row$y,
                                            arrow = arrow,
                                            gp = grid::gpar(
                                              col = if (segment.linewidth == 0) NA else # lwd = 0 is invalid in 'grid'
                                                ifelse(any(colour.target %in% c("all", "segment")),
                                                       ggplot2::alpha(row$colour, segment.alpha),
                                                       ggplot2::alpha(default.colour, segment.alpha)),
                                              lwd = (if (segment.linewidth == 0) 0.5 else segment.linewidth) * ggplot2::.stroke),
                                            name = paste("text.pw.segment", row$group, row.idx, sep = "."))
                       all.grobs <- grid::gList(all.grobs, segment.grob, user.grob)
                     }


                     # name needs to be unique within plot, so we would have to know layer name to use as basis
                     #                     grid::grobTree(children = all.grobs, name = "geom.text.s.panel")
                     grid::grobTree(children = all.grobs)

                   },

                   draw_key = draw_key_label_s
  )

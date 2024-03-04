#' @rdname geom_text_s
#'
#' @param label.padding Amount of padding around label. Defaults to 0.25 lines.
#' @param label.r Radius of rounded corners. Defaults to 0.15 lines.
#'
#' @export
#'
geom_label_s <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           ...,
           parse = FALSE,
           nudge_x = 0,
           nudge_y = 0,
           default.colour = "black",
           default.color = default.colour,
           colour.target = c("text", "box"),
           color.target = colour.target,
           default.alpha = 1,
           alpha.target = "all",
           label.padding = grid::unit(0.25, "lines"),
           label.r = grid::unit(0.15, "lines"),
           segment.linewidth = 0.5,
           add.segments = TRUE,
           box.padding = 1e-06,
           point.padding = 1e-06,
           min.segment.length = 0,
           arrow = NULL,
           size.unit = "mm",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE) {

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
      # by default we keep the original positions
      position <- position_nudge_keep(nudge_x, nudge_y)
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
      default.colour = default.color,
      colour.target = colour.target,
      default.alpha = default.alpha,
      alpha.target = alpha.target,
      label.padding = label.padding,
      label.r = label.r,
      segment.linewidth = segment.linewidth,
      add.segments = add.segments,
      box.padding = box.padding,
      point.padding = point.padding,
      min.segment.length = min.segment.length,
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
GeomLabelS <-
  ggplot2::ggproto("GeomLabelS", ggplot2::Geom,
                   required_aes = c("x", "y", "label"),

                   default_aes = ggplot2::aes(
                     colour = "black",
                     fill = "white",
                     size = 3.88,
                     angle = 0, # currently ignored
                     linewidth = 0.25,
                     linetype = "solid",
                     hjust = "position",
                     vjust = "position",
                     alpha = 0.75, # ensure that occluded data are visible by default
                     family = "",
                     fontface = 1,
                     lineheight = 1.2
                   ),

                   draw_panel = function(data, panel_params, coord, #panel_scales,
                                         parse = FALSE,
                                         na.rm = FALSE,
                                         size.unit = "mm",
                                         add.segments = TRUE,
                                         default.colour = "black",
                                         colour.target = "all",
                                         default.alpha = 1,
                                         alpha.target = "fill",
                                         box.padding = 0.25,
                                         point.padding = 1e-06,
                                         min.segment.length = 0,
                                         segment.linewidth = 0.5,
                                         arrow = NULL,
                                         label.padding = unit(0.25, "lines"),
                                         label.r = unit(0.15, "lines")) {

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

                     if (add.segments) {
                       segments.data <-
                         shrink_segments(data,
                                         point.padding = point.padding,
                                         box.padding = box.padding,
                                         min.segment.length = min.segment.length)
                     }
                     # loop needed as gpar is not vectorized
                     all.grobs <- grid::gList()

                     for (row.idx in 1:nrow(data)) {
                       row <- data[row.idx, , drop = FALSE]
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
                                                # lwd = (if (row$linewidth == 0) 1 else row$linewidth) * .pt, # mm -> points (as in 'ggplot2')
                                                lwd = (if (row$linewidth == 0) 0.5 else row$linewidth) * ggplot2::.stroke, # mm -> stroke (correct)
                                                lty = row$linetype
                                              )
                       )

                       # give unique name to each grob
                       user.grob$name <- paste("text.s.grob", row$group, row.idx, sep = ".")

                       if (add.segments) {
                         segment.row <- segments.data[row.idx, , drop = FALSE]
                         if (segment.row$too.short) {
                           segment.grob <- grid::nullGrob()
                         } else {
                           segment.grob <-
                             grid::segmentsGrob(x0 = segment.row$x,
                                                y0 = segment.row$y,
                                                x1 = segment.row$x_orig,
                                                y1 = segment.row$y_orig,
                                                arrow = arrow,
                                                gp = grid::gpar(
                                                  col = if (segment.linewidth == 0) NA else # lwd = 0 is invalid in 'grid'
                                                    ifelse(any(colour.target %in% c("all", "segment")),
                                                           ggplot2::alpha(row$colour, segment.alpha),
                                                           ggplot2::alpha(default.colour, segment.alpha)),
                                                  lwd = (if (segment.linewidth == 0) 0.5 else segment.linewidth) * ggplot2::.stroke),
                                                name = paste("text.s.segment", row$group, row.idx, sep = "."))
                         }
                         all.grobs <- grid::gList(all.grobs, segment.grob, user.grob)
                       } else {
                         all.grobs <- grid::gList(all.grobs, user.grob)
                       }
                     }

                     # name needs to be unique within plot, so we would to know layer
                     #                     grid::grobTree(children = all.grobs, name = "geom.text.s.panel")
                     grid::grobTree(children = all.grobs)

                   },

                   draw_key = ggplot2::draw_key_text
  )

labelGrob <- function(label, x = grid::unit(0.5, "npc"), y = grid::unit(0.5, "npc"),
                      just = "center", padding = grid::unit(0.25, "lines"), r = grid::unit(0.1, "snpc"),
                      angle = NULL, default.units = "npc", name = NULL,
                      text.gp = grid::gpar(), rect.gp = grid::gpar(fill = "white"), vp = NULL) {

  if (length(label) != 1) {
    rlang::abort("label must be of length 1")
  }

  if (!grid::is.unit(x))
    x <- grid::unit(x, default.units)
  if (!grid::is.unit(y))
    y <- grid::unit(y, default.units)

  if (!is.null(angle) & is.null(vp)) {
    vp <- grid::viewport(
      angle = angle, x = x, y = y,
      width = grid::unit(0, "cm"), height = grid::unit(0, "cm"),
      gp = grid::gpar(fontsize = text.gp$fontsize)
    )
    x <- grid::unit(rep(0.5, length(x)), "npc")
    y <- grid::unit(rep(0.5, length(y)), "npc")
  }

  descent <- font_descent(
    text.gp$fontfamily, text.gp$fontface, text.gp$fontsize, text.gp$cex
  )
  hjust <- grid::resolveHJust(just, NULL)
  vjust <- grid::resolveVJust(just, NULL)

  text <- titleGrob(
    label = label, hjust = hjust, vjust = vjust, x = x, y = y,
    margin = padding,
    margin_x = TRUE, margin_y = TRUE,
    gp = text.gp
  )

  box <- grid::roundrectGrob(
    x = x, y = y - (1 - vjust) * descent,
    width  = grid::widthDetails(text),
    height = grid::heightDetails(text),
    just   = c(hjust, vjust),
    r = r, gp = rect.gp, name = "box"
  )

  grid::gTree(children = grid::gList(box, text), name = name, vp = vp)
}

#' Label pairwise comparisons
#'
#' @description Add a plot layer with a text label and a segment connecting two
#'   values along the \code{x} aesthetic. These are usually two levels of a
#'   factor mapped to the \code{x} aesthetic when used to report significance or
#'   highlighting pairwise comparisons.
#'
#' @section Under development!: This geometry is still under development
#'   and its user interface subject to change.
#'
#' @details Geometries \code{geom_text_pairwise()} and
#'   \code{geom_label_pairwise()} have an interface similar to that of
#'   \code{\link[ggplot2]{geom_text}} and \code{\link[ggplot2]{geom_label}}, but
#'   add a segment connecting two values along \code{x}. In the most
#'   frequent use case they add a segment connecting pairs of levels from a
#'   grouping factor mapped to the \emph{x} or \emph{y} aesthetic. They can
#'   also be used to label ranges of values.
#'
#'   The segment extends from \code{xmin} to \code{xmax}, and the text label is
#'   located at \code{x} with a default that positions the label at the centre
#'   of the bar. The ends of the bar can be terminated with arrow heads given
#'   by parameter \code{arrow}, with a default of a plain segment without
#'   arrow tips. The text label is located slightly above the segment by the
#'   default value of \code{vjust} in \code{geom_text_pairwise()} and on top
#'   of the segment in \code{geom_label_pairwise()}.
#'
#'   Layer functions \code{geom_text_pairwise()} and
#'   \code{geom_label_pairwise()} use by default
#'   \code{\link[ggplot2]{position_nudge}}. Nudging affects both text label and
#'   bar, and its default of no displacement will very rarely need to be
#'   changed.
#'
#'   Differently to \code{geom_text_repel()} and \code{geom_label_repel()},
#'   \code{geom_text_pairwise()} and \code{geom_label_pairwise()} do not make
#'   use of additional aesthetics for the segments or boxes, but instead allow
#'   the choice of which elements are targeted by the usual 'ggplot2' aesthetics
#'   and which are rendered using a default constant value. In the grammar of
#'   graphics using the same aesthetic with multiple meanings is not allowed,
#'   thus, the approach used in package 'ggpp' attempts to enforce this.
#'
#' @section Plot boundaries and clipping: Note that when you change the scale
#'   limits for \emph{x} and/or \emph{y} of a plot, text labels stay the same
#'   size, as determined by the \code{size} aesthetic, given in millimetres. The
#'   actual size as seen in the plotted output is decided during the rendering
#'   of the plot to a graphics device. Limits are expanded only to include the
#'   anchor point of the labels because the "width" and "height" of a text
#'   element are 0 (as seen by ggplot2). Text labels do have height and width,
#'   but in grid units, not data units. Either function
#'   \code{\link[ggplot2]{expand_limits}} or the scale expansion can be used to
#'   ensure text labels remain within the plotting area.
#'
#' @section Alignment: You can modify text alignment with the \code{vjust} and
#'   \code{hjust} aesthetics. These can either be a number between 0
#'   (right/bottom) and 1 (top/left) or a character (\code{"left"},
#'   \code{"middle"}, \code{"right"}, \code{"bottom"}, \code{"center"},
#'   \code{"top"}). Values outside the range 0..1 displace the text label so
#'   that the anchor point is outside the text label. In addition, you can use
#'   special alignments for justification including \code{"position"},
#'   \code{"inward"} and \code{"outward"}. Inward always aligns text towards the
#'   center of the plotting area, and outward aligns it away from the center of
#'   the plotting area. If tagged with \code{_mean} or \code{_median} (e.g.,
#'   \code{"outward_mean"}) the mean or median of the data in the panel along
#'   the corresponding axis is used as center. If the characters following the
#'   underscore represent a number (e.g., \code{"outward_10.5"}) the reference
#'   point will be this value in data units. Position justification is computed
#'   based on the direction of the displacement of the position of the label so
#'   that each individual text or label is justified outwards from its original
#'   position. The default justification is \code{"identity"}.
#'
#' @param mapping Set of aesthetic mappings created by
#'   \code{\link[ggplot2]{aes}}. With \code{inherit.aes = FALSE}
#'   (the default) it is not combined with the default mapping at the top level of
#'   the plot. You always need to supply a \code{mapping} unless you set
#'   \code{inherit.aes = TRUE}.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param parse If \code{TRUE}, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA} includes a legend if any aesthetics are mapped.
#'   \code{FALSE}, the default, never includes it, and \code{TRUE} always includes it.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining them.
#' @param check_overlap If \code{TRUE}, text that overlaps previous text in the
#'   same layer will not be plotted. \code{check_overlap} takes place at draw
#'   time and in the order of the data, thus its action depends of the size at
#'   which the plot is drawn.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There
#'   are three types of arguments you can use here:
#'
#'   \itemize{ \item Aesthetics: to set an aesthetic to a fixed value, like
#'   \code{colour = "red"} or \code{size = 3}. \item Other arguments to the
#'   layer, for example you override the default \code{stat} associated with the
#'   layer. \item Other arguments passed on to the stat. }
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label. The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param default.colour,default.color A colour definition to use for elements
#'   not targeted by the colour aesthetic.
#' @param colour.target,color.target A vector of character strings; \code{"all"},
#'   \code{"text"}, \code{"segment"}, \code{"box"}, \code{"box.line"}, and
#'   \code{"box.fill"} or \code{"none"}.
#' @param default.alpha numeric in [0..1] A transparency value to use for
#'   elements not targeted by the alpha aesthetic.
#' @param alpha.target A vector of character strings; \code{"all"},
#'   \code{"text"}, \code{"segment"}, \code{"box"}, \code{"box.line"}, and
#'   \code{"box.fill"} or \code{"none"}.
#' @param segment.linewidth numeric Width of the segments or arrows in mm.
#' @param arrow specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}
#'
#' @section Aesthetics: Layer functions \code{geom_text_pairwise()} and
#'   \code{geom_label_pairwise()} require aesthetics \code{xmin}, \code{xmax},
#'   \code{x}, \code{y} and
#'   \code{label} and support aesthetics: \code{alpha}, \code{colour},
#'   \code{group}, \code{size} (of text), \code{family}, \code{fontface},
#'   \code{linewidth}, \code{linetype}, \code{hjust} and \code{vjust}. In addition,
#'   \code{geom_text_pairwise} supports \code{angle} and \code{geom_label_pairwise} supports
#'   \code{fill}. See
#'   \code{\link[ggplot2]{aes_colour_fill_alpha}},
#'   \code{\link[ggplot2]{aes_linetype_size_shape}},
#'   \code{\link[ggplot2]{aes_position}}, and
#'   \code{\link[ggplot2]{aes_group_order}}.
#'
#'   In 'ggplot2' \code{linewidth} when applied to the border of the box drawn
#'   by \code{geom_label()} is given in points rather than in mm because of a
#'   historical error in the code. In other geometries such as
#'   \code{geom_segment()} \code{linewidth} is given in mm. As in
#'   \code{geom_label_pairwise()} it is important to remain consistent among
#'   different \code{linewidth} specifications, mm are used both for the box
#'   border and linkig segment. To imitate the behaviour of `geom_label()` a
#'   correction factor of 0.75 (more exactly 1 pt = 0.7528 mm) can be used the
#'   border of the box.
#'
#' @seealso \code{\link{geom_text_s}}, \code{\link{geom_label_s}},
#'   \code{\link[ggplot2]{geom_text}}, \code{\link[ggplot2]{geom_label}} and
#'   other documentation of package 'ggplot2'.
#'
#' @return A plot layer instance.
#'
#' @export
#'
#' @examples
#'
#' my.cars <- mtcars
#' my.cars$name <- rownames(my.cars)
#' p1 <- ggplot(my.cars, aes(factor(cyl), mpg)) +
#'        geom_boxplot(width = 0.33)
#'
#' # With a factor mapped to x, highlight pairs
#'
#' my.pairs <-
#'   data.frame(A = 1:2, B = 2:3, bar.height = c(12, 30),
#'              p.value = c(0.01, 0.05678))
#' p1 +
#'   geom_text_pairwise(data = my.pairs,
#'                      aes(xmin = A, xmax = B,
#'                          y = bar.height,
#'                          label = p.value),
#'                      parse = TRUE)
#'
#' p1 +
#'   geom_text_pairwise(data = my.pairs,
#'                      aes(xmin = A, xmax = B,
#'                          y = bar.height,
#'                          label = sprintf("italic(P)~`=`~%.2f", p.value)),
#'                      arrow = grid::arrow(angle = 90,
#'                                          length = unit(1, "mm"),
#'                                          ends = "both"),
#'                      parse = TRUE)
#'
#' p1 +
#'   geom_text_pairwise(data = my.pairs,
#'                      aes(xmin = A, xmax = B,
#'                          y = bar.height,
#'                          label = sprintf("italic(P)~`=`~%.2f", p.value)),
#'                      colour = "red",
#'                      arrow = grid::arrow(angle = 90,
#'                                          length = unit(1, "mm"),
#'                                          ends = "both"),
#'                      parse = TRUE)
#'
#' p1 +
#'   geom_label_pairwise(data = my.pairs,
#'                       aes(xmin = A, xmax = B,
#'                           y = bar.height,
#'                           label = sprintf("italic(P)~`=`~%.2f", p.value)),
#'                       colour = "red", size = 2.75,
#'                       arrow = grid::arrow(angle = 30,
#'                                           length = unit(1.5, "mm"),
#'                                           ends = "both"),
#'                       parse = TRUE)
#'
#' p1 +
#'   geom_text_pairwise(data = my.pairs,
#'                      aes(xmin = A, xmax = B,
#'                          y = bar.height,
#'                          label = sprintf("italic(P)~`=`~%.2f", p.value)),
#'                      colour = "red", colour.target = "segment",
#'                      arrow = grid::arrow(angle = 90,
#'                                          length = unit(1, "mm"),
#'                                          ends = "both"),
#'                      parse = TRUE)
#'
#' p1 +
#'   geom_text_pairwise(data = my.pairs,
#'                      aes(xmin = A, xmax = B,
#'                          y = bar.height,
#'                          label = sprintf("italic(P)~`=`~%.2f", p.value)),
#'                      colour = "red", colour.target = "text",
#'                      arrow = grid::arrow(angle = 90,
#'                                          length = unit(1, "mm"),
#'                                          ends = "both"),
#'                      parse = TRUE)
#'
#' # with a numeric vector mapped to x, indicate range
#'
#' p2 <-
#'   ggplot(my.cars, aes(disp, mpg)) +
#'     geom_point()
#'
#' my.ranges <-
#'   data.frame(A = c(50, 400),
#'              B = c(200, 500),
#'              bar.height = 5,
#'              text = c("small", "large"))
#'
#' p2 +
#'   geom_text_pairwise(data = my.ranges,
#'                      aes(xmin = A, xmax = B,
#'                      y = bar.height, label = text))
#'
#' p2 +
#'   geom_label_pairwise(data = my.ranges,
#'                       aes(xmin = A, xmax = B,
#'                       y = bar.height, label = text))
#'
#' p2 +
#'   geom_text_pairwise(data = my.ranges,
#'                      aes(xmin = A, xmax = B,
#'                          y = bar.height, label = text),
#'                      arrow = grid::arrow(ends = "both", length = unit(2, "mm")))
#'
geom_text_pairwise <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               ...,
                               parse = FALSE,
                               nudge_x = 0,
                               nudge_y = 0,
                               default.colour = "black",
                               default.color = default.colour,
                               colour.target = "all",
                               color.target = colour.target,
                               default.alpha = 1,
                               alpha.target = "all",
                               segment.linewidth = 0.5,
                               arrow = NULL,
                               check_overlap = FALSE,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = FALSE) {

  colour.target <-
    rlang::arg_match(color.target,
                     values = c("all", "text", "segment", "none"),
                     multiple = TRUE)
  alpha.target <-
    rlang::arg_match(alpha.target,
                     values = c("all", "text", "segment", "none"),
                     multiple = TRUE)


  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position) && position != "identity") {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }
    # original position needed for "position" justification
    position <-
      ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextPairwise,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      default.colour = default.color,
      colour.target = colour.target,
      default.alpha = default.alpha,
      alpha.target = alpha.target,
      segment.linewidth = segment.linewidth,
      arrow = arrow,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTextPairwise <-
  ggplot2::ggproto("GeomTextPairwise", ggplot2::Geom,
                   required_aes = c("xmin", "xmax", "y", "label"),

                   default_aes = ggplot2::aes(
                     x = NA_real_,
                     colour = "black",
                     size = 3.88,
                     angle = 0,
                     hjust = 0.5,
                     vjust = -0.4,
                     alpha = 1,
                     family = "",
                     fontface = 1,
                     lineheight = 1.2
                   ),

                   draw_panel = function(data,
                                         panel_params,
                                         coord, #panel_scales,
                                         parse = FALSE,
                                         default.colour = "black",
                                         colour.target = "all",
                                         default.alpha = 1,
                                         alpha.target = "all",
                                         na.rm = FALSE,
                                         check_overlap = FALSE,
                                         segment.linewidth = 0.5,
                                         arrow = NULL) {

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
                       user.grob <- grid::textGrob(
                         lab[row.idx],
                         x = row$x, y = row$y, default.units = "native",
                         hjust = row$hjust, vjust = row$vjust,
                         rot = row$angle,
                         gp = gpar(
                           col = ifelse(any(colour.target %in% c("all", "text")),
                                        ggplot2::alpha(row$colour, text.alpha),
                                        ggplot2::alpha(default.colour, text.alpha)),
                           fontsize = row$size * .pt,
                           fontfamily = row$family,
                           fontface = row$fontface,
                           lineheight = row$lineheight
                         ),
                         check.overlap = check_overlap
                       )

                       # give unique name to each grob
                       user.grob$name <- paste("text.pairwise.grob", row$group, row.idx, sep = ".")

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
                                            name = paste("text.s.segment", row$group, row.idx, sep = ".")
                         )
                       all.grobs <- grid::gList(all.grobs, segment.grob, user.grob)

                     }

                     # name needs to be unique within plot, so we would need to know other layers
                     #                     grid::grobTree(children = all.grobs, name = "geom.text.pairwise.panel")
                     grid::grobTree(children = all.grobs)

                   },

                   draw_key = draw_key_text
  )

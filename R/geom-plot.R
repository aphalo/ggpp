#' Inset plots
#'
#' \code{geom_plot} and \code{geom_plot_npc} add ggplot objects as insets to the
#' base ggplot, using syntax similar to that of
#' \code{\link[ggplot2]{geom_label}}  and \code{\link{geom_text_s}}.
#' In most respects they behave as any other ggplot geometry: they add a layer
#' containing one or more grobs and grouping and faceting works as usual. The
#' most common use of \code{geom_plot} is to add data labels that are themselves
#' ggplots rather than text. \code{\link{geom_plot_npc}} is used to add ggplots
#' as annotations to plots, but contrary to layer function \code{annotate()},
#' \code{\link{geom_plot_npc}} is data driven and respects grouping and facets,
#' thus plot insets can differ among panels.
#'
#' @details You can modify the size of inset plots with the \code{vp.width} and
#'   \code{vp.height} aesthetics. These can take a number between 0 (smallest
#'   possible inset) and 1 (whole plotting area width or height). The default
#'   value for for both of these aesthetics is 1/5. Thus, in contrast to
#'   \code{\link[ggplot2]{geom_text}} and \code{\link{geom_text_s}} the size of
#'   the insets remains the same relative to the size of the plotting area
#'   irrespective of how the plot is rendered. The aspect ratio of insets is
#'   preserved and size is adjusted until the whole inset fits within the
#'   viewport.
#'
#'   By default this geom uses \code{\link{position_nudge_center}} which is
#'   backwards compatible with \code{\link[ggplot2]{position_nudge}} but
#'   provides additional control on the direction of the nudging. In contrast to
#'   \code{\link[ggplot2]{position_nudge}}, \code{\link{position_nudge_center}}
#'   and all other position functions defined in packages 'ggpp' and 'ggrepel'
#'   keep the original coordinates thus allowing the plotting of connecting
#'   segments and arrows.
#'
#'   This geom works only with tibbles as \code{data}, as its expects a list of
#'   ggplot objects (\code{"gg"} class) to be mapped to the \code{label}
#'   aesthetic.
#'
#'   The \code{x} and \code{y} aesthetics determine the position of the whole
#'   inset plot, similarly to that of a text label, justification is interpreted
#'   as indicating the position of the plot with respect to its x and y
#'   coordinates in the data, and \code{angle} is used to rotate the plot as a
#'   whole.
#'
#'   In the case of \code{geom_plot_npc()}, \code{npcx} and \code{npcy}
#'   aesthetics determine the position of the inset plot. Justification as
#'   described above for .
#'
#' @inheritSection geom_text_s Alignment
#'
#' @inheritSection geom_text_s Position functions
#'
#' @inherit geom_grob return note seealso references
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you
#'   are overriding the plot defaults.
#' @param data A layer specific data set - only needed if you want to override
#'   the plot defaults.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label. The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param default.colour A colour definition to use for elements not targeted by
#'   the colour aesthetic.
#' @param colour.target A vector of character strings; \code{"all"},
#'   \code{"text"}, \code{"box"} and \code{"segment"}.
#' @param default.alpha numeric in [0..1] A transparency value to use for
#'   elements not targeted by the alpha aesthetic.
#' @param alpha.target A vector of character strings; \code{"all"},
#'   \code{"text"}, \code{"segment"}, \code{"box"}, \code{"box.line"}, and
#'   \code{"box.fill"}.
#' @param add.segments logical Display connecting segments or arrows between
#'   original positions and displaced ones if both are available.
#' @param box.padding,point.padding numeric By how much each end of the segments
#'   should shortened in mm.
#' @param segment.linewidth numeric Width of the segments or arrows in mm.
#' @param min.segment.length numeric Segments shorter that the minimum length
#'   are not rendered, in mm.
#' @param arrow specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}
#'
#' @inheritSection geom_grob Plot boundaries and clipping
#'
#' @family geometries adding layers with insets
#'
#' @export
#'
#' @examples
#' # inset plot with enlarged detail from a region of the main plot
#' library(tibble)
#' p <-
#'   ggplot(data = mtcars, mapping = aes(wt, mpg)) +
#'   geom_point()
#'
#' df <- tibble(x = 0.01,
#'              y = 0.01,
#'              plot = list(p +
#'                          coord_cartesian(xlim = c(3, 4),
#'                                          ylim = c(13, 16)) +
#'                          labs(x = NULL, y = NULL) +
#'                          theme_bw(10)))
#' p +
#'   expand_limits(x = 0, y = 0) +
#'   geom_plot_npc(data = df,
#'                 aes(npcx = x, npcy = y, label = plot))
#'
#' p +
#'   expand_limits(x = 0, y = 0) +
#'   geom_plot_npc(data = df,
#'                 aes(npcx = x, npcy = y, label = plot,
#'                 vp.width = 1/2, vp.height = 1/4))
#'
#' p +
#'   expand_limits(x = 0, y = 0) +
#'   geom_plot_npc(data = df,
#'                 aes(npcx = x, npcy = y, label = plot),
#'                 vp.width = 1/4, vp.height = 1/4)
#'
#' p +
#'   geom_plot(data = df,
#'             aes(x = x + 3, y = y + 20, label = plot),
#'             nudge_x = -1, nudge_y = - 7,
#'             hjust = 0.5, vjust = 0.5,
#'             arrow = arrow(length = unit(0.5, "lines")),
#'             colour = "red",
#'             vp.width = 1/5, vp.height = 1/5)
#'
geom_plot <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      ...,
                      nudge_x = 0,
                      nudge_y = 0,
                      default.colour = "black",
                      colour.target = "segment",
                      default.alpha = 1,
                      alpha.target = "segment",
                      add.segments = TRUE,
                      box.padding = 0.25,
                      point.padding = 1e-06,
                      segment.linewidth = 0.5,
                      min.segment.length = 0,
                      arrow = NULL,
                      na.rm = FALSE,
                      show.legend = FALSE,
                      inherit.aes = FALSE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position) && !identical(position, "identity")) {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }
    # original position needed for "position" justification
    position <-
      position_nudge_center(nudge_x, nudge_y, kept.origin = "original")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPlot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      default.colour = default.colour,
      colour.target = colour.target,
      default.alpha = default.alpha,
      alpha.target = alpha.target,
      add.segments = add.segments,
      box.padding = box.padding,
      point.padding = point.padding,
      segment.linewidth = segment.linewidth,
      min.segment.length = min.segment.length,
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
GeomPlot <-
  ggplot2::ggproto("GeomPlot", ggplot2::Geom,
          required_aes = c("x", "y", "label"),

          default_aes = ggplot2::aes(
            colour = "black",
            angle = 0,
            hjust = "inward",
            vjust = "inward",
            alpha = NA,
            family = "",
            fontface = 1,
            vp.width = 0.4,
            vp.height = 0.4
          ),

          draw_panel =   function(data,
                                  panel_params,
                                  coord,
                                  add.segments = TRUE,
                                  box.padding = 0.25,
                                  point.padding = 1e-06,
                                  segment.linewidth = 1,
                                  min.segment.length = 0,
                                  arrow = NULL,
                                  default.colour = "black",
                                  colour.target = "all",
                                  default.alpha = 1,
                                  alpha.target = "all",
                                  na.rm = FALSE) {

            if (nrow(data) == 0) {
              return(grid::nullGrob())
            }

            if (!ggplot2::is.ggplot(data$label[[1]])) {
              warning("Skipping as object mapped to 'label' is not",
                      " a list of \"gg\" or \"ggplot\" objects.")
              return(grid::nullGrob())
            }

            add.segments <- add.segments && all(c("x_orig", "y_orig") %in% colnames(data))

            # should be called only once!
            data <- coord$transform(data, panel_params)
            if (add.segments) {
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
              plot.alpha <-
                ifelse(any(alpha.target %in% c("all", "plot")),
                       row$alpha, default.alpha)
              segment.alpha <-
                ifelse(any(alpha.target %in% c("all", "segment")),
                       row$alpha, default.alpha)
              user.grob <- ggplot2::ggplotGrob(x = data$label[[row.idx]])

              user.grob$vp <-
                grid::viewport(x = grid::unit(row$x, "native"),
                               y = grid::unit(row$y, "native"),
                               width = grid::unit(row$vp.width, "npc"),
                               height = grid::unit(row$vp.height, "npc"),
                               just = c(row$hjust, row$vjust),
                               angle = row$angle,
                               name = paste("inset.plot.vp", row$PANEL,
                                            "row", row.idx, sep = "."))

              # give unique name to each grob
              user.grob$name <- paste("inset.plot", row.idx, sep = ".")

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
                                       name = paste("plot.s.segment", row$group, row.idx, sep = "."))
                }
                all.grobs <- grid::gList(all.grobs, segment.grob, user.grob)
              } else {
                all.grobs <- grid::gList(all.grobs, user.grob)
              }
            }
            #    grid::grobTree(children = all.grobs, name = "geom.plot.panel")
            grid::grobTree(children = all.grobs)

          },
          draw_key = function(...) {
            grid::nullGrob()
          }
  )

#' @rdname geom_plot
#' @export
#'
geom_plot_npc <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = FALSE,
                          inherit.aes = FALSE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPlotNpc,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPlotNpc <-
  ggplot2::ggproto("GeomPlotNpc", ggplot2::Geom,
          required_aes = c("npcx", "npcy", "label"),

          default_aes = ggplot2::aes(
            colour = "black", angle = 0, hjust = "inward",
            vjust = "inward", alpha = NA, family = "", fontface = 1,
            vp.width = 0.4, vp.height = 0.4
          ),

          draw_panel = function(data, panel_params, coord,
                                na.rm = FALSE) {

            if (nrow(data) == 0) {
              return(grid::nullGrob())
            }

            if (!is.ggplot(data$label[[1]])) {
              warning("Skipping as object mapped to 'label' is not a list of \"gg\" or \"ggplot\" objects.")
              return(grid::nullGrob())
            }

            data$npcx <- compute_npcx(data$npcx)
            data$npcy <- compute_npcy(data$npcy)

            if (is.character(data$vjust)) {
              data$vjust <- compute_just(data$vjust, data$npcy)
            }
            if (is.character(data$hjust)) {
              data$hjust <- compute_just(data$hjust, data$npcx)
            }

            plot.grobs <- grid::gList()

            for (row.idx in seq_len(nrow(data))) {
              plotGrob <-
                ggplot2::ggplotGrob(x = data$label[[row.idx]])

              plotGrob$vp <- grid::viewport(x = grid::unit(data$npcx[row.idx], "native"),
                                            y = grid::unit(data$npcy[row.idx], "native"),
                                            width = grid::unit(data$vp.width[row.idx], "npc"),
                                            height = grid::unit(data$vp.height[row.idx], "npc"),
                                            just = c(data$hjust[row.idx],
                                                     data$vjust[row.idx]),
                                            angle = data$angle[row.idx],
                                            name = paste("geom_plot.panel",
                                                         data$PANEL[row.idx], "row",
                                                         row.idx, sep = "."))

              # give unique name to each plot
              plotGrob$name <- paste("inset.plot", row.idx, sep = ".")

              plot.grobs[[row.idx]] <- plotGrob
            }

            # grid.name <- paste("geom_plot.panel",
            #                    data$PANEL[row.idx], sep = ".")
            # grid::gTree(children = plot.grobs, name = grid.name)

            grid::gTree(children = plot.grobs)
          },

          draw_key = function(...) {
            grid::nullGrob()
          }
  )

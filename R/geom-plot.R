#' Inset plots
#'
#' \code{geom_plot} and \code{geom_plot_npc} add ggplot objects as insets to the
#' base ggplot, using syntax similar to that of
#' \code{\link[ggplot2]{geom_label}}  and \code{\link{geom_text_s}}. In most
#' respects they behave as any other ggplot geometry: a layer con contain
#' multiple tables and faceting works as usual.
#'
#' @details You can modify the alignment of inset plots with the \code{vjust}
#'   and \code{hjust} aesthetics. These can either be a number between 0
#'   (right/bottom) and 1 (top/left) or a character ("left", "middle", "right",
#'   "bottom", "center", "top").
#'
#'   You can modify the size of inset plots with the \code{vp.width} and
#'   \code{vp.height} aesthetics. These can take a number between 0 (smallest
#'   possible inset) and 1 (whole plotting area width or height). The default
#'   value for for both of these aesthetics is 1/5. Thus, in contrast to
#'   \code{\link[ggplot2]{geom_text}} and \code{\link{geom_text_s}} the size of
#'   the insets remains the same relative to the size of the plotting area
#'   irrespective of how the plot is rendered. The aspect ratio of insets is
#'   preserved and size is adjusted until the whole inset fits within the
#'   viewport.
#'
#'   You can modify inset plot alignment with the `vjust` and `hjust`
#'   aesthetics. These can either be a number between 0 (right/bottom) and 1
#'   (top/left) or a character (`"left"`, `"middle"`, `"right"`, `"bottom"`,
#'   `"center"`, `"top"`). There several two special alignments: `"inward"` and
#'   `"outward"`. Inward always aligns text towards the center of the plotting
#'   area, and outward aligns it away from the center of the plotting area. It
#'   tagged with `_mean` or `_median` the mean or median of the data in the
#'   panel along the corresponding axis is used as center.
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
#'   ggplot objects ("gg" class) to be mapped to the \code{label} aesthetic.
#'
#'   The \code{x} and \code{y} aesthetics determine the position of the whole
#'   inset plot, similarly to that of a text label, justification is interpreted
#'   as indicating the position of the plot with respect to its x and y
#'   coordinates in the data, and \code{angle} is used to rotate the plot as a
#'   whole.
#'
#'   In the case of \code{geom_plot_npc()}, \code{npcx} and \code{npcy}
#'   aesthetics determine the position of the inset plot. As for text labels,
#'   justification is interpreted as indicating the position of the inset plot
#'   with respect to its \code{npcx} and \code{npcy} coordinates, and
#'   \code{angle} is used to rotate the plot as a whole.
#'
#'   \code{\link[ggplot2]{annotate}} cannot be used with \code{geom = "plot"}.
#'   Use \code{\link{annotate}} (automatic unless 'ggpp' is not attached) as
#'   redefined in 'ggpp' when adding inset plots as annotations (automatic
#'   unless 'ggpp' is not attached).
#'
#' @seealso \code{\link{geom_plot}}, \code{\link{geom_table}},
#'   \code{\link{annotate}}, \code{\link{position_nudge_keep}},
#'   \code{\link{position_nudge_to}}, \code{\link{position_jitternudge}},
#'   \code{\link{position_dodgenudge}} and \code{\link{position_stacknudge}}.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
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
#' @param add.segments logical Display connecting segments or arrows between
#'   original positions and displaced ones if both are available.
#' @param arrow specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}
#'
#' @section Known problem!: In some cases when explicit coordinates are added to
#'   the inner plot, it may be also necessary to add explicitly coordinates to
#'   the outer plots.
#'
#' @details The "width" and "height" of an inset as for a text element are 0, so
#'   stacking and dodging inset plots will not work by default, and axis limits
#'   are not automatically expanded to include all inset plots. Obviously,
#'   insets do have height and width, but they are physical units, not data
#'   units. The amount of space they occupy on the main plot is not constant in
#'   data units of the base plot: when you modify scale limits, inset plots stay
#'   the same size relative to the physical size of the base plot.
#'
#' @note These geoms work only with tibbles as \code{data}, as they expects a
#'   list of ggplots ("gg" objects) to be mapped to the \code{label} aesthetic.
#'   Aesthetics mappings in the inset plot are independent of those in the base
#'   plot.
#'
#'   In the case of \code{geom_plot()}, \code{x} and \code{y} aesthetics
#'   determine the position of the whole inset plot, similarly to that of a text
#'   label, justification is interpreted as indicating the position of the plot
#'   with respect to the $x$ and $y$ coordinates in the data, and \code{angle}
#'   is used to rotate the plot as a whole.
#'
#'   In the case of \code{geom_plot_npc()}, \code{npcx} and \code{npcy}
#'   aesthetics determine the position of the whole inset plot, similarly to
#'   that of a text label, justification is interpreted as indicating the
#'   position of the plot with respect to the $x$ and $y$ coordinates in "npc"
#'   units, and \code{angle} is used to rotate the plot as a whole.
#'
#'   \strong{\code{annotate()} cannot be used with \code{geom = "plot"}}. Use
#'   \code{\link[ggplot2]{annotation_custom}} directly when adding inset plots
#'   as annotations.
#'
#' @references The idea of implementing a \code{geom_custom()} for grobs has
#'   been discussed as an issue at
#'   \url{https://github.com/tidyverse/ggplot2/issues/1399}.
#'
#' @family geometries adding layers with insets
#'
#' @return A plot layer instance.
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
#'   geom_plot_npc(data = df, aes(npcx = x, npcy = y, label = plot))
#'
#' p +
#'   expand_limits(x = 0, y = 0) +
#'   geom_plot_npc(data = df,
#'                 vp.width = 1/2, vp.height = 1/4,
#'                 aes(npcx = x, npcy = y, label = plot))
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
#'             segment.colour = "red",
#'             vp.width = 1/5, vp.height = 1/5)
#'
geom_plot <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      nudge_x = 0,
                      nudge_y = 0,
                      add.segments = TRUE,
                      arrow = NULL,
                      na.rm = FALSE,
                      show.legend = FALSE,
                      inherit.aes = FALSE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position) && position != "identity") {
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
    geom = GeomPlot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      add.segments = add.segments,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#'
#' @format NULL
#' @usage NULL
#'
gplot_draw_panel_fun <-
  function(data,
           panel_params,
           coord,
           add.segments = TRUE,
           arrow = NULL,
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

    # loop needed as gpar is not vectorized
    all.grobs <- grid::gList()

    for (row.idx in 1:nrow(data)) {
      row <- data[row.idx, , drop = FALSE]
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
        segment.grob <-
          grid::segmentsGrob(x0 = row$x,
                             y0 = row$y,
                             x1 = row$x_orig,
                             y1 = row$y_orig,
                             arrow = arrow,
                             gp = grid::gpar(col = ggplot2::alpha(row$segment.colour,
                                                                  row$segment.alpha)),
                             name = paste("inset.plot.segment", row.idx, sep = "."))
        all.grobs <- grid::gList(all.grobs, segment.grob, user.grob)
      } else {
        all.grobs <- grid::gList(all.grobs, user.grob)
      }
    }

#    grid::grobTree(children = all.grobs, name = "geom.plot.panel")
    grid::grobTree(children = all.grobs)

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
            vp.height = 0.4,
            segment.linetype = 1,
            segment.colour = "grey33",
            segment.size = 0.5,
            segment.alpha = 1
          ),

          draw_panel = gplot_draw_panel_fun,
          draw_key = function(...) {
            grid::nullGrob()
          }
  )

#' @rdname geom_plot
#' @export
#'
geom_plot_npc <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
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
#'
#' @format NULL
#' @usage NULL
#'
gplotnpc_draw_panel_fun <-
  function(data, panel_params, coord,
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

          draw_panel = gplotnpc_draw_panel_fun,
          draw_key = function(...) {
            grid::nullGrob()
          }
  )

#' Inset plots
#'
#' \code{geom_plot} and \code{geom_plot_npc} add ggplot objects as insets to the
#' base ggplot, using syntax similar to that of
#' \code{\link[ggplot2]{geom_label}}. In most respects they behave as any other
#' ggplot geometry: a layer con contain multiple tables and faceting works as
#' usual.
#'
#' @section Inset alignment: You can modify inset plot alignment with the
#'   \code{vjust} and \code{hjust} aesthetics. These can either be a number
#'   between 0 (right/bottom) and 1 (top/left) or a character ("left", "middle",
#'   "right", "bottom", "center", "top"). The \code{angle} aesthetics can be
#'   used to rotate the inset plots.
#'
#' @section Inset size and aspect: You can modify the size of the inset plot
#'   with the \code{vp.width} and \code{vp.height} aesthetics. Arguments can be
#'   a number between 0 (smallest possible inset) and 1 (whole plotting area
#'   width or height). The default value for for both of these aesthetics is
#'   1/3. If the coordinates are "free" the plot stretches to fill the viewport.
#'   However, if the coordinates of the inset are "fixed" and the aspect ratio
#'   of the viewport is different to that of the inset, the viewport will be
#'   surrounded on either $x$ or $y$ margins by invisible space, which may look
#'   as if the position of the inset is wrong.
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
#'
#' @section Known problem!: In some cases when explicit coordinates are added
#'   to the inner plot, it may be also necessary to add explicitly coordinates
#'   to the outer plots.
#'
#' @details The "width" and "height" of an inset as for a text element are
#'   0, so stacking and dodging inset plots will not work by default, and axis
#'   limits are not automatically expanded to include all inset plots.
#'   Obviously, insets do have height and width, but they are physical units,
#'   not data units. The amount of space they occupy on the main plot is not
#'   constant in data units of the base plot: when you modify scale limits,
#'   inset plots stay the same size relative to the physical size of the base
#'   plot.
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
#' df <- tibble(x = 0.01, y = 0.01,
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
geom_plot <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = FALSE,
                       inherit.aes = FALSE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPlot,
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
gplot_draw_panel_fun <-
  function(data, panel_params, coord,
           na.rm = FALSE) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (!is.ggplot(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not",
              " a list of \"gg\" or \"ggplot\" objects.")
      return(grid::nullGrob())
    }

    # should be called only once!
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

    plot.grobs <- grid::gList()

    for (row.idx in seq_len(nrow(data))) {
      plotGrob <-
        ggplotGrob(x = data$label[[row.idx]])

      plotGrob$vp <-
        grid::viewport(x = unit(data$x[row.idx], "native"),
                       y = unit(data$y[row.idx], "native"),
                       width = unit(data$vp.width[row.idx], "npc"),
                       height = unit(data$vp.height[row.idx], "npc"),
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

    grid.name <- paste("geom_plot.panel",
                       data$PANEL[row.idx], sep = ".")

    grid::gTree(children = plot.grobs, name = grid.name)
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPlot <-
  ggproto("GeomPlot", Geom,
          required_aes = c("x", "y", "label"),

          default_aes = aes(
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
  layer(
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
        ggplotGrob(x = data$label[[row.idx]])

      plotGrob$vp <- grid::viewport(x = unit(data$npcx[row.idx], "native"),
                                    y = unit(data$npcy[row.idx], "native"),
                                    width = unit(data$vp.width[row.idx], "npc"),
                                    height = unit(data$vp.height[row.idx], "npc"),
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

    grid.name <- paste("geom_plot.panel",
                       data$PANEL[row.idx], sep = ".")

    grid::gTree(children = plot.grobs, name = grid.name)
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomPlotNpc <-
  ggproto("GeomPlotNpc", Geom,
          required_aes = c("npcx", "npcy", "label"),

          default_aes = aes(
            colour = "black", angle = 0, hjust = "inward",
            vjust = "inward", alpha = NA, family = "", fontface = 1,
            vp.width = 0.4, vp.height = 0.4
          ),

          draw_panel = gplotnpc_draw_panel_fun,
          draw_key = function(...) {
            grid::nullGrob()
          }
  )

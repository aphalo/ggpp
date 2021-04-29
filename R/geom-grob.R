#' Inset graphical objects
#'
#' \code{geom_grob} and \code{geom_grob_npc} add a Grob as inset to the ggplot
#' using syntax similar to that of \code{\link[ggplot2]{geom_label}}.In most
#' respects they behave as any other ggplot geometry: a layer con contain
#' multiple tables and faceting works as usual.
#'
#' @section Alignment: You can modify table alignment with the \code{vjust} and
#'   \code{hjust} aesthetics. These can either be a number between 0
#'   (right/bottom) and 1 (top/left) or a character ("left", "middle", "right",
#'   "bottom", "center", "top").
#'
#' @section Inset size: You can modify inset plot size with the \code{vp.width}
#'   and \code{vp.height} aesthetics. These can be a number between 0 (smallest
#'   posisble inset) and 1 (whole plotting area width or height). The default
#'   value for for both of these aesthetics is 1/3.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
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
#' @details The "width" and "height" of an inset as for a text element are
#'   0, so stacking and dodging inset plots will not work by default, and axis
#'   limits are not automatically expanded to include all inset plots.
#'   Obviously, insets do have height and width, but they are physical units,
#'   not data units. The amount of space they occupy on the main plot is not
#'   constant in data units of the base plot: when you modify scale limits,
#'   inset plots stay the same size relative to the physical size of the base
#'   plot.
#'
#' @note These geoms work only with tibbles as \code{data}, as they expects a list
#'   of graphics objects ("grob") to be mapped to the \code{label} aesthetic.
#'   Aesthetics mappings in the inset plot are independent of those in the base
#'   plot.
#'
#'   In the case of \code{geom_grob()}, \code{x} and \code{y} aesthetics
#'   determine the position of the whole inset grob, similarly to that of a text
#'   label, justification is interpreted as indicating the position of the grob
#'   with respect to the $x$ and $y$ coordinates in the data, and \code{angle}
#'   is used to rotate the plot as a whole.
#'
#'   In the case of \code{geom_grob_npc()}, \code{npcx} and \code{npcy} aesthetics
#'   determine the position of the whole inset plot, similarly to that of a text
#'   label, justification is interpreted as indicating the position of the grob
#'   with respect to the $x$ and $y$ coordinates in "npc" units, and \code{angle}
#'   is used to rotate the plot as a whole.
#'
#'   \strong{\code{annotate()} cannot be used with \code{geom = "grob"}}. Use
#'   \code{\link[ggplot2]{annotation_custom}} directly when adding inset plots
#'   as annotations.
#'
#' @references The idea of implementing a \code{geom_custom()} for grobs has
#'   been discussed as an issue at
#'   \url{https://github.com/tidyverse/ggplot2/issues/1399}.
#'
#' @family geometries adding layers with insets
#'
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(x = 2, y = 15, grob = list(grid::circleGrob(r = 0.2)))
#' ggplot(data = mtcars, aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   geom_grob(data = df, aes(x, y, label = grob))
#'
geom_grob <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = FALSE,
                      inherit.aes = FALSE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGrob,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpextra-ggproto
#'
#' @format NULL
#' @usage NULL
#'
grob_draw_panel_fun <-
  function(data, panel_params, coord,
           na.rm = FALSE) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (!grid::is.grob(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not a list of \"grob\" objects.")
      return(grid::nullGrob())
    }

    # should be called only once!
    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <-
        compute_just2d(data = data,
                       panel_params = panel_params,
                       coord = coord,
                       hjust = NULL,
                       vjust = data$vjust)
    }
    if (is.character(data$hjust)) {
      data$hjust <-
        compute_just2d(data = data,
                       panel_params = panel_params,
                       coord = coord,
                       hjust = data$hjust,
                       vjust = NULL)
    }

    user.grobs <- grid::gList()

    for (row.idx in 1:nrow(data)) {
      userGrob <- data$label[[row.idx]]

      userGrob$vp <-
        grid::viewport(x = grid::unit(data$x[row.idx], "native"),
                       y = grid::unit(data$y[row.idx], "native"),
                       width = grid::unit(data$vp.width[row.idx], "npc"),
                       height = grid::unit(data$vp.height[row.idx], "npc"),
                       just = c(data$hjust[row.idx], data$vjust[row.idx]),
                       angle = data$angle[row.idx],
                       name = paste("geom_grob.panel", data$PANEL[row.idx],
                                    "row", row.idx, sep = "."))

      # give unique name to each grob
      userGrob$name <- paste("inset.grob", row.idx, sep = ".")

      user.grobs[[row.idx]] <- userGrob
    }

    grid.name <- paste("geom_grob.panel",
                       data$PANEL[row.idx], sep = ".")

    grid::gTree(children = user.grobs, name = grid.name)
  }

#' @rdname ggpextra-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomGrob <-
  ggplot2::ggproto("GeomGrob", ggplot2::Geom,
          required_aes = c("x", "y", "label"),

          default_aes = ggplot2::aes(
            colour = "black", angle = 0, hjust = 0.5,
            vjust = 0.5, alpha = NA, family = "", fontface = 1,
            vp.width = 1/5, vp.height = 1/5
          ),

          draw_panel = grob_draw_panel_fun,
          draw_key = function(...) {
            grid::nullGrob()
          }
  )

#' @rdname geom_grob
#' @export
#'
geom_grob_npc <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = FALSE,
                          inherit.aes = FALSE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGrobNpc,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpextra-ggproto
#'
#' @format NULL
#' @usage NULL
#'
grobnpc_draw_panel_fun <-
  function(data, panel_params, coord,
           na.rm = FALSE) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (!grid::is.grob(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not a list of \"grob\".")
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

    user.grobs <- grid::gList()

    for (row.idx in 1:nrow(data)) {
      userGrob <- data$label[[row.idx]]

      userGrob$vp <-
        grid::viewport(x = grid::unit(data$npcx[row.idx], "npc"),
                       y = grid::unit(data$npcy[row.idx], "npc"),
                       width = grid::unit(data$vp.width[row.idx], "npc"),
                       height = grid::unit(data$vp.height[row.idx], "npc"),
                       just = c(data$hjust[row.idx], data$vjust[row.idx]),
                       angle = data$angle[row.idx],
                       name = paste("geom_grob.panel", data$PANEL[row.idx],
                                    "row", row.idx, sep = "."))

      # give unique name to each grob
      userGrob$name <- paste("inset.grob", row.idx, sep = ".")

      user.grobs[[row.idx]] <- userGrob
    }

    grid.name <- paste("geom_grob.panel",
                       data$PANEL[row.idx], sep = ".")

    grid::gTree(children = user.grobs, name = grid.name)
  }

#' @rdname ggpextra-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomGrobNpc <-
  ggplot2::ggproto("GeomGrobNpc", ggplot2::Geom,
          required_aes = c("npcx", "npcy", "label"),

          default_aes = ggplot2::aes(
            colour = "black", angle = 0, hjust = "inward",
            vjust = "inward", alpha = NA, family = "", fontface = 1,
            vp.width = 1/5, vp.height = 1/5
          ),

          draw_panel = grobnpc_draw_panel_fun,
          draw_key = function(...) {
            grid::nullGrob()
          }
  )

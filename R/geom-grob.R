#' Inset graphical objects
#'
#' \code{geom_grob} and \code{geom_grob_npc} add Grobs as insets to the ggplot
#' using syntax similar to that of \code{\link[ggplot2]{geom_text}}. In most
#' respects they behave as any other ggplot geometry: ther add a layer
#' containing one or more grobs and faceting works as usual.
#'
#' @section Alignment: You can modify gron alignment with the \code{vjust} and
#'   \code{hjust} aesthetics. These can either be a number between 0
#'   (right/bottom) and 1 (top/left) or a character ("left", "middle", "right",
#'   "bottom", "center", "top").
#'
#' @section Inset size: You can modify inset plot size with the \code{vp.width}
#'   and \code{vp.height} aesthetics. These can take a number between 0 (smallest
#'   possible inset) and 1 (whole plotting area width or height). The default
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
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label. The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param add.segments logical Display connecting segments or arrows between
#'   original positions and displaced ones if both are available.
#' @param arrow specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}
#'
#' @details The "width" and "height" of an inset grob as for a text element are
#'   seen as zero by ggplot. However, their size is respected when plotted. The
#'   amount of space the grobs on the main plot is not constant in data units of
#'   the base plot: when you modify scale limits, inset grobs stay the same size
#'   relative to the physical size of the base plot. They do scale when the
#'   output size of the plot changes, e.g., when zooming. Axis limits are not
#'   automatically expanded to include inset grobs in whole but only their x and
#'   y coordinates.
#'
#'   Position functions, including all nudge funcrions in this package are
#'   supported, as well as drawing of connecting segments. However, stacking and
#'   dodging of inset grobs will not usually work as for they do for plot
#'   elements with non-zero size.
#'
#'   These geoms work only with tibbles as \code{data}, as they expect a list of
#'   graphics objects ("grob") to be mapped to the \code{label} aesthetic.
#'   Aesthetics mappings in the inset plot are independent of those in the base
#'   plot.
#'
#'   In the case of \code{geom_grob()}, \code{x} and \code{y} aesthetics
#'   determine the position of the whole inset grob, similarly to that of a text
#'   label, justification is interpreted as indicating the position of the grob
#'   with respect to its $x$ and $y$ coordinates in the data, and \code{angle}
#'   is used to rotate the grob as a whole.
#'
#'   In the case of \code{geom_grob_npc()}, \code{npcx} and \code{npcy}
#'   aesthetics determine the position of the inset grob. As for text labels,
#'   justification is interpreted as indicating the position of the grob with
#'   respect to the $x$ and $y$ coordinates in "npc" units, and \code{angle} is
#'   used to rotate the plot as a whole.
#'
#'   \strong{\code{annotate()} cannot be used with \code{geom = "grob"}}. Use
#'   \code{\link[ggplot2]{annotation_custom}} directly when adding inset plots
#'   as annotations.
#'
#' @return A plot layer instance.
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
#'
#' ggplot(data = mtcars, aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   geom_grob(data = df, aes(x, y, label = grob))
#'
#' ggplot(data = mtcars, aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   geom_grob(data = df, aes(x, y, label = grob),
#'             nudge_x = 0.5,
#'             add.segments = TRUE,
#'             segment.colour = "red")
#'
#' ggplot(data = mtcars, aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   geom_grob(data = df, aes(x, y, label = grob),
#'             nudge_x = 0.5)
#'
geom_grob <- function(mapping = NULL,
                      data = NULL,
                      stat = "identity",
                      position = "identity",
                      ...,
                      nudge_x = 0,
                      nudge_y = 0,
                      add.segments = FALSE,
                      arrow = NULL,
                      na.rm = FALSE,
                      show.legend = FALSE,
                      inherit.aes = FALSE) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- position_nudge_center(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomGrob,
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
grob_draw_panel_fun <-
  function(data,
           panel_params,
           coord,
           na.rm = FALSE,
           add.segments,
           arrow = NULL) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (!grid::is.grob(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not a list of \"grob\" objects.")
      return(grid::nullGrob())
    }

    add.segments <- add.segments && all(c("x_orig", "y_orig") %in% colnames(data))

    # should be called only once!
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

    user.grobs <- grid::gList()
    idx.shift <- 0

    # Draw segments first
    if(add.segments) {
      idx.shift <- idx.shift + 1
      user.grobs[[1L]] <-
        grid::segmentsGrob(x0 = data$x,
                           y0 = data$y,
                           x1 = data_orig$x,
                           y1 = data_orig$y,
                           arrow = arrow,
                           gp = grid::gpar(col = alpha(data$segment.colour,
                                                       data$segment.alpha)),
                           name = "linking.segments.grob")
    }

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

      user.grobs[[row.idx + idx.shift]] <- userGrob
    }

    grid.name <- paste("geom_grob.panel",
                       data$PANEL[row.idx], sep = ".")
    grid.name <- c(grid.name, "geom_grob.panel.segments")


    grid::grobTree(children = user.grobs)

  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomGrob <-
  ggplot2::ggproto("GeomGrob", ggplot2::Geom,
                   required_aes = c("x", "y", "label"),

                   default_aes = ggplot2::aes(
                     colour = "black", angle = 0, hjust = 0.5,
                     vjust = 0.5, alpha = NA, family = "", fontface = 1,
                     vp.width = 1/5, vp.height = 1/5,
                     segment.linetype = 1,
                     segment.colour = "grey33",
                     segment.size = 0.5,
                     segment.alpha = 1
                   ),

                   draw_panel = grob_draw_panel_fun,

                   draw_key = function(...) {
                     grid::nullGrob()
                   }
  )

#' @rdname geom_grob
#' @export
#'
geom_grob_npc <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
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

#' @rdname ggpp-ggproto
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

#' @rdname ggpp-ggproto
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


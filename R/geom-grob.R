#' Inset graphical objects
#'
#' \code{geom_grob} and \code{geom_grob_npc} add Grobs as insets to the ggplot
#' using syntax similar to that of \code{\link[ggplot2]{geom_text}},
#' \code{\link{geom_text_s}} and \code{\link{geom_text_npc}}.
#' In most respects they behave as any other ggplot geometry: they add a layer
#' containing one or more grobs and grouping and faceting works as usual. The
#' most common use of \code{geom_grob} is to add data labels that are graphical
#' objects rather than text. \code{\link{geom_grob_npc}} is used to add grobs
#' as annotations to plots, but contrary to layer function \code{annotate()},
#' \code{\link{geom_grob_npc}} is data driven and respects grouping and facets,
#' thus plot insets can differ among panels. Of these two geoms only
#' \code{\link{geom_grob}} supports the plotting of segments, as
#' \code{\link{geom_grob_npc}} uses a coordinate system that is unrelated
#' to data units and data.
#'
#' @details You can modify the size of insets with the \code{vp.width} and
#'   \code{vp.height} aesthetics. These can take a number between 0 (smallest
#'   possible inset) and 1 (whole plotting area width or height). The default
#'   value for for both of these aesthetics is 1/5. Thus, in contrast to
#'   \code{\link[ggplot2]{geom_text}}, \code{\link[ggplot2]{geom_label}},
#'   \code{\link{geom_text_s}} and \code{\link{geom_label_s}} the size of the
#'   insets remains the same relative to the size of the plotting area
#'   irrespective of the size the plot is rendered at. The aspect ratio of
#'   insets is preserved and size is adjusted until the whole inset fits within
#'   the viewport.
#'
#'   By default \code{geom_grob} uses \code{\link{position_nudge_center}} and
#'   justification \code{"position"}, while \code{geom_grob_npc} uses
#'   \code{\link[ggplot2]{position_nudge}} and justification \code{"inward"}. In
#'   contrast to \code{\link[ggplot2]{position_nudge}},
#'   \code{\link{position_nudge_center}} and all other position functions
#'   defined in packages 'ggpp' keep the original coordinates thus allowing the
#'   plotting of connecting segments and arrows.
#'
#'   \code{geom_grob} and \code{geom_grob_npc} expect a list of graphic objects
#'   ("grob") to be mapped to the \code{label} aesthetic. These geoms work with
#'   tibbles or data frames as \code{data} as they both support \code{list}
#'   objects as member variables.
#'
#'   The \code{x} and \code{y} aesthetics determine the position of the whole
#'   inset grob, similarly to that of a text label, justification is interpreted
#'   as indicating the position of the grob with respect to its \emph{x} and
#'   \emph{y} coordinates in the data, and \code{angle} is used to rotate the
#'   grob as a whole.
#'
#' @section Plot boundaries and clipping: The "width" and "height" of an inset
#'   as for a text element are 0, so stacking and dodging inset plots will not
#'   work by default, and axis limits are not automatically expanded to include
#'   all inset plots. Obviously, insets do have height and width, but they are
#'   physical units, not data units. The amount of space they occupy on the main
#'   plot is not constant in data units of the base plot: when you modify scale
#'   limits, inset plots stay the same size relative to the physical size of the
#'   base plot.
#'
#' @inheritSection geom_text_s Alignment
#'
#' @inheritSection geom_text_s Position functions
#'
#' @note The insets are stored nested within the main ggplot object and
#'   contain their own copy of the data, and are rendered as grid grobs as normal
#'   ggplots at the time the main ggplot is rendered. They can have different
#'   themes.
#'
#'   Use \code{\link{annotate}} as redefined in 'ggpp' when adding insets
#'   as annotations (automatically available unless 'ggpp' is not attached).
#'   \code{\link[ggplot2]{annotate}} cannot be used with the \code{npcx} and
#'   \code{npcy} pseudo-aesthetics.
#'
#' @seealso \code{\link[grid]{grid-package}}, \code{\link[ggplot2]{geom_text}},
#'   and other documentation of package 'ggplot2'.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you
#'   are overriding the plot defaults.
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
#'   starting position of each "label". The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param default.colour,default.color A colour definition to use for elements
#'   not targeted by the colour aesthetic.
#' @param colour.target,color.target A vector of character strings; \code{"all"},
#'   \code{"text"}, \code{"box"} and \code{"segment"} or \code{"none"}.
#' @param default.alpha numeric in [0..1] A transparency value to use for
#'   elements not targeted by the alpha aesthetic.
#' @param alpha.target A vector of character strings; \code{"all"},
#'   \code{"text"}, \code{"segment"}, \code{"box"}, \code{"box.line"}, and
#'   \code{"box.fill"} or \code{"none"}.
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
#' @return A plot layer instance.
#'
#' @references The idea of implementing a \code{geom_custom()} for grobs has
#'   been discussed as an issue at
#'   \url{https://github.com/tidyverse/ggplot2/issues/1399}.
#'
#' @family geometries adding layers with insets.
#'
#' @export
#'
#' @examples
#' library(tibble)
#' df <- tibble(x = 2, y = 15, grob = list(grid::circleGrob(r = 0.2)))
#'
#' # without nudging no segments are drawn
#' ggplot(data = mtcars,
#'        aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   geom_grob(data = df,
#'             aes(x, y, label = grob))
#'
#' # with nudging segments are drawn
#' ggplot(data = mtcars,
#'        aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   geom_grob(data = df,
#'             aes(x, y, label = grob),
#'             nudge_x = 0.5,
#'             colour = "red",
#'             hjust = 0.5,
#'             vjust = 0.5)
#'
#' ggplot(data = mtcars,
#'        aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   geom_grob(data = df,
#'             aes(x, y, label = grob),
#'             nudge_x = 0.5,
#'             colour = "red",
#'             colour.target = "none",
#'             hjust = 0.5,
#'             vjust = 0.5)
#'
#' # with nudging plotting of segments can be disabled
#' ggplot(data = mtcars,
#'        aes(wt, mpg)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   geom_grob(data = df,
#'             aes(x, y, label = grob),
#'             add.segments = FALSE,
#'             nudge_x = 0.5,
#'             hjust = 0.5,
#'             vjust = 0.5)
#'
geom_grob <-
  function(mapping = NULL,
           data = NULL,
           stat = "identity",
           position = "identity",
           ...,
           nudge_x = 0,
           nudge_y = 0,
           default.colour = "black",
           default.color = default.colour,
           colour.target = "segment",
           color.target = colour.target,
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

    colour.target <-
      rlang::arg_match(color.target,
                       values = c("segment", "all", "grob", "box", "none"),
                       multiple = TRUE)
    alpha.target <-
      rlang::arg_match(alpha.target,
                       values = c("segment", "all", "grob", "box",
                                  "box.line", "box.fill", "none"),
                       multiple = TRUE)

    if (!missing(nudge_x) || !missing(nudge_y)) {
      if (!missing(position) && position != "identity") {
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
      geom = GeomGrob,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        default.colour = default.color,
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
GeomGrob <-
  ggplot2::ggproto("GeomGrob", ggplot2::Geom,
                   required_aes = c("x", "y", "label"),

                   default_aes = ggplot2::aes(
                     colour = "black",
                     angle = 0,
                     hjust = 0.5,
                     vjust = 0.5,
                     alpha = NA,
                     family = "",
                     fontface = 1,
                     vp.width = 1/5, vp.height = 1/5
                   ),

                   draw_panel = function(data,
                                         panel_params,
                                         coord,
                                         default.colour = "black",
                                         colour.target = "all",
                                         default.alpha = 1,
                                         alpha.target = "all",
                                         na.rm = FALSE,
                                         add.segments = TRUE,
                                         box.padding = 0.25,
                                         point.padding = 1e-06,
                                         segment.linewidth = 0.5,
                                         min.segment.length = 0,
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
                     user.grobs <- data[["label"]]
                     for (row.idx in 1:nrow(data)) {
                       row <- data[row.idx, , drop = FALSE]
                       grob.alpha <-
                         ifelse(any(alpha.target %in% c("all", "grob")),
                                row$alpha, default.alpha)
                       segment.alpha <-
                         ifelse(any(alpha.target %in% c("all", "segment")),
                                row$alpha, default.alpha)
                       user.grob <- user.grobs[[row.idx]]

                       user.grob$vp <-
                         grid::viewport(x = grid::unit(row$x, "native"),
                                        y = grid::unit(row$y, "native"),
                                        width = grid::unit(row$vp.width, "npc"),
                                        height = grid::unit(row$vp.height, "npc"),
                                        just = c(row$hjust, row$vjust),
                                        angle = row$angle,
                                        name = paste("inset.grob.vp", row$PANEL,
                                                     "row", row.idx, sep = "."))

                       # give unique name to each grob
                       user.grob$name <- paste("inset.grob", row.idx, sep = ".")

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
                                                name = paste("grob.s.segment", row$group, row.idx, sep = "."))
                         }
                         all.grobs <- grid::gList(all.grobs, segment.grob, user.grob)
                       } else {
                         all.grobs <- grid::gList(all.grobs, user.grob)
                       }
                     }
                     #    grid::grobTree(children = all.grobs, name = "geom.grob.panel")
                     grid::grobTree(children = all.grobs)

                   }
                   ,

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

                   draw_panel = function(data,
                                         panel_params,
                                         coord,
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

                     # grid.name <- paste("geom_grob.panel",
                     #                    data$PANEL[row.idx], sep = ".")
                     #
                     # grid::gTree(children = user.grobs, name = grid.name)
                     grid::gTree(children = user.grobs)
                   },

                   draw_key = function(...) {
                     grid::nullGrob()
                   }
  )


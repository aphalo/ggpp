#' Add Grobs on the margins
#'
#' Margin grobs can supplement a 2d display with annotations. Margin grobs such
#' as icons or symbols can highlight individual values along a margin. The
#' geometries \code{geom_x_margin_grob()} and \code{geom_y_margin_grob()} behave
#' similarly \code{geom_vline()} and \code{geom_hline()} and share their "double
#' personality" as both annotations and geometries.
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
#' @param sides A character string of length one that controls on which side of
#'   the plot the grob annotations appear on. It can be set to a string
#'   containing one of \code{"t"}, \code{"r"}, \code{"b"} or \code{"l"}, for
#'   top, right, bottom, and left.
#' @param grob.shift numeric value expressed in npc units for the shift of the
#'   marginal grob inwards from the edge of the plotting area.
#' @param xintercept,yintercept numeric Parameters that control the position of
#'   the marginal points. If these are set, data, mapping and show.legend are
#'   overridden.
#'
#' @family Geometries for marginal annotations in ggplots
#'
#' @return A plot layer instance.
#'
#' @export
#'
#' @examples
#' # We can add icons to the margin of a plot to signal events
#'
#'
#'
#'
geom_x_margin_grob <- function(mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               ...,
                               xintercept,
                               sides = "b",
                               grob.shift = 0,
                               na.rm = FALSE,
                               show.legend = FALSE,
                               inherit.aes = FALSE) {

  # Act like an annotation
  if (!missing(xintercept)) {
    data <- as.data.frame(list(xintercept = xintercept))
    mapping <- aes(xintercept = xintercept)
    show.legend <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXMarginGrob,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sides = stringr::str_sub(sides, 1L, 1L),
      grob.shift = grob.shift,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomXMarginGrob <-
  ggproto("GeomXMarginGrob", Geom,
          required_aes = c("xintercept"),
          default_aes = aes(
            colour = "black", angle = 0, hjust = 0.5,
            vjust = 0.5, alpha = NA, family = "", fontface = 1,
            vp.width = 1/5, vp.height = 1/5
          ),

          draw_panel = function(data, panel_params, coord, sides = "b",
                                grob.shift = 0.01, na.rm = FALSE) {
            if (nrow(data) == 0) {
              return(grid::nullGrob())
            }

            if (!grid::is.grob(data$label[[1]])) {
              warning("Skipping as object mapped to 'label' is not a list of \"grob\" objects.")
              return(grid::nullGrob())
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

            user.grobs <- grid::gList()

            # For coord_flip, coord$tranform does not flip the sides where to
            # draw the rugs. We have to flip them.
            flipped <- inherits(coord, 'CoordFlip')
            if (flipped) {
              sides <- chartr('tblr', 'rlbt', sides)
            }

            if (!flipped && !is.null(data$xintercept) &&
                (grepl("b", sides) || grepl("t", sides) ) ) {
              if (grepl("b", sides)) {
                y.pos <- unit(grob.shift, "npc")
              } else if (grepl("t", sides)) {
                y.pos <- unit(1 - grob.shift, "npc")
              }
              for (row.idx in seq_len(nrow(data))) {
                userGrob <- data$label[[row.idx]]

                userGrob$vp <-
                  grid::viewport(x = unit(data$xintercept[row.idx], "native"),
                                 y = y.pos,
                                 width = unit(data$vp.width[row.idx], "npc"),
                                 height = unit(data$vp.height[row.idx], "npc"),
                                 just = c(data$hjust[row.idx], data$vjust[row.idx]),
                                 angle = data$angle[row.idx],
                                 name = paste("geom_x_margin_grob.panel", data$PANEL[row.idx],
                                              "row", row.idx, sep = "."))

                # give unique name to each grob
                userGrob$name <- paste(sides, "margin.grob", row.idx, sep = ".")

                user.grobs[[row.idx]] <- userGrob
              }
            }

            # needed for handling flipped coords
            if (flipped && !is.null(data$yintercept) &&
                (grepl("l", sides) || grepl("r", sides) ) ) {
              if (grepl("l", sides)) {
                x.pos <- unit(grob.shift, "npc")
              } else if (grepl("r", sides)) {
                x.pos <- unit(1 - grob.shift, "npc")
              }
              for (row.idx in seq_len(nrow(data))) {
                userGrob <- data$label[[row.idx]]
                userGrob$vp <-
                  grid::viewport(x = x.pos,
                                 y = unit(data$yintercept[row.idx], "native"),
                                 width = unit(data$vp.width[row.idx], "npc"),
                                 height = unit(data$vp.height[row.idx], "npc"),
                                 just = c(data$hjust[row.idx], data$vjust[row.idx]),
                                 angle = data$angle[row.idx],
                                 name = paste("geom_x_margin_grob.panel", data$PANEL[row.idx],
                                              "row", row.idx, sep = "."))

                # give unique name to each grob
                userGrob$name <- paste(sides, "margin.grob", row.idx, sep = ".")

                user.grobs[[row.idx]] <- userGrob
              }
            }

            # grid.name <- paste("geom_x_margin_grob.panel",
            #                    data$PANEL[row.idx], sep = ".")
            #
            # grid::gTree(children = user.grobs, name = grid.name)
            grid::gTree(children = user.grobs)
          },

          draw_key = function(...) {
            grid::nullGrob()
          }
  )

## y axis marging
#' @rdname geom_x_margin_grob
#' @export
#'
geom_y_margin_grob <- function(mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               ...,
                               yintercept,
                               sides = "l",
                               grob.shift = 0,
                               na.rm = FALSE,
                               show.legend = FALSE,
                               inherit.aes = FALSE) {

  # Act like an annotation
  if (!missing(yintercept)) {
    data <- as.data.frame(list(yintercept = yintercept))
    mapping <- aes(yintercept = yintercept)
    show.legend <- FALSE
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYMarginGrob,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sides = stringr::str_sub(sides, 1L, 1L),
      grob.shift = grob.shift,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomYMarginGrob <-
  ggproto("GeomYMarginGrob", Geom,
          required_aes = c("yintercept"),
          default_aes = aes(
            colour = "black", angle = 0, hjust = 0.5,
            vjust = 0.5, alpha = NA, family = "", fontface = 1,
            vp.width = 1/5, vp.height = 1/5
          ),

          draw_panel = function(data, panel_params, coord, sides = "l",
                                grob.shift = 0, na.rm = FALSE) {
            if (nrow(data) == 0) {
              return(grid::nullGrob())
            }

            if (!grid::is.grob(data$label[[1]])) {
              warning("Skipping as object mapped to 'label' is not a list of \"grob\" objects.")
              return(grid::nullGrob())
            }

            data <- coord$transform(data, panel_params)
            if (is.character(data$vjust)) {
              data$vjust <- compute_just(data$vjust, data$y)
            }
            if (is.character(data$hjust)) {
              data$hjust <- compute_just(data$hjust, data$x)
            }

            user.grobs <- grid::gList()

            # For coord_flip, coord$transform does not flip the sides where to
            # draw the grobs. We have to flip them.
            flipped <- inherits(coord, 'CoordFlip')
            if (flipped) {
              sides <- chartr('tblr', 'rlbt', sides)
            }

            if (flipped && !is.null(data$xintercept) &&
                (grepl("b", sides) || grepl("t", sides) ) ) {
              if (grepl("b", sides)) {
                y.pos <- unit(grob.shift, "npc")
              } else if (grepl("t", sides)) {
                y.pos <- unit(1 - grob.shift, "npc")
              }
              for (row.idx in seq_len(nrow(data))) {
                userGrob <- data$label[[row.idx]]

                userGrob$vp <-
                  grid::viewport(x = unit(data$xintercept[row.idx], "native"),
                                 y = y.pos,
                                 width = unit(data$vp.width[row.idx], "npc"),
                                 height = unit(data$vp.height[row.idx], "npc"),
                                 just = c(data$hjust[row.idx], data$vjust[row.idx]),
                                 angle = data$angle[row.idx],
                                 name = paste("geom_grob.panel", data$PANEL[row.idx],
                                              "row", row.idx, sep = "."))

                # give unique name to each grob
                userGrob$name <- paste(sides, "margin.grob", row.idx, sep = ".")

                user.grobs[[row.idx]] <- userGrob
              }
            }

            # needed for handling flipped coords
            if (!flipped && !is.null(data$yintercept) &&
                (grepl("l", sides) || grepl("r", sides) ) ) {
              if (grepl("l", sides)) {
                x.pos <- unit(grob.shift, "npc")
              } else if (grepl("r", sides)) {
                x.pos <- unit(1 - grob.shift, "npc")
              }
              for (row.idx in seq_len(nrow(data))) {
                userGrob <- data$label[[row.idx]]
                userGrob$vp <-
                  grid::viewport(x = x.pos,
                                 y = unit(data$yintercept[row.idx], "native"),
                                 width = unit(data$vp.width[row.idx], "npc"),
                                 height = unit(data$vp.height[row.idx], "npc"),
                                 just = c(data$hjust[row.idx], data$vjust[row.idx]),
                                 angle = data$angle[row.idx],
                                 name = paste("geom_y_margin_grob.panel", data$PANEL[row.idx],
                                              "row", row.idx, sep = "."))

                # give unique name to each grob
                userGrob$name <- paste("inset.grob", row.idx, sep = ".")

                user.grobs[[row.idx]] <- userGrob
              }
            }

            grid.name <- paste("geom_y_margin_grob.panel",
                               data$PANEL[row.idx], sep = ".")

            grid::gTree(children = user.grobs, name = grid.name)
          },

          draw_key = function(...) {
            grid::nullGrob()
          }
  )


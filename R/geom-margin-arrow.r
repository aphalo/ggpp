#' Reference arrows on the margins
#'
#' Small arrows on plot margins can supplement a 2d display with annotations.
#' Arrows can be used to highlight specific values along a margin. The geometries
#' \code{geom_x_margin_arrow()} and \code{geom_y_margin_arrow()} behave
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
#'   default plot specification, e.g., \code{\link[ggplot2]{borders}}.
#' @param sides A string that controls which sides of the plot the rugs appear
#'   on. It can be set to a string containing any combination of \code{"trbl"},
#'   for top, right, bottom, and left.
#' @param arrow.length numeric value expressed in npc units for the length of the
#'   arows inwards from the edge of the plotting area.
#' @param xintercept,yintercept numeric Parameters that control the position of
#'   the marginal points. If these are set, data, mapping and show.legend are
#'   overridden.
#'
#' @family Geometries for marginal annotations in ggplots
#'
#' @return A plot layer instance.
#'
#' @export
#' @examples
#' p <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#' p
#' p + geom_x_margin_arrow(xintercept = 3.5)
#' p + geom_y_margin_arrow(yintercept = c(18, 28, 15))
#' p + geom_x_margin_arrow(data = data.frame(x = c(2.5, 4.5)),
#'                         mapping = aes(xintercept = x))
#' p + geom_x_margin_arrow(data = data.frame(x = c(2.5, 4.5)),
#'                         mapping = aes(xintercept = x),
#'                         sides="tb")
#'
geom_x_margin_arrow <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                ...,
                                xintercept,
                                sides = "b",
                                arrow.length = 0.03,
                                na.rm = FALSE,
                                show.legend = FALSE,
                                inherit.aes = FALSE) {

  # Act like an annotation
  if (!missing(xintercept)) {
    data <- as.data.frame(list(xintercept = xintercept))
    mapping <- ggplot2::aes(xintercept = xintercept)
    show.legend <- FALSE
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomXMarginArrow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sides = sides,
      arrow.length = arrow.length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomXMarginArrow <-
  ggplot2::ggproto("GeomXMarginArrow", ggplot2::Geom,
          required_aes = c("xintercept"),
          non_missing_aes = c("size", "shape", "colour"),
          default_aes = ggplot2::aes(colour = "red", size = 1,
                                     fill = "red", alpha = NA),

  draw_panel = function(data, panel_params, coord, sides = "b",
                        arrow.length = 0.03, na.rm = FALSE) {
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }

    rugarrows <- list()
    data <- coord$transform(data, panel_params)

    # For coord_flip, coord$tranform does not flip the sides where to
    # draw the rugs. We have to flip them.
    flipped <- inherits(coord, "CoordFlip")
    if (flipped) {
      sides <- chartr("tblr", "rlbt", sides)
    }

    arrow <- arrow(length = unit(arrow.length / 3, "npc"), ends = "first", type = "open")
    gp <- gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)
    if (!flipped && !is.null(data$xintercept)) {
      if (grepl("b", sides)) {
        rugarrows$x_b <- grid::segmentsGrob(
          x0 = grid::unit(data$xintercept, "native"),
          x1 = grid::unit(data$xintercept, "native"),
          y0 = grid::unit(0, "npc"),
          y1 = grid::unit(arrow.length, "npc"),
          arrow = arrow,
          gp = gp
        )
      }

      if (grepl("t", sides)) {
        rugarrows$x_t <- grid::segmentsGrob(
          x0 = grid::unit(data$xintercept, "native"),
          x1 = grid::unit(data$xintercept, "native"),
          y0 = grid::unit(1, "npc"), y1 = unit(1 - arrow.length, "npc"),
          arrow = arrow,
          gp = gp
        )
      }
    }

    # needed for handling flipped coords
    if (flipped && !is.null(data$yintercept)) {
      if (grepl("l", sides)) {
        rugarrows$y_l <- segmentsGrob(
          y0 = unit(data$yintercept, "native"), y1 = unit(data$yintercept, "native"),
          x0 = unit(0, "npc"), x1 = unit(arrow.length, "npc"),
          arrow = arrow,
          gp = gp
        )
      }

      if (grepl("r", sides)) {
        rugarrows$y_r <- segmentsGrob(
          y0 = unit(data$yintercept, "native"), y1 = unit(data$yintercept, "native"),
          x0 = unit(1, "npc"), x1 = unit(1 - arrow.length, "npc"),
          arrow = arrow,
          gp = gp
        )
      }
    }

    gTree(children = do.call("gList", rugarrows))
  },

  draw_key = ggplot2:::draw_key_path
)

## y axis marging
#' @rdname geom_x_margin_arrow
#' @export
#'
geom_y_margin_arrow <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                ...,
                                yintercept,
                                sides = "l",
                                arrow.length = 0.03,
                                na.rm = FALSE,
                                show.legend = FALSE,
                                inherit.aes = FALSE) {

  # Act like an annotation
  if (!missing(yintercept)) {
    data <- as.data.frame(list(yintercept = yintercept))
    mapping <- ggplot2::aes(yintercept = yintercept)
    show.legend <- FALSE
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomYMarginArrow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      sides = sides,
      arrow.length = arrow.length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomYMarginArrow <-
  ggproto("GeomYMarginArrow", Geom,
          required_aes = c("yintercept"),
          non_missing_aes = c("size", "shape", "colour"),
          default_aes = aes(colour = "red", size = 1,
                            fill = "red", alpha = NA),

          draw_panel = function(data, panel_params, coord, sides = "l",
                                arrow.length = 0.03, na.rm = FALSE) {
            if (is.character(data$shape)) {
              data$shape <- translate_shape_string(data$shape)
            }

            rugarrows <- list()
            data <- coord$transform(data, panel_params)

            # For coord_flip, coord$tranform does not flip the sides where to
            # draw the rugs. We have to flip them.
            flipped <- inherits(coord, "CoordFlip")
            if (flipped) {
              sides <- chartr("tblr", "rlbt", sides)
            }

            arrow <- arrow(length = unit(arrow.length / 3, "npc"), ends = "first", type = "open")
            gp <- gpar(col = alpha(data$colour, data$alpha), lty = data$linetype, lwd = data$size * .pt)

            if (!flipped && !is.null(data$yintercept)) {
              if (grepl("l", sides)) {
                rugarrows$y_l <- segmentsGrob(
                  y0 = unit(data$yintercept, "native"), y1 = unit(data$yintercept, "native"),
                  x0 = unit(0, "npc"), x1 = unit(arrow.length, "npc"),
                  arrow = arrow,
                  gp = gp
                )
              }

              if (grepl("r", sides)) {
                rugarrows$y_r <- segmentsGrob(
                  y0 = unit(data$yintercept, "native"), y1 = unit(data$yintercept, "native"),
                  x0 = unit(1, "npc"), x1 = unit(1 - arrow.length, "npc"),
                  arrow = arrow,
                  gp = gp
                )
              }
            }
            # needed for handling flipped coords
            if (flipped && !is.null(data$xintercept)) {
              if (grepl("b", sides)) {
                rugarrows$x_b <- segmentsGrob(
                  x0 = unit(data$xintercept, "native"), x1 = unit(data$xintercept, "native"),
                  y0 = unit(0, "npc"), y1 = unit(arrow.length, "npc"),
                  arrow = arrow,
                  gp = gp
                )
              }

              if (grepl("t", sides)) {
                rugarrows$x_t <- segmentsGrob(
                  x0 = unit(data$xintercept, "native"), x1 = unit(data$xintercept, "native"),
                  y0 = unit(1, "npc"), y1 = unit(1 - arrow.length, "npc"),
                  arrow = arrow,
                  gp = gp
                )
              }
            }

            gTree(children = do.call("gList", rugarrows))
          },


          draw_key = ggplot2:::draw_key_path
  )

#' Points linked by a segment
#'
#' The geometry \code{"geom_point_s"} provides a super set of the capabilities of
#' geom \code{\link[ggplot2]{geom_point}} from package 'ggplot2' by allowing
#' plotting of arrows or segments joining the original position of displaced observations
#' to their current position rendered as points or graphic symbols. The most
#' common use is to demonstrate the action of different position functions. It
#' can be also used to highlight observations.
#'
#' @details The plotting of segments is similar in idea to that implemented in
#'   \code{\link[ggrepel]{geom_text_repel}} and relies on position functions
#'   that rename instead of only replacing the original \code{x} and \code{y}
#'   coordinates from the \code{data} object.
#'
#'   By default this geom uses \code{\link{position_nudge_center}} which is backwards
#'   compatible with \code{\link[ggplot2]{position_nudge}} but provides additional control
#'   on the direction of the nudging.
#'
#' @inheritSection geom_text_s Position functions
#'
#' @inherit geom_grob note return
#
#' @seealso \code{\link[ggplot2]{geom_point}}.
#'
#' @param mapping Set of aesthetic mappings created by
#'   \code{\link[ggplot2]{aes}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), is combined with the default mapping at the top level of the
#'   plot. You only need to supply \code{mapping} if there isn't a mapping
#'   defined for the plot.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There
#'   are three types of arguments you can use here:
#'   \itemize{ \item Aesthetics: to set an aesthetic to a fixed value, like
#'   \code{colour = "red"} or \code{size = 3}. \item Other arguments to the
#'   layer, for example you override the default \code{stat} associated with the
#'   layer. \item Other arguments passed on to the stat. }
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label. The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param move.point logical If \code{TRUE} the point is drawn at the nudged
#'   position while if \code{FALSE} the point is drawn at the original position.
#' @param default.colour,default.color A colour definition to use for elements not targeted by
#'   the colour aesthetic.
#' @param colour.target,color.target A character string, one of \code{"all"},
#'   \code{"point"} and \code{"segment"} or \code{"none"}.
#' @param default.alpha numeric in [0..1] A transparency value to use for
#'   elements not targeted by the alpha aesthetic.
#' @param alpha.target A character string, one of \code{"all"},
#'   \code{"segment"}, \code{"point"}, or \code{"none"}.
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
#' @export
#'
#' @examples
#'
#' # Same output as with geom_point()
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy)) +
#'   geom_point_s(colour = "blue")
#'
#' # with segment drawn after nudging
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point_s(position = position_nudge_keep(x = 0.2),
#'                colour = "red") +
#'   geom_point_s(colour = "blue") +
#'   expand_limits(x = c(3.5, 8.5))
#'
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point_s(position = position_nudge_keep(x = 0.2),
#'                colour = "blue",
#'                move.point = FALSE) +
#'   expand_limits(x = c(3.5, 8.5))
#'
#' # with segment drawn after nudging
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point_s(position = position_nudge_keep(x = 0.2),
#'                colour = "red",
#'                colour.target = "all") +
#'   geom_point_s(colour = "blue")
#'
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point_s(position = position_nudge_keep(x = 0.2),
#'                colour = "red",
#'                colour.target = "segment") +
#'   geom_point_s(colour = "blue")
#'
#' ggplot(mpg[1:20, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point_s(position = position_nudge_keep(x = 0.2),
#'                colour = "red",
#'                colour.target = "point") +
#'   geom_point_s(colour = "blue")
#'
#' ggplot(mpg[1:50, ],
#'        aes(cyl, hwy, label = drv)) +
#'   geom_point_s(position = position_jitternudge(width = 0.66, height = 2,
#'                                                seed = 456,
#'                                                nudge.from = "jittered",
#'                                                kept.origin = "original"),
#'                colour = "red",
#'                alpha = 0.3, alpha.target = "segment",
#'                arrow = grid::arrow(length = grid::unit(0.4, "lines"),
#'                                    ends = "first")) +
#'   geom_point_s(colour = "blue")
#'
geom_point_s <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         nudge_x = 0,
                         nudge_y = 0,
                         move.point = TRUE,
                         arrow = grid::arrow(length = unit(1/3, "lines"),
                                             ends = "first"),
                         default.colour = "black",
                         default.color = default.colour,
                         colour.target = "point",
                         color.target = colour.target,
                         default.alpha = 1,
                         alpha.target = "all",
                         add.segments = TRUE,
                         box.padding = 0.25,
                         point.padding = 1e-06,
                         segment.linewidth = 0.5,
                         min.segment.length = 0,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  colour.target <-
    rlang::arg_match(color.target,
                     values = c("point", "all", "segment", "none"),
                     multiple = TRUE)
  alpha.target <-
    rlang::arg_match(alpha.target,
                     values = c("point", "all", "segment", "none"),
                     multiple = TRUE)

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position) && position != "identity") {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }
    # We drop original positions if they are not needed for segments
    # (Justification is not supported)
    position <-
      position_nudge_center(nudge_x, nudge_y,
                            kept.origin = ifelse(add.segments,
                                                 "original", "none"))
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointS,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      move.point = move.point,
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
GeomPointS <-
  ggplot2::ggproto("GeomPointS", Geom,
                   required_aes = c("x", "y"),
                   non_missing_aes = c("size", "shape", "colour"),
                   default_aes = ggplot2::aes(
                     shape = 19,
                     colour = "black",
                     size = 1.5,
                     fill = NA,
                     alpha = NA,
                     stroke = 0.5
                   ),

                   draw_panel = function(data,
                                         panel_params,
                                         coord,
                                         move.point = TRUE,
                                         default.colour = "black",
                                         colour.target = "point",
                                         default.alpha = 1,
                                         alpha.target = "segment",
                                         na.rm = FALSE,
                                         arrow = NULL,
                                         box.padding = 0.25,
                                         point.padding = 1e-06,
                                         segment.linewidth = 0.5,
                                         min.segment.length = 0,
                                         add.segments = FALSE) {

                     if (is.character(data$shape)) {
                       data$shape <- translate_shape_string(data$shape)
                     }

                     if (nrow(data) == 0L) {
                       return(nullGrob())
                     }

                     add.segments <- add.segments && all(c("x_orig", "y_orig") %in% colnames(data))

                     data <- coord$transform(data, panel_params)
                     if (all(c("x_orig", "y_orig") %in% colnames(data))) {
                       data_orig <- data.frame(x = data$x_orig, y = data$y_orig)
                       data_orig <- coord$transform(data_orig, panel_params)
                       data$x_orig <- data_orig$x
                       data$y_orig <- data_orig$y
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

                     if (move.point) {
                       x0.name <- "x"
                       y0.name <- "y"
                       x1.name <- "x_orig"
                       y1.name <- "y_orig"
                     } else {
                       x0.name <- "x_orig"
                       y0.name <- "y_orig"
                       x1.name <- "x"
                       y1.name <- "y"
                     }
                     for (row.idx in 1:nrow(data)) {
                       row <- data[row.idx, , drop = FALSE]
                       point.alpha <-
                         ifelse(any(alpha.target %in% c("all", "point")),
                                row$alpha, default.alpha)
                       segment.alpha <-
                         ifelse(any(alpha.target %in% c("all", "segment")),
                                row$alpha, default.alpha)
                       user.grob <- grid::pointsGrob(
                         row[[x0.name]], row[[y0.name]], default.units = "native",
                         pch = row$shape,
                         gp = gpar(
                           col = ifelse(any(colour.target %in% c("all", "point")),
                                        ggplot2::alpha(row$colour, point.alpha),
                                        ggplot2::alpha(default.colour, point.alpha)),
                           fill = alpha(row$fill, point.alpha),
                           # Stroke is added around the outside of the point
                           fontsize = row$size * .pt + row$stroke * ggplot2::.stroke / 2,
                           lwd = row$stroke * ggplot2::.stroke / 2
                         )
                       )

                       # give unique name to each grob
                       user.grob$name <- paste("point.s.grob", row$group, row.idx, sep = ".")

                       if (add.segments) {
                         segment.row <- segments.data[row.idx, , drop = FALSE]
                         if (segment.row$too.short) {
                           segment.grob <- grid::nullGrob()
                         } else {
                           segment.grob <-
                             grid::segmentsGrob(x0 = segment.row[[x0.name]],
                                                y0 = segment.row[[y0.name]],
                                                x1 = segment.row[[x1.name]],
                                                y1 = segment.row[[y1.name]],
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

                     # name needs to be unique within plot, so we would to know other layers
                     #                     grid::grobTree(children = all.grobs, name = "geom.point.s.panel")
                     grid::grobTree(children = all.grobs)

                   },

                   draw_key = ggplot2::draw_key_point
  )

# adopts from 'ggplot2' rejected pull-request #5143 by Teun van den Brand
# addresing issue #5088.
# 'blank' and 'dot' deviate from the grammar of graphics but
# are consistent with underlying R and 'grid' pch codes
#
translate_shape_string <- function(shape_string) {
  # strings of length 0 or 1 are interpreted as symbols by grid
  if (nchar(shape_string[1]) <= 1) {
    return(shape_string)
  }

  pch_table <- c(
    "square open"           = 0,
    "circle open"           = 1,
    "triangle open"         = 2,
    "plus"                  = 3,
    "cross"                 = 4,
    "diamond open"          = 5,
    "triangle down open"    = 6,
    "square cross"          = 7,
    "asterisk"              = 8,
    "diamond plus"          = 9,
    "circle plus"           = 10,
    "star"                  = 11,
    "square plus"           = 12,
    "circle cross"          = 13,
    "square triangle"       = 14,
    "triangle square"       = 14,
    "square"                = 15,
    "circle small"          = 16,
    "triangle"              = 17,
    "diamond"               = 18,
    "circle"                = 19,
    "bullet"                = 20,
    "circle filled"         = 21,
    "square filled"         = 22,
    "diamond filled"        = 23,
    "triangle filled"       = 24,
    "triangle down filled"  = 25,
    "dot"                   = 46,
    "blank"                 = NA
  )

  shape_match <- charmatch(shape_string, names(pch_table), nomatch = 99)

  invisible_shape <- anyNA(shape_match)
  invalid_strings <- shape_match == 99
  nonunique_strings <- shape_match == 0

  if (any(invalid_strings)) {
    bad_string <- unique(shape_string[invalid_strings])
    n_bad <- length(bad_string)

    collapsed_names <- sprintf("\n* '%s'", bad_string[1:min(5, n_bad)])

    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }

    stop(paste("Can't find shape name:", collapsed_names, more_problems))
  }

  if (any(nonunique_strings)) {
    bad_string <- unique(shape_string[nonunique_strings])
    n_bad <- length(bad_string)

    n_matches <- vapply(
      bad_string[1:min(5, n_bad)],
      function(shape_string) sum(grepl(paste0("^", shape_string), names(pch_table))),
      integer(1)
    )

    collapsed_names <- sprintf(
      "\n* '%s' partially matches %d shape names",
      bad_string[1:min(5, n_bad)], n_matches
    )

    more_problems <- if (n_bad > 5) {
      sprintf("\n* ... and %d more problem%s", n_bad - 5, ifelse(n_bad > 6, "s", ""))
    } else {
      ""
    }

    stop(paste("Shape names must be unambiguous:", collapsed_names, more_problems))
  }

  if (invisible_shape) {
    message("Invisible shape in use: not all data points plotted!!")
  }
  unname(pch_table[shape_match])
}

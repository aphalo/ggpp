#' Points linked by a segment or arrow
#'
#' The "point_linked" geom is used to display displacement of points in
#' scatterplots.
#'
#' @seealso [ggplot2::geom_point], [nudge_keep] and [nudge_to]
#'
#' @param mapping Set of aesthetic mappings created by
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. If specified and
#'   \code{inherit.aes = TRUE} (the default), is combined with the default
#'   mapping at the top level of the plot. You only need to supply
#'   \code{mapping} if there isn't a mapping defined for the plot.
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
#'
#'   \itemize{ \item Aesthetics: to set an aesthetic to a fixed value, like
#'   \code{colour = "red"} or \code{size = 3}. \item Other arguments to the
#'   layer, for example you override the default \code{stat} associated with the
#'   layer. \item Other arguments passed on to the stat. }
#' @param arrow specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}
#'
#' @return A plot layer instance.
#' @export
#' @examples
#'
#' ggplot(mpg[1:20, ],
#'        aes(cyl,
#'            hwy,
#'            label = drv)) +
#'   geom_point_linked(position = position_nudge_keep(x = 0.2),
#'                     color = "red", segment.colour = "brown") +
#'   geom_point()
#'
#' ggplot(mpg[1:50, ],
#'        aes(cyl,
#'            hwy,
#'            label = drv)) +
#'   geom_point_linked(position = position_jitter_and_nudge(width = 0.66, height = 2,
#'                                                          seed = 456,
#'                                                   nudge.from = "jittered",
#'                                                   returned.origin = "original"),
#'                     color = "red", arrow = grid::arrow(length = grid::unit(0.4, "lines"))) +
#'   geom_point()
#'
geom_point_linked <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              ...,
                              arrow = NULL,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointLinked,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
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
GeomPointLinked <- ggplot2::ggproto("GeomPointLinked", Geom,
  required_aes = c("x", "y"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = ggplot2::aes(
    shape = 19,
    colour = "black",
    size = 1.5,
    fill = NA,
    alpha = NA,
    stroke = 0.5,
    segment.linetype = 1,
    segment.colour = "grey33",
    segment.size = 0.5,
    segment.alpha = 1
  ),

  draw_panel = function(data,
                        panel_params,
                        coord,
                        na.rm = FALSE,
                        arrow = NULL) {
    if (is.character(data$shape)) {
      data$shape <- ggplot2::translate_shape_string(data$shape)
    }

    if (nrow(data) == 0L) {
      return(nullGrob())
    }

    add.links <- all(c("x_orig", "y_orig") %in% colnames(data))

    coords <- coord$transform(data, panel_params)
    if (add.links) {
      data_orig <- data.frame(x = data$x_orig, y = data$y_orig)
      data_orig <- coord$transform(data_orig, panel_params)
    }

    # create the grobs
    if(add.links) {
      ggname("geom_point_linked",
             grid::grobTree(
               grid::segmentsGrob(
                 x0 = data_orig$x,
                 y0 = data_orig$y,
                 x1 = coords$x,
                 y1 = coords$y,
                 arrow = arrow,
                 gp = grid::gpar(col = alpha(coords$segment.colour,
                                             coords$segment.alpha))
               ),
               grid::pointsGrob(
                 coords$x, coords$y,
                 pch = coords$shape,
                 gp = gpar(
                   col = alpha(coords$colour, coords$alpha),
                   fill = alpha(coords$fill, coords$alpha),
                   # Stroke is added around the outside of the point
                   fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                   lwd = coords$stroke * .stroke / 2
                 )
               )
             )
      )
    } else {
      ggname("geom_point_linked",
             grid::pointsGrob(
               coords$x, coords$y,
               pch = coords$shape,
               gp = gpar(
                 col = alpha(coords$colour, coords$alpha),
                 fill = alpha(coords$fill, coords$alpha),
                 # Stroke is added around the outside of the point
                 fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
                 lwd = coords$stroke * .stroke / 2
               )
             )
      )
    }
  },

  draw_key = ggplot2::draw_key_point
)

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
    "triangle down filled"  = 25
  )

  shape_match <- charmatch(shape_string, names(pch_table))

  invalid_strings <- is.na(shape_match)
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

  unname(pch_table[shape_match])
}

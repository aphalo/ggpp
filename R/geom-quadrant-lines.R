#' Reference lines: horizontal plus vertical, and quadrants
#'
#' \code{geom_vhlines()} adds in a single layer both vertical and horizontal
#' guide lines. Can be thought of as a convenience function that helps with
#' producing consistent vertical and horizontal guide lines. It behaves like
#' \code{geom_vline()} and \code{geom_hline()}.
#' \code{geom_quadrant_lines()} displays the boundaries of four quadrants
#' with an arbitrary origin. The quadrants are specified in the same way as
#' in \code{stat_quadrant_counts()} and is intended to be used to add guide
#' lines consistent with the counts by quadrant computed by this stat.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific data set - only needed if you want to override
#'   the plot defaults.
#' @param stat The statistic object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and should not inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether NA values should be stripped before
#'   the computation proceeds.
#' @param pool.along character, one of "none", "x" or "y", indicating which
#'   quadrants to pool to calculate counts by pair of quadrants.
#' @param xintercept,yintercept numeric vectors the coordinates of the origin of the
#'   quadrants.
#'
#' @details While \code{geom_vhlines()} does not provide defaults for the
#'   intercepts and accept vectors of length > 1, \code{geom_quadrant_lines()}
#'   sets by default the intercepts to zero producing the natural quadrants and
#'   only accepts vectors of length one per panel. That is \code{geom_vhlines()}
#'   can be used to plot a grid while \code{geom_quadrant_lines()} plots at
#'   most one vertical and one horizontal line. In the case of
#'   \code{geom_quadrant_lines()} the pooling along axes can be specified in the
#'   same way as in \code{\link{stat_quadrant_counts}()}.
#'
#' @family Functions for quadrant and volcano plots
#'
#' @seealso \code{\link[ggplot2]{geom_abline}}, the topic where
#'   \code{geom_vline()} and \code{geom_hline()} are described.
#'
#' @return A plot layer instance.
#'
#' @export
#'
#' @examples
#' # generate artificial data
#' set.seed(4321)
#' x <- 1:100
#' y <- rnorm(length(x), mean = 10)
#' my.data <- data.frame(x, y)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_quadrant_lines() +
#'   geom_point()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_quadrant_lines(linetype = "dotted") +
#'   geom_point()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_quadrant_lines(xintercept = 50, yintercept = 10, colour = "blue") +
#'   geom_point()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_quadrant_lines(xintercept = 50, pool.along = "y", colour = "blue") +
#'   geom_point()
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_vhlines(xintercept = c(25, 50, 75), yintercept = 10 ,
#'                linetype = "dotted", colour = "red") +
#'   geom_point() +
#'   theme_bw()
#'
geom_quadrant_lines <- function(mapping = NULL,
                                data = NULL,
                                stat = "identity",
                                position = "identity",
                                pool.along = "none",
                                xintercept = 0,
                                yintercept = 0,
                                na.rm = FALSE,
                                show.legend = FALSE,
                                inherit.aes = FALSE, ...) {

  stopifnot(pool.along %in% c("none", "x", "y"))
  stopifnot(length(xintercept) <= 1 && length(yintercept) <= 1)

  # Act like an annotation
  if (!is.null(xintercept) && !is.null(yintercept)) {
    data <- as.data.frame(list(xintercept = xintercept,
                               yintercept = yintercept))
    mapping <- ggplot2::aes(xintercept = xintercept,
                            yintercept = yintercept)
    show.legend <- FALSE
  } else if (xor(missing(xintercept), missing(yintercept))) {
    stop("Missing 'xintercept' and 'yintercept'")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomQuadrantLines,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      pool.along = pool.along,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomQuadrantLines <-
  ggplot2::ggproto(
    "GeomQuadrantLines", Geom,
    draw_panel = function(data, panel_params, coord, pool.along = "none", lineend = "butt") {
      ranges <- coord$backtransform_range(panel_params)
      data.hline <- data.vline <- data

      if (!grepl("y", x = pool.along, ignore.case = TRUE)) {
        data.hline[["x"]]    <- ranges[["x"]][1]
        data.hline[["xend"]] <- ranges[["x"]][2]
        data.hline[["y"]]    <- data[["yintercept"]][1]
        data.hline[["yend"]] <- data[["yintercept"]][1]
      }

      if (!grepl("x", x = pool.along, ignore.case = TRUE)) {
        data.vline[["x"]]    <- data[["xintercept"]][1]
        data.vline[["xend"]] <- data[["xintercept"]][1]
        data.vline[["y"]]    <- ranges[["y"]][1]
        data.vline[["yend"]] <- ranges[["y"]][2]
      }

      # discard infinite values
      # other off-range values still respect oob
      #
      data.hline <- data.hline[is.finite(data.hline[["y"]])]
      data.vline <- data.vline[is.finite(data.vline[["x"]])]

      data <- switch(tolower(pool.along),
                     none = rbind(data.hline, data.vline),
                     x = data.hline,
                     y = data.vline,
                     data[NULL, ]
      )

      ggplot2::GeomSegment$draw_panel(unique(data), panel_params, coord, lineend = lineend)

    },

    default_aes = ggplot2::aes(colour = "black",
                               size = 0.5, # ggplot2 (<= 3.3.6)
                               linewidth = 0.5, # ggplot2 (> 3.3.6)
                               linetype = "dashed",
                               alpha = NA),
    required_aes = c("xintercept", "yintercept"),
    non_missing_aes = c("size", "linetype", "colour"),

    draw_key = draw_key_path,

    rename_size = TRUE
  )

#' @rdname geom_quadrant_lines
#'
#' @export
#'
geom_vhlines <- function(mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                xintercept = NULL, yintercept = NULL,
                                na.rm = FALSE, show.legend = FALSE,
                                inherit.aes = FALSE, ...) {
  # Act like an annotation
  if (!is.null(xintercept) && !is.null(yintercept)) {
    data <- as.data.frame(list(xintercept = xintercept,
                               yintercept = yintercept))
    mapping <- ggplot2::aes(xintercept = xintercept,
                            yintercept = yintercept)
    show.legend <- FALSE
  } else if (xor(missing(xintercept), missing(yintercept))) {
    stop("Arguments should be passed either to none or both of 'xintrecept' and 'yintercept'")
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomVHLines,
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
GeomVHLines <-
  ggplot2::ggproto(
    "GeomVHLines", Geom,
    draw_panel = function(data, panel_params, coord, lineend = "butt") {
      ranges <- coord$backtransform_range(panel_params)
      data.hline <- data.vline <- data

      data.hline[["x"]]    <- ranges[["x"]][1]
      data.hline[["xend"]] <- ranges[["x"]][2]
      data.hline[["y"]]    <- data[["yintercept"]][1]
      data.hline[["yend"]] <- data[["yintercept"]][1]

      data.vline[["x"]]    <- data[["xintercept"]][1]
      data.vline[["xend"]] <- data[["xintercept"]][1]
      data.vline[["y"]]    <- ranges[["y"]][1]
      data.vline[["yend"]] <- ranges[["y"]][2]

      data <- rbind(data.hline, data.vline)

      ggplot2::GeomSegment$draw_panel(unique(data), panel_params, coord, lineend = lineend)

    },

    default_aes = ggplot2::aes(colour = "black",
                               size = 0.5, # ggplot2 (<= 3.3.6)
                               linewidth = 0.5, # ggplot2 (> 3.3.6)
                               linetype = 1,
                               alpha = NA),
    required_aes = c("xintercept", "yintercept"),
    non_missing_aes = c("size", "linetype", "colour"),

    draw_key = draw_key_path,

    rename_size = TRUE
  )

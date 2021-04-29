### Based on position-dodge.r from 'ggplot2' as of 2021-04-03
###
### Minor addition to code so that the original position is also returned.
###
#' Dodge overlapping objects side-to-side
#'
#' Dodging preserves the vertical position of an geom while adjusting the
#' horizontal position. `position_dodge_keep()` requires the grouping variable to be
#' be specified in the global or `geom_*` layer.
#'
#' @param width Dodging width, when different to the width of the individual
#'   elements. This is useful when you want to align narrow geoms with wider
#'   geoms. See the examples.
#' @param preserve Should dodging preserve the total width of all elements
#'    at a position, or the width of a single element?
#' @family position adjustments
#' @export
#' @examples
#'
#' # Dodging with various widths -------------------------------------
#' # To dodge items with different widths, you need to be explicit
#' df <- data.frame(
#'   x = c("a","a","b","b"),
#'   y = 2:5,
#'   g = rep(1:2, 2)
#' )
#' p <- ggplot(df, aes(x, y, group = g)) +
#'   geom_col(position = "dodge_keep", fill = "grey50", colour = "black")
#' p
#'
#' # A line range has no width:
#' p + geom_linerange(aes(ymin = y - 1, ymax = y + 1), position = "dodge_keep")
#'
#' # So you must explicitly specify the width
#' p + geom_linerange(
#'   aes(ymin = y - 1, ymax = y + 1),
#'   position = position_dodge_keep(width = 0.9)
#' )
#'
#' # The same principle applies to error bars, which are usually
#' # narrower than the bars
#' p + geom_errorbar(
#'   aes(ymin = y - 1, ymax = y + 1),
#'   width = 0.2,
#'   position = "dodge_keep"
#' )
#' p + geom_errorbar(
#'   aes(ymin = y - 1, ymax = y + 1),
#'   width = 0.2,
#'   position = position_dodge(width = 0.9)
#' )
#'
position_dodge_keep <- function(width = NULL, preserve = c("total", "single")) {
  ggproto(NULL, PositionDodgeKeep,
    width = width,
    preserve = match.arg(preserve)
  )
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
PositionDodgeKeep <- ggproto("PositionDodgeKeep", Position,
  width = NULL,
  preserve = "total",
  setup_params = function(self, data) {
    flipped_aes <- has_flipped_aes(data)
    data <- flip_data(data, flipped_aes)
    if (is.null(data$xmin) && is.null(data$xmax) && is.null(self$width)) {
      warn("Width not defined. Set with `position_dodge_keep(width = ?)`")
    }

    if (identical(self$preserve, "total")) {
      n <- NULL
    } else {
      panels <- unname(split(data, data$PANEL))
      ns <- vapply(panels, function(panel) max(table(panel$xmin)), double(1))
      n <- max(ns)
    }

    list(
      width = self$width,
      n = n,
      flipped_aes = flipped_aes
    )
  },

  setup_data = function(self, data, params) {
    data <- flip_data(data, params$flipped_aes)
    if (!"x" %in% names(data) && all(c("xmin", "xmax") %in% names(data))) {
      data$x <- (data$xmin + data$xmax) / 2
    }
    flip_data(data, params$flipped_aes)
  },

  compute_panel = function(data, params, scales) {
    x_orig <- data$x
    y_orig <- data$y
    data <- ggplot2::flip_data(data, params$flipped_aes)
    collided <- collide(
      data,
      params$width,
      name = "position_dodge_keep",
      strategy = pos_dodge,
      n = params$n,
      check.width = FALSE
    )
    data <- ggplot2::flip_data(collided, params$flipped_aes)
    data$x_orig <- x_orig
    data$y_orig <- y_orig
    data
  }
)

# Dodge overlapping interval.
# Assumes that each set has the same horizontal position.
pos_dodge <- function(df, width, n = NULL) {
  if (is.null(n)) {
    n <- length(unique(df$group))
  }

  if (n == 1)
    return(df)

  if (!all(c("xmin", "xmax") %in% names(df))) {
    df$xmin <- df$x
    df$xmax <- df$x
  }

  d_width <- max(df$xmax - df$xmin)

  # Have a new group index from 1 to number of groups.
  # This might be needed if the group numbers in this set don't include all of 1:n
  groupidx <- match(df$group, sort(unique(df$group)))

  # Find the center for each group, then use that to calculate xmin and xmax
  df$x <- df$x + width * ((groupidx - 0.5) / n - .5)
  df$xmin <- df$x - d_width / n / 2
  df$xmax <- df$x + d_width / n / 2

  df
}

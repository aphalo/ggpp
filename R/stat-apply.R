#' Apply a function to x or y values
#'
#' \code{stat_summary_xy()} and \code{stat_centroid()} are similar to
#' \code{ggplot2::stat_summary()} but summarize both \code{x} and \code{y}
#' values in the same plot layer. This makes it possible to highight the actual
#' location of the centroid with \code{geom_point()}, \code{geom_text()}, and
#' similar geometries. Instead, if we use \code{geom_rug()} they are only a
#' convenience avoiding the need to add two separate layers and flipping one of
#' them using \code{orientation = "y"}.
#'
#' \code{stat_apply_group} and \code{stat_apply_panel} apply functions to data.
#' When possible it is preferable to use transformations through scales or
#' summary functions such as \code{ggplot2::stat_summary()},
#' \code{stat_summary_xy()} or \code{stat_centroid()}. There are some
#' computations that are not scale transformations but are not usual summaries
#' either, as the number of data values does not decrease all the way to one row
#' per group. A typical case for a summary is the computation of quantiles. For
#' transformations are cumulative ones, e.g., using \code{cumsum()},
#' \code{runmed()} and similar functions. Obviously, it is always possible to
#' apply such functions to the data before plotting and passing them to a single
#' layer function. However, it can be useful to apply such functions on-the-fly
#' to ensure that grouping is consistent between computations and aesthetics.
#' One particularity of these statistics is that they can apply simultaneously
#' different functions to \code{x} values and to \code{y} values when needed. In
#' contrast to these statistics, \code{\link[ggplot2]{geom_smooth}} applies a
#' function that takes both \code{x} and \code{y} values as arguments.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you
#'   are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this
#'   layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical value indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param .fun.x,.fun.y,.fun function to be applied or the name of the function
#'   to be applied as a character string.
#' @param .fun.x.args,.fun.y.args,.fun.args additional arguments to be passed to
#'   the function as a named list.
#'
#' @details These four statistics are very similar and are implemented using the
#'   same internal function. They differ on whether the function(s) is(are) applied by
#'   group or by panel, and whether they return a single or multiple rows of
#'   data.
#'
#' @return A data frame with the same variables as the data input, with either a
#'   single or multiple rows, with the values of \code{x} and \code{y} variables replaced by
#'   the values returned by the applied functions, or possibly filled if no
#'   function was supplied or available by default. If the applied function
#'   returns a named vector, the names are copied into columns \code{x.names}
#'   and/or \code{y.names}.
#'
#'   \describe{
#'   \item{x}{x-value as returned by \code{.fun.x}, with names removed}
#'   \item{y}{y-value as returned by \code{.fun.y}, with names removed}
#'   \item{x.names}{if the x-value returned by \code{.fun.x} is named, these names}
#'   \item{y.names}{if the y-value returned by \code{.fun.y} is named, these names}
#'   }
#'
#' @note The applied function(s) must accept as first argument a vector that
#'   matches the variables mapped to \code{x} or \code{y} aesthetics. For
#'   \code{stat_summary_xy()} and \code{stat_centroid()} the function(s) to be
#'   applied is(are) expected to return a vector of length 1, while for
#'   \code{stat_apply_group} and \code{stat_apply_panel} the vectors returned by
#'   the two functions applied to \code{x} and \code{y}, respectively, must
#'   return vectors of exactly the same length. When only one of \code{.fun.x}
#'   or \code{.fun.y} are passed a function as argument, and the returned value
#'   is shorter than the data, the values of the other variable in the returned
#'   data are filled with their median. If other values are desired, they can be
#'   set by means a function.
#'
##' @references
#'
#' Answers to question "R ggplot on-the-fly calculation by grouping variable" at
#' \url{https://stackoverflow.com/questions/51412522}.
#'
#' @examples
#' set.seed(123456)
#' my.df <- data.frame(X = rep(1:20,2),
#'                     Y = runif(40),
#'                     category = rep(c("A","B"), each = 20))
#'
#' # make sure rows are ordered for X as we will use functions that rely on this
#' my.df <- my.df[order(my.df[["X"]]), ]
#'
#' # Centroid
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_centroid(shape = "cross", size = 6) +
#'   geom_point()
#'
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_centroid(geom = "rug", size = 1.5, .fun = median) +
#'   geom_point()
#'
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_centroid(geom = "text", aes(label = category)) +
#'   geom_point()
#'
#' # quantiles
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   geom_point() +
#'   stat_apply_group(geom = "rug", .fun.y = quantile, .fun.x = quantile)
#'
#' ggplot(my.df, aes(x = X, y = Y)) +
#'   geom_point() +
#'   stat_apply_group(geom = "rug", sides = "lr", color = "darkred",
#'                    .fun.y = quantile) +
#'   stat_apply_group(geom = "text", hjust = "right", color = "darkred",
#'                    .fun.y = quantile,
#'                    .fun.x = function(x) {rep(22, 5)},
#'                    mapping = aes(label = after_stat(y.names))) +
#'                    expand_limits(x = 21)
#'
#' my.probs <- c(0.25, 0.5, 0.75)
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   geom_point() +
#'   stat_apply_group(geom = "hline",
#'                    aes(yintercept = after_stat(y)),
#'                    .fun.y = quantile,
#'                    .fun.y.args = list(probs = my.probs))
#'
#' # cummulative summaries
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_apply_group(.fun.y = cummax)
#'
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_apply_group(.fun.x = cumsum, .fun.y = cumsum)
#'
#' # diff returns a shorter vector by 1 for each group
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_apply_group(.fun.y = diff, na.rm = TRUE)
#'
#' # Running summaries
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   geom_point() +
#'   stat_apply_group(.fun.y = runmed, .fun.y.args = list(k = 5))
#'
#' # Rescaling per group
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_apply_group(.fun.y = function(x) {(x - min(x)) / (max(x) - min(x))})
#'
#' # inspecting the returned data
#' library(gginnards)
#'
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_apply_group(.fun.y = cumsum, geom = "debug")
#'
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   geom_point() +
#'   stat_apply_group(geom = "debug",
#'                    .fun.x = quantile,
#'                    .fun.x.args = list(probs = my.probs),
#'                    .fun.y = quantile,
#'                    .fun.y.args = list(probs = my.probs))
#'
#' @rdname stat_apply
#'
#' @export
#' @family summary stats
#'
stat_apply_group <- function(mapping = NULL,
                             data = NULL,
                             geom = "line",
                             .fun.x = NULL,
                             .fun.x.args = list(),
                             .fun.y = NULL,
                             .fun.y.args = list(),
                             position = "identity",
                             na.rm = FALSE,
                             show.legend = FALSE,
                             inherit.aes = TRUE,
                             ...) {
  .fun.x.null <- is.null(.fun.x)
  if (.fun.x.null) {
    .fun.x <- function(x) {x}
  }
  .fun.y.null <- is.null(.fun.y)
  if (.fun.y.null) {
    .fun.y <- function(x) {x}
  }
  ggplot2::layer(
    stat = StatApplyGroup,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(.fun.x = .fun.x,
                  .fun.x.args = .fun.x.args,
                  .fun.x.null = .fun.x.null,
                  .fun.y = .fun.y,
                  .fun.y.args = .fun.y.args,
                  .fun.y.null = .fun.y.null,
                  na.rm = na.rm,
                  single.row = FALSE,
                  ...)
  )
}

#' @rdname stat_apply
#'
#' @export
#'
stat_apply_panel <- function(mapping = NULL,
                             data = NULL,
                             geom = "line",
                             .fun.x = NULL,
                             .fun.x.args = list(),
                             .fun.y = NULL,
                             .fun.y.args = list(),
                             position = "identity",
                             na.rm = FALSE,
                             show.legend = FALSE,
                             inherit.aes = TRUE, ...) {
  .fun.x.null <- is.null(.fun.x)
  if (.fun.x.null) {
    .fun.x <- function(x) {x}
  }
  .fun.y.null <- is.null(.fun.y)
  if (.fun.y.null) {
    .fun.y <- function(x) {x}
  }
  ggplot2::layer(
    stat = StatApplyPanel, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(.fun.x = .fun.x,
                  .fun.x.args = .fun.x.args,
                  .fun.x.null = .fun.x.null,
                  .fun.y = .fun.y,
                  .fun.y.args = .fun.y.args,
                  .fun.y.null = .fun.y.null,
                  na.rm = na.rm,
                  single.row = FALSE,
                  ...)
  )
}

#' @rdname stat_apply
#'
#' @export
#'
stat_summary_xy <- function(mapping = NULL,
                            data = NULL,
                            geom = "point",
                            .fun.x = NULL,
                            .fun.x.args = list(),
                            .fun.y = NULL,
                            .fun.y.args = list(),
                            position = "identity",
                            na.rm = FALSE,
                            show.legend = FALSE,
                            inherit.aes = TRUE,
                            ...) {
  .fun.x.null <- is.null(.fun.x)
  if (.fun.x.null) {
    .fun.x <- function(x) {x}
  }
  .fun.y.null <- is.null(.fun.y)
  if (.fun.x.null) {
    .fun.y <- function(x) {x}
  }
  ggplot2::layer(
    stat = StatApplyGroup, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(.fun.x = .fun.x,
                  .fun.x.args = .fun.x.args,
                  .fun.x.null = .fun.x.null,
                  .fun.y = .fun.y,
                  .fun.y.args = .fun.y.args,
                  .fun.y.null = .fun.y.null,
                  na.rm = na.rm,
                  single.row = TRUE,
                  ...)
  )
}

#' @rdname stat_apply
#'
#' @export
#'
stat_centroid <- function(mapping = NULL,
                          data = NULL,
                          geom = "point",
                          .fun = mean,
                          .fun.args = list(),
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = FALSE,
                          inherit.aes = TRUE,
                          ...) {
  if (is.null(.fun)) {
    .fun <- mean
  }
  ggplot2::layer(
    stat = StatApplyGroup,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(.fun.x = .fun,
                  .fun.x.args = .fun.args,
                  .fun.x.null = FALSE,
                  .fun.y = .fun,
                  .fun.y.args = .fun.args,
                  .fun.y.null = FALSE,
                  na.rm = na.rm,
                  single.row = TRUE,
                  ...)
  )
}

# Define here to avoid a note in check as the import from 'dplyr' is not seen
# when the function is defined in-line in the ggproto object.
#' @rdname ggpp-ggproto
#'
#' @format NULL
#' @usage NULL
#'
stat_apply_fun <- function(data,
                           scales,
                           .fun.x, .fun.x.args, .fun.x.null,
                           .fun.y, .fun.y.args, .fun.y.null,
                           .fun.filler = stats::median,
                           single.row) {

  if (single.row) {
    stat_apply_fun_rw(data,
                      scales,
                      .fun.x, .fun.x.args, .fun.x.null,
                      .fun.y, .fun.y.args, .fun.y.null,
                      .fun.filler = stats::median)
  } else {
    stat_apply_fun_vc(data,
                      scales,
                      .fun.x, .fun.x.args, .fun.x.null,
                      .fun.y, .fun.y.args, .fun.y.null,
                      .fun.filler = stats::median)
  }
}

# Define here to avoid a note in check as the import from 'dplyr' is not seen
# when the function is defined in-line in the ggproto object.
#' @rdname ggpp-ggproto
#'
#' @format NULL
#' @usage NULL
#'
stat_apply_fun_vc <- function(data,
                              scales,
                              .fun.x, .fun.x.args, .fun.x.null,
                              .fun.y, .fun.y.args, .fun.y.null,
                              .fun.filler = stats::median) {

  #  Fill with NAs if returned vector is too short
  fill2length <- function(x, nrow) {
    c(x, rep(NA_real_, nrow - length(x)))
  }

  force(data)
  new.data <- data
  if (!is.null(.fun.x)) {
    args <- c(unname(data["x"]), .fun.x.args)
    new.x <- do.call(.fun.x, args = args)
    if (!is.null(names(new.x))) {
      new.data[["x.names"]] <-
        fill2length(names(new.x), nrow = nrow(new.data))
      new.x <- unname(new.x)
    }
    new.data[["x"]] <-
      fill2length(new.x, nrow = nrow(new.data))
  }
  if (!is.null(.fun.y)) {
    args <- c(unname(data["y"]), .fun.y.args)
    new.y <- do.call(.fun.y, args = args)
    if (!is.null(names(new.y))) {
      new.data[["y.names"]] <-
        fill2length(names(new.y), nrow = nrow(new.data))
      new.y <- unname(new.y)
    }
    new.data[["y"]] <-
      fill2length(new.y, nrow = nrow(new.data))
  }
  if (.fun.x.null) {
    selector <- !is.na(new.data[["y"]])
    new.data <- new.data[selector, ]
    # cummulative summaries and diff() can shorten the vector by one
    if (nrow(data) - nrow(new.data) > 2L) {
      new.data[["x"]] <- .fun.filler(data[["x"]])
    }
  } else if (.fun.y.null) {
    selector <- !is.na(new.data[["x"]])
    new.data <- new.data[selector, ]
    # cummulative summaries and diff() can shorten the vector by one
    if (nrow(data) - nrow(new.data) > 2L) {
      new.data[["y"]] <- .fun.filler(data[["y"]])
    }
  } else {
    selector <- !is.na(new.data[["x"]]) | !is.na(new.data[["y"]])
    new.data <- new.data[selector, ]
  }
  new.data
}

# Define here to avoid a note in check as the import from 'dplyr' is not seen
# when the function is defined in-line in the ggproto object.
#' @rdname ggpp-ggproto
#'
#' @format NULL
#' @usage NULL
#'
stat_apply_fun_rw <- function(data,
                              scales,
                              .fun.x, .fun.x.args, .fun.x.null,
                              .fun.y, .fun.y.args, .fun.y.null,
                              .fun.filler = stats::median) {

  #  Fill with NAs if returned vector is too short
  fill2length <- function(x, nrow) {
    c(x, rep(NA_real_, nrow - length(x)))
  }

  force(data)
  new.data <- data[1, ]
  unique_value_cols <-
    sapply(data, function(x) {length(unique(x)) == 1L})

  if (sum(!unique_value_cols) > 2L) {
    warning("Non-unique values in columns: ",
            setdiff(names(data)[!unique_value_cols], c("x", "y")))
  }
  new.data[ , !unique_value_cols] <- NA

  if (!is.null(.fun.x)) {
    args <- c(unname(data["x"]), .fun.x.args)
    new.x <- do.call(.fun.x, args = args)
    stopifnot((is.data.frame(new.x) && nrow(new.x) == 1L) ||
                 (is.vector(new.x) && length(new.x) == 1L))
    if (is.vector(new.x) && length(new.x) == 1L) {
      if(!is.null(names(new.x))) {
        new.data[["x.names"]] <- names(new.x)
        new.x <- unname(new.x)
      }
      new.data[["x"]] <- new.x
    } else if (is.data.frame(new.x) && nrow(new.x) == 1L) {
      # functions like mean_se() return columns y, ymin and ymax.
      names(new.x) <- gsub("y", "x", names(new.x))
      if ("x" %in% names(new.x)) {
        new.data <- cbind(new.data[ , setdiff(names(new.data), "x")],
                          new.x)
      } else {
        new.data <- cbind(new.data, new.x)
      }
    }
  }

  if (!is.null(.fun.y)) {
    args <- c(unname(data["y"]), .fun.y.args)
    new.y <- do.call(.fun.y, args = args)
    stopifnot((is.data.frame(new.y) && nrow(new.y) == 1L) ||
                (is.vector(new.y) && length(new.y) == 1L))
    if (is.vector(new.y) && length(new.y) == 1L) {
      if(!is.null(names(new.y))) {
        new.data[["y.names"]] <- names(new.y)
        new.y <- unname(new.y)
      }
      new.data[["y"]] <- new.y
    } else if (is.data.frame(new.y) && nrow(new.y) == 1L) {
      if ("y" %in% names(new.y)) {
        new.data <- cbind(new.data[ , setdiff(names(new.data), "y")],
                          new.y)
      } else {
        new.data <- cbind(new.data, new.y)
      }
    }
  }

  new.data
}

#' \code{Stat*} Objects
#'
#' All \code{stat_*} functions (like \code{stat_bin}) return a layer that
#' contains a \code{Stat*} object (like \code{StatBin}). The \code{Stat*}
#' object is responsible for rendering the data in the plot.
#'
#' Each of the \code{Stat*} objects is a \code{\link[ggplot2]{ggproto}} object, descended
#' from the top-level \code{Stat}, and each implements various methods and
#' fields. To create a new type of Stat object, you typically will want to
#' implement one or more of the following:
#'
#' @name Stats
#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
#' @keywords internal
StatApplyGroup <-
  ggplot2::ggproto("StatApplyGroup", ggplot2::Stat,
                   compute_group = stat_apply_fun,
                   required_aes = c("x", "y")
  )

#'
#' @name Stats
#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @seealso \code{\link[ggplot2]{ggplot2-ggproto}}
#' @keywords internal
StatApplyPanel <-
  ggplot2::ggproto("StatApplyPanel", ggplot2::Stat,
                   compute_panel = stat_apply_fun,
                   required_aes = c("x", "y")
  )


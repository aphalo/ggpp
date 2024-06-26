#' Apply a function to x or y values
#'
#' \code{stat_summary_xy()} and \code{stat_centroid()} are similar to
#' \code{ggplot2::stat_summary()} but summarize both \code{x} and \code{y}
#' values in the same plot layer. Differently to \code{stat_summary()} no
#' grouping based on data \code{values} is done; the grouping respected is that
#' already present based on mappings to aesthetics. This makes it possible to
#' highlight the actual location of the centroid with \code{geom_point()},
#' \code{geom_text()}, and similar geometries. Instead, if we use
#' \code{geom_rug()} they are only a convenience avoiding the need to add two
#' separate layers and flipping one of them using \code{orientation = "y"}.
#'
#' \code{stat_apply_group} applies functions to data.
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
#' @details These four statistics are similar. They differ on whether they
#'   return a single or multiple rows of data per group.
#'
#' @return A data frame with the same variables as the data input, with either a
#'   single or multiple rows, with the values of \code{x} and \code{y} variables
#'   replaced by the values returned by the applied functions, or possibly
#'   filled with \code{NA} if no function was supplied or available by default.
#'   If the applied function returns a named vector, the names are copied into
#'   columns \code{x.names} and/or \code{y.names}. If the summary function
#'   applied returns a one row data frame, it will be column bound keeping
#'   the column names, but overwritting columns x and/or y with y from the
#'   summary data frame. In the names returned by \code{.fun.x} the letter
#'   "y" is replaced by "x". These allows the use of the same functions as in
#'   \code{ggplot2::stat_summary()}.
#'
#'   \describe{
#'   \item{x}{x-value as returned by \code{.fun.x}, with names removed}
#'   \item{y}{y-value as returned by \code{.fun.y}, with names removed}
#'   \item{x.names}{if the x-value returned by \code{.fun.x} is named, these names}
#'   \item{y.names}{if the y-value returned by \code{.fun.y} is named, these names}
#'   \item{xmin, xmax}{values returned by \code{.fun.x} under these names, if present}
#'   \item{ymin, ymax}{values returned by \code{.fun.y} under these names, if present}
#'   \item{<other>}{additional values as returned by \code{.fun.y} under other names}
#'   }
#'
#' @note The applied function(s) must accept as first argument a vector that
#'   matches the variables mapped to \code{x} or \code{y} aesthetics. For
#'   \code{stat_summary_xy()} and \code{stat_centroid()} the function(s) to be
#'   applied is(are) expected to return a vector of length 1 or a data frame
#'   with only one row, as \code{mean_se()}, \code{mean_cl_normal()}
#'   \code{mean_cl_boot()}, \code{mean_sdl()} and \code{median_hilow()} from
#'   'ggplot2' do.
#'
#'   For \code{stat_apply_group} the vectors returned by the
#'   the functions applied to \code{x} and \code{y} must be of exactly the same
#'   length. When only one of \code{.fun.x} or \code{.fun.y} are passed a
#'   function as argument, the other variable in the returned data is filled
#'   with \code{NA_real_}. If other values are desired, they can be set by means
#'   of a user-defined function.
#'
#' @references
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
#'   stat_centroid(geom = "rug", linewidth = 1.5, .fun = median) +
#'   geom_point()
#'
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_centroid(geom = "text", aes(label = category)) +
#'   geom_point()
#'
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_summary_xy(geom = "pointrange",
#'                   .fun.x = mean, .fun.y = mean_se) +
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
#'                    .fun.x = function(x) {rep(22, 5)}, # set x to 22
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
#'   stat_apply_group(.fun.x = function(x) {x},
#'                    .fun.y = cummax)
#'
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_apply_group(.fun.x = cumsum, .fun.y = cumsum)
#'
#' # diff returns a shorter vector by 1 for each group
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_apply_group(.fun.x = function(x) {x[-1L]},
#'                    .fun.y = diff, na.rm = TRUE)
#'
#' # Running summaries
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   geom_point() +
#'   stat_apply_group(.fun.x = function(x) {x},
#'                    .fun.y = runmed, .fun.y.args = list(k = 5))
#'
#' # Rescaling per group
#' ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'   stat_apply_group(.fun.x = function(x) {x},
#'                    .fun.y = function(x) {(x - min(x)) / (max(x) - min(x))})
#'
#' # inspecting the returned data
#' if (requireNamespace("gginnards", quietly = TRUE)) {
#'   library(gginnards)
#'
#'   ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'     stat_centroid(.fun = mean_se, geom = "debug")
#'
#'   ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'     stat_summary_xy(.fun.y = mean_se, geom = "debug")
#'
#'   ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'     stat_apply_group(.fun.y = cumsum, geom = "debug")
#'
#'   ggplot(my.df, aes(x = X, y = Y, colour = category)) +
#'     geom_point() +
#'     stat_apply_group(geom = "debug",
#'                     .fun.x = quantile,
#'                     .fun.x.args = list(probs = my.probs),
#'                     .fun.y = quantile,
#'                    .fun.y.args = list(probs = my.probs))
#' }
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
  if (.fun.y.null) {
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
                          .fun = NULL,
                          .fun.args = list(),
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = FALSE,
                          inherit.aes = TRUE,
                          ...) {
  if (is.null(.fun)) {
    .fun <- ggplot2::mean_se
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
                   compute_group = function(data,
                                            scales,
                                            .fun.x, .fun.x.args, .fun.x.null,
                                            .fun.y, .fun.y.args, .fun.y.null,
                                            single.row,
                                            na.rm = FALSE) {

                     new.data <- data[1, ]
                     unique_value_cols <-
                       sapply(data, function(x) {length(unique(x)) == 1L})

                     if (sum(!unique_value_cols) > 2L) {
                       bad.vars <-
                         setdiff(names(data)[!unique_value_cols], c("x", "y"))
                       warning("Non-unique values in columns: ", bad.vars)
                       new.data[ , bad.vars] <- NA
                     }

                     if (single.row) {
                       # stat_apply_fun_rw(data,
                       #                   new.data,
                       #                   scales,
                       #                   .fun.x, .fun.x.args, .fun.x.null,
                       #                   .fun.y, .fun.y.args, .fun.y.null)
                       if (!.fun.x.null) {
                         args <- c(unname(data["x"]), .fun.x.args)
                         new.x <- do.call(.fun.x, args = args)

                         stopifnot("'new.x' is NULL." = !is.null(new.x))

                         if (is.vector(new.x) && (length(new.x) == 1L)) {
                           if(!is.null(names(new.x))) {
                             new.data[["x.names"]] <- names(new.x)
                             new.x <- unname(new.x)
                           }
                           new.data[["x"]] <- new.x
                         } else if (is.data.frame(new.x) && (nrow(new.x) == 1L)) {
                           # functions like mean_se() return columns y, ymin and ymax.
                           names(new.x) <- gsub("y", "x", names(new.x))
                           if ("x" %in% names(new.x)) {
                             new.data <- cbind(new.data[ , setdiff(names(new.data), "x")],
                                               new.x)
                           } else {
                             new.data <- cbind(new.data, new.x)
                           }
                         } else {
                           warning("Object of class '", class(new.x),
                                   "' with names '", names(new.x),
                                   "' from '.fun.x', is incompatible.")
                         }
                       }

                       if (!.fun.y.null) {
                         args <- c(unname(data["y"]), .fun.y.args)
                         new.y <- do.call(.fun.y, args = args)

                         stopifnot("'new.y' is NULL." = !is.null(new.y))

                         if (is.vector(new.y) && (length(new.y) == 1L)) {
                           if(!is.null(names(new.y))) {
                             new.data[["y.names"]] <- names(new.y)
                             new.y <- unname(new.y)
                           }
                           new.data[["y"]] <- new.y
                         } else if (is.data.frame(new.y) && (nrow(new.y) == 1L)) {
                           if ("y" %in% names(new.y)) {
                             new.data <- cbind(new.data[ , setdiff(names(new.data), "y")],
                                               new.y)
                           } else {
                             new.data <- cbind(new.data, new.y)
                           }
                         } else {
                           warning("Object of class '", class(new.y),
                                   "' with names '", names(new.y),
                                   "' from '.fun.y', is incompatible.")
                         }
                       }

                       new.data
                     } else {
                       # stat_apply_fun_vc(data,
                       #                   new.data,
                       #                   scales,
                       #                   .fun.x, .fun.x.args, .fun.x.null,
                       #                   .fun.y, .fun.y.args, .fun.y.null)
                       if (!.fun.x.null) {
                         args <- c(unname(data["x"]), .fun.x.args)
                         new.x <- do.call(.fun.x, args = args)
                         stopifnot("'new.x' must be 'numeric'" =
                                     is.numeric(new.x))
                         x.names <- names(new.x)
                       }

                       if (!.fun.y.null) {
                         args <- c(unname(data["y"]), .fun.y.args)
                         new.y <- do.call(.fun.y, args = args)
                         stopifnot("'new.y' must be 'numeric'" =
                                     is.numeric(new.y))
                         y.names <- names(new.y)
                       }

                       if (.fun.x.null && .fun.y.null) {
                         new.data[["y"]] <- NA_real_
                         new.data[["y"]] <- NA_real_
                       } else if (.fun.y.null) {
                         new.data <- new.data[rep(1L, length(new.x)), ]
                         new.data[["x"]] <- new.x
                         new.data[["y"]] <- NA_real_
                         if (!is.null(x.names)) {
                           new.data[["x.names"]] <- x.names
                         }
                       } else if (.fun.x.null) {
                         new.data <- new.data[rep(1L, length(new.y)), ]
                         new.data[["x"]] <- NA_real_
                         new.data[["y"]] <- new.y
                         if (!is.null(y.names)) {
                           new.data[["y.names"]] <- y.names
                         }
                       } else if (length(new.x) == length(new.y)) {
                         new.data <- new.data[rep(1L, length(new.y)), ]
                         new.data[["x"]] <- new.x
                         new.data[["y"]] <- new.y
                         if (!is.null(x.names)) {
                           new.data[["x.names"]] <- x.names
                         }
                         if (!is.null(y.names)) {
                           new.data[["y.names"]] <- y.names
                         }
                       } else {
                         warning("x and y summaries of different length!")
                         new.data[["y"]] <- NA_real_
                         new.data[["y"]] <- NA_real_
                       }

                       new.data
                     }
                   },

                   required_aes = c("x", "y"),
                   extra_params = c("na.rm")
  )


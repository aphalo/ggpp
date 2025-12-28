#' Draw functions as curves
#'
#' \code{stat_functions()} computes values from functions and returns new data
#' containing numeric vectors for \code{x} and \code{y}. As function definitions
#' are passed through \code{data} this statistic follows the grammar of graphics in
#' its behaviour.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset. Useful if the function curve is to be
#'   overlaid on other layers.
#' @param n integer Number of points to interpolate along the x axis.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use on this layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes it if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether \code{NA} values should be stripped
#'   before the computation proceeds.
#'
#' @details This statistic can be used to plot values computed by functions. As
#'   it follows the grammar of graphics, grouping and facets are supported. In
#'   this it differs from \code{\link[ggplot2]{geom_function}} which behaves
#'   like a plot annotation.
#'
#'   Aesthetics \code{xmin} and \code{xmax} should be mapped to numeric values
#'   defining the range of the vector to be created and passed as argument to
#'   the function to compute the \code{y} values, and returned as \code{x} in
#'   data. \code{n} is the length of this \code{x} vector.
#'
#' @section Computed variables: Data frame with \code{n} rows or a multiple of
#'   this, one for each
#'   row in \code{data}. \describe{
#'   \item{x}{numeric vector}
#'   \item{y}{numeric vactor}
#'   \item{idx}{integer vector, with values corresponding to rows in the input
#'       \code{data}, i.e., for each function}
#'   }
#'   As shown in one example below \code{\link[gginnards]{geom_debug}} can be
#'   used to print the computed values returned by any statistic. The output
#'   shown includes also values mapped to aesthetics.
#'
#' @return A plot layer instance.
#'
#' @export
#'
#' @examples
#'
#' # one function
#'
#' df1 <- data.frame(min = 0, max = pi, fun = I(list(sin)))
#'
#' ggplot(df1, aes(xmin = min, xmax = max, y = fun)) +
#'   stat_functions()
#'
#' ggplot(df1, aes(xmin = min, xmax = max, y = fun)) +
#'   stat_functions(geom = "point", n = 20)
#'
#' # two functions
#'
#' df2 <- data.frame(min = -pi, max = pi,
#'                   fun = I(list(sin, cos)), name = c("sin", "cos"))
#'
#' # each function must be in a separate group for correct plotting of lines
#'
#' ggplot(df2, aes(xmin = min, xmax = max, y = fun, group = after_stat(idx))) +
#'   stat_functions()
#'
#' ggplot(df2, aes(xmin = min, xmax = max, y = fun, colour = name)) +
#'   stat_functions()
#'
#' ggplot(df2, aes(xmin = min, xmax = max, y = fun)) +
#'   stat_functions() +
#'   facet_grid(~ name)
#'
#' # two curves with same function
#'
#' df3 <- data.frame(min = c(-pi, 0),
#'                   max = c(0,pi),
#'                   fun = I(list(sin, sin)),
#'                   name = c("negative", "positive"))
#'
#' ggplot(df3, aes(xmin = min, xmax = max, y = fun, colour = name)) +
#'   stat_functions()
#'
#' # We use geom_debug_group() to see the computed values
#'
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#' if (gginnards.installed) {
#'   library(gginnards)
#'
#'   ggplot(df1, aes(xmin = min, xmax = max, y = fun)) +
#'     stat_functions(geom = "debug_group")
#' }
#'
stat_functions <- function(mapping = NULL,
                           data = NULL,
                           n = 101,
                           geom = "line",
                           position = "identity",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {

  ggplot2::layer(
    stat = StatFunctions,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  n = n,
                  ...)
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFunctions <-
  ggplot2::ggproto("StatFunctions", ggplot2::Stat,

                   compute_group = function(data,
                                            scales,
                                            n = 101) {
                     z <- data.frame()

                     for (i in seq_along(data$xmin)) {
                       temp <- data.frame(x = seq(from = data$xmin[i],
                                              to = data$xmax[i],
                                              length.out = n),
                                          idx = i)

                       if (is.list(data[["y"]]) && is.function(data[["y"]][[i]])) {
                         temp$y <- data[["y"]][[i]](temp$x)
                       } else {
                         temp$y <- NA_real_
                       }

                       cols2copy <- setdiff(colnames(data),
                                            c(colnames(temp), "xmin", "xmax"))

                       temp <- cbind(temp, data[rep(i, n) , cols2copy])

                       z <- rbind(z, temp)
                     }

                     z
                   },

                   default_aes = ggplot2::aes(x = ggplot2::after_stat(x)),
                   required_aes = c("xmin", "xmax", "y")
  )


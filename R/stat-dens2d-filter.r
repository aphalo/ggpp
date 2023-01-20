#' @title Filter observations by local 2D density
#'
#' @description \code{stat_dens2d_filter} Filters-out/filters-in observations in
#'   regions of a plot panel with high density of observations, based on the
#'   values mapped to both \code{x} and \code{y} aesthetics.
#'   \code{stat_dens2d_filter_g} does the filtering by group instead of by
#'   panel. This second stat is useful for highlighting observations, while the
#'   first one tends to be most useful when the aim is to prevent clashes among
#'   text labels. If there is no mapping to \code{label} in \code{data}, the
#'   mapping is silently set to \code{rownames(data)}.
#'
#' @details The local density of observations in 2D (\emph{x} and \emph{y}) is
#'   computed with function \code{\link[MASS]{kde2d}} and used to select
#'   observations, passing to the geom a subset of the rows in its \code{data}
#'   input. The default is to select observations in sparse regions of the plot,
#'   but the selection can be inverted so that only observations in the densest
#'   regions are returned. Specific observations can be protected from being
#'   deselected and "kept" by passing a suitable argument to \code{keep.these}.
#'   Logical and integer vectors work as indexes to rows in \code{data}, while a
#'   character vector values are compared to the character values mapped to the
#'   \code{label} aesthetic. A function passed as argument to keep.these will
#'   receive as argument the values in the variable mapped to \code{label} and
#'   should return a character, logical or numeric vector as described above. If
#'   no variable has been mapped to \code{label}, row names are used in its
#'   place.
#'
#'   How many rows are retained in addition to those in \code{keep.these} is
#'   controlled with arguments passed to \code{keep.number} and
#'   \code{keep.fraction}. \code{keep.number} sets the maximum number of
#'   observations selected, whenever \code{keep.fraction} results in fewer
#'   observations selected, it is obeyed.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data.
#' @param keep.fraction numeric [0..1]. The fraction of the observations (or
#'   rows) in \code{data} to be retained.
#' @param keep.number integer Set the maximum number of observations to retain,
#'   effective only if obeying \code{keep.fraction} would result in a larger
#'   number.
#' @param keep.sparse logical If \code{TRUE}, the default, observations from the
#'   more sparse regions are retained, if \code{FALSE} those from the densest
#'   regions.
#' @param keep.these character vector, integer vector, logical vector or
#'   function that takes the variable mapped to the \code{label} aesthetic as
#'   first argument and returns a character vector or a logical vector. These
#'   rows from \code{data} are selected irrespective of the local density.
#' @param invert.selection logical If \code{TRUE}, the complement of the
#'   selected rows are returned.
#' @param h vector of bandwidths for x and y directions. Defaults to normal
#'   reference bandwidth (see bandwidth.nrd). A scalar value will be taken to
#'   apply to both directions.
#' @param n Number of grid points in each direction. Can be scalar or a
#'   length-2 integer vector
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
#'
#' @return A plot layer instance. Using as output \code{data} a subset of the
#'   rows in input \code{data} retained based on a 2D-density-based filtering
#'   criterion.
#'
#' @seealso \code{\link{stat_dens2d_labels}} and \code{\link[MASS]{kde2d}} used
#'   internally. Parameters \code{n}, \code{h} in these statistics correspond to
#'   the parameters with the same name in this imported function. Limits are set
#'   to the limits of the plot scales.
#'
#' @family statistics returning a subset of data
#'
#' @examples
#'
#' random_string <-
#'   function(len = 6) {
#'     paste(sample(letters, len, replace = TRUE), collapse = "")
#'   }
#'
#' # Make random data.
#' set.seed(1001)
#' d <- tibble::tibble(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   group = rep(c("A", "B"), c(50, 50)),
#'   lab = replicate(100, { random_string() })
#' )
#'
#' # filter (and here highlight) 1/10 observations in sparsest regions
#' ggplot(data = d, aes(x, y)) +
#'   geom_point() +
#'   stat_dens2d_filter(colour = "red")
#'
#' # filter observations not in the sparsest regions
#' ggplot(data = d, aes(x, y)) +
#'   geom_point() +
#'   stat_dens2d_filter(colour = "blue", invert.selection = TRUE)
#'
#' # filter observations in dense regions of the plot
#' ggplot(data = d, aes(x, y)) +
#'   geom_point() +
#'   stat_dens2d_filter(colour = "blue", keep.sparse = FALSE)
#'
#' # filter 1/2 the observations
#' ggplot(data = d, aes(x, y)) +
#'   geom_point() +
#'   stat_dens2d_filter(colour = "red", keep.fraction = 0.5)
#'
#' # filter 1/2 the observations but cap their number to maximum 12 observations
#' ggplot(data = d, aes(x, y)) +
#'   geom_point() +
#'   stat_dens2d_filter(colour = "red",
#'                      keep.fraction = 0.5,
#'                      keep.number = 12)
#'
#' # density filtering done jointly across groups
#' ggplot(data = d, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_dens2d_filter(shape = 1, size = 3, keep.fraction = 1/4)
#'
#' # density filtering done independently for each group
#' ggplot(data = d, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_dens2d_filter_g(shape = 1, size = 3, keep.fraction = 1/4)
#'
#' # density filtering done jointly across groups by overriding grouping
#' ggplot(data = d, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_dens2d_filter_g(colour = "black",
#'                        shape = 1, size = 3, keep.fraction = 1/4)
#'
#' # label observations
#' ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'   geom_point() +
#'   stat_dens2d_filter(geom = "text")
#'
#' ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'   geom_point() +
#'   stat_dens2d_filter(geom = "text",
#'                      keep.these = function(x) {grepl("^u", x)})
#'
#' ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'   geom_point() +
#'   stat_dens2d_filter(geom = "text",
#'                      keep.these = function(x) {grepl("^u", x)})
#'
#' ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'   geom_point() +
#'   stat_dens2d_filter(geom = "text",
#'                      keep.these = 1:30)
#'
#' # repulsive labels with ggrepel::geom_text_repel()
#' ggrepel.installed <- requireNamespace("ggrepel", quietly = TRUE)
#' if (ggrepel.installed) {
#'   library(ggrepel)
#'
#'   ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'     geom_point() +
#'     stat_dens2d_filter(geom = "text_repel")
#'
#'   ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'     geom_point() +
#'     stat_dens2d_filter(geom = "text_repel",
#'                        keep.these = function(x) {grepl("^u", x)})
#' }
#'
#' @export
#'
stat_dens2d_filter <-
  function(mapping = NULL, data = NULL,
           geom = "point", position = "identity",
           keep.fraction = 0.10,
           keep.number = Inf,
           keep.sparse = TRUE,
           keep.these = FALSE,
           invert.selection = FALSE,
           na.rm = TRUE, show.legend = FALSE,
           inherit.aes = TRUE,
           h = NULL,
           n = NULL,
           ...) {
    ggplot2::layer(
      stat = StatDens2dFilter, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    keep.fraction = keep.fraction,
                    keep.number = keep.number,
                    keep.sparse = keep.sparse,
                    keep.these = keep.these,
                    invert.selection = invert.selection,
                    h = h,
                    n = n,
                    ...)
    )
  }

#' @rdname stat_dens2d_filter
#'
#' @export
#'
stat_dens2d_filter_g <-
  function(mapping = NULL, data = NULL,
           geom = "point", position = "identity",
           keep.fraction = 0.10,
           keep.number = Inf,
           keep.sparse = TRUE,
           keep.these = FALSE,
           invert.selection = FALSE,
           na.rm = TRUE, show.legend = FALSE,
           inherit.aes = TRUE,
           h = NULL,
           n = NULL,
           ...) {
    ggplot2::layer(
      stat = StatDens2dFilterG, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    keep.fraction = keep.fraction,
                    keep.number = keep.number,
                    keep.sparse = keep.sparse,
                    keep.these = keep.these,
                    invert.selection = invert.selection,
                    h = h,
                    n = n,
                    ...)
    )
  }

dens2d_flt_compute_fun <-
  function(data,
           scales,
           keep.fraction,
           keep.number,
           keep.sparse,
           keep.these,
           invert.selection,
           h,
           n) {

    if (is.na(keep.fraction) || keep.fraction < 0 || keep.fraction > 1) {
      stop("Out of range or missing value for 'keep.fraction': ", keep.fraction)
    }
    if (is.na(keep.number) || keep.number < 0) {
      stop("Out of range or missing value for 'keep.number': ", keep.number)
    }

    force(data)
    if (!exists("label", data)) {
#      message("Mapping 'rownames(data)' to missing 'label' aesthetic")
      data[["label"]] <- rownames(data)
    }

    if (length(keep.these)) {
      if (is.function(keep.these)) {
        keep.these <- keep.these(data$label) # character or logical vector
      }
      if (is.character(keep.these)) {
        keep.these <- data$label %in% keep.these # logical vector
      }
      if (is.numeric(keep.these)) {
        temp <- logical(nrow(data))
        temp[keep.these] <- TRUE
        keep.these <- temp
      }
      if (anyNA(keep.these)) {
        warning("Discarding 'NA's in keep.these")
        keep.these <- ifelse(is.na(keep.these),
                            FALSE,
                            keep.these)
      }
    }
    if (nrow(data) * keep.fraction > keep.number) {
      keep.fraction <- keep.number / nrow(data)
    }

    if (keep.fraction == 1) {
      keep <- TRUE
    } else if (keep.fraction == 0) {
      keep <- FALSE
    } else {

      if (is.null(h)) {
        h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
      }

      if (is.null(n)) {
        n <- trunc(sqrt(nrow(data))) * 8L
      }

      kk <-  MASS::kde2d(
        data$x, data$y, h = h, n = n,
        lims = c(scales$x$dimension(), scales$y$dimension()))

      dimnames(kk$z) <- list(kk$x, kk$y)

      # Identify points that are in the low density regions of the plot.
      kx <- cut(data$x, kk$x, labels = FALSE, include.lowest = TRUE)
      ky <- cut(data$y, kk$y, labels = FALSE, include.lowest = TRUE)
      kz <- sapply(seq_along(kx), function(i) kk$z[kx[i], ky[i]])

      if (keep.sparse) {
        keep <- kz < stats::quantile(kz, keep.fraction, names = FALSE)
      } else {
        keep <- kz >= stats::quantile(kz, 1 - keep.fraction, names = FALSE)
      }
    }
    if (invert.selection){
      data[!(keep | keep.these), ]
    } else {
      data[keep | keep.these, ]
    }
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDens2dFilter <-
  ggplot2::ggproto(
    "StatDens2dFilter",
    ggplot2::Stat,
    compute_panel =
      dens2d_flt_compute_fun,
    required_aes = c("x", "y")
  )

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDens2dFilterG <-
  ggplot2::ggproto(
    "StatDens2dFilterG",
    ggplot2::Stat,
    compute_group =
      dens2d_flt_compute_fun,
    required_aes = c("x", "y")
  )

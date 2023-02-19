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
#'   Computation of density and of the default bandwidth require at least
#'   two observations with different values. If data do not fulfill this
#'   condition, they are kept only if \code{keep.fraction = 1}. This is correct
#'   behavior for a single observation, but can be surprising in the case of
#'   multiple observations.
#'
#'   Parameters \code{keep.these} and \code{exclude.these} make it possible to
#'   force inclusion or exclusion of observations after the density is computed.
#'   In case of conflict, \code{exclude.these} overrides \code{keep.these}.
#'
#' @note Which points are kept and which not depends on how dense a grid is used
#'   and how flexible the density surface estimate is. This depends on the
#'   values passed as arguments to parameters \code{n}, \code{bw} and
#'   \code{kernel}. It is also important to be aware that both
#'   \code{geom_text()} and \code{geom_text_repel()} can avoid overplotting by
#'   discarding labels at the plot rendering stage, i.e., what is plotted may
#'   differ from what is returned by this statistic.
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
#' @param keep.these,exclude.these character vector, integer vector, logical
#'   vector or function that takes one or more variables in data selected by
#'   \code{these.target}. Negative integers behave as in R's extraction methods.
#'   The rows from \code{data} indicated by \code{keep.these} and
#'   \code{exclude.these} are kept or excluded irrespective of the local
#'   density.
#' @param these.target character, numeric or logical selecting one or more
#'   column(s) of \code{data}. If \code{TRUE} the whole \code{data} object is
#'   passed.
#' @param pool.along character, one of \code{"none"} or \code{"x"},
#'   indicating if selection should be done pooling the observations along the
#'   \emph{x} aesthetic, or separately on either side of \code{xintercept}.
#' @param xintercept,yintercept numeric The split points for the data filtering.
#' @param invert.selection logical If \code{TRUE}, the complement of the
#'   selected rows are returned.
#' @param h vector of bandwidths for x and y directions. Defaults to normal
#'   reference bandwidth (see bandwidth.nrd). A scalar value will be taken to
#'   apply to both directions.
#' @param n Number of grid points in each direction. Can be scalar or a
#'   length-2 integer vector
#' @param return.density logical vector of lenght 1. If \code{TRUE} add columns
#'   \code{"density"} and \code{"keep.obs"} to the returned data frame.
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
#' # looking under the hood with gginnards::geom_debug()
#' gginnards.installed <- requireNamespace("ggrepel", quietly = TRUE)
#' if (gginnards.installed) {
#'   library(gginnards)
#'
#'   ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'     stat_dens2d_filter(geom = "debug")
#'
#'   ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'     geom_point() +
#'     stat_dens2d_filter(geom = "debug", return.density = TRUE)
#' }
#'
#' @export
#'
stat_dens2d_filter <-
  function(mapping = NULL, data = NULL,
           geom = "point", position = "identity",
           ...,
           keep.fraction = 0.10,
           keep.number = Inf,
           keep.sparse = TRUE,
           keep.these = FALSE,
           exclude.these = FALSE,
           these.target = "label",
           pool.along = c("xy", "x", "y", "none"),
           xintercept = 0,
           yintercept = 0,
           invert.selection = FALSE,
           na.rm = TRUE, show.legend = FALSE,
           inherit.aes = TRUE,
           h = NULL,
           n = NULL,
           return.density = FALSE) {

    pool.along <- match.arg(pool.along)

    if (any(is.na(keep.fraction) | keep.fraction < 0 | keep.fraction > 1)) {
      stop("Out of range or missing value for 'keep.fraction': ", keep.fraction)
    }
    if (any(is.na(keep.number) | keep.number < 0)) {
      stop("Out of range or missing value for 'keep.number': ", keep.number)
    }
    max.expected.length <- c(none = 4L, x = 2L, y = 2L, xy = 1L)[pool.along]
    if (length(keep.fraction) > max.expected.length) {
      if (max.expected.length == 4L) {
        stop("Length of 'keep.fraction' should not exceed 4")
      } else {
        warning("'keep.fraction' is too long, did you forget to set 'pool.along'?")
      }
    }
    if (length(keep.number) > max.expected.length) {
      if (max.expected.length == 4L) {
        stop("Length of 'keep.number' should not exceed 4")
      } else {
        warning("'keep.number' is too long, did you forget to set 'pool.along'?")
      }
    }

    ggplot2::layer(
      stat = StatDens2dFilter, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    keep.fraction = keep.fraction,
                    keep.number = keep.number,
                    keep.sparse = keep.sparse,
                    keep.these = keep.these,
                    exclude.these = exclude.these,
                    these.target = these.target,
                    pool.along = pool.along,
                    xintercept = xintercept,
                    yintercept = yintercept,
                    invert.selection = invert.selection,
                    h = h,
                    n = n,
                    return.density = return.density,
                    ...)
    )
  }

#' @rdname stat_dens2d_filter
#'
#' @export
#'
stat_dens2d_filter_g <-
  function(mapping = NULL, data = NULL,
           geom = "point",
           position = "identity",
           ...,
           keep.fraction = 0.10,
           keep.number = Inf,
           keep.sparse = TRUE,
           keep.these = FALSE,
           exclude.these = FALSE,
           these.target = "label",
           pool.along = c("xy", "x", "y", "none"),
           xintercept = 0,
           yintercept = 0,
           invert.selection = FALSE,
           na.rm = TRUE, show.legend = FALSE,
           inherit.aes = TRUE,
           h = NULL,
           n = NULL,
           return.density = FALSE) {

    pool.along <- match.arg(pool.along)

    if (is.na(keep.fraction) || keep.fraction < 0 || keep.fraction > 1) {
      stop("Out of range or missing value for 'keep.fraction': ", keep.fraction)
    }
    if (is.na(keep.number) || keep.number < 0) {
      stop("Out of range or missing value for 'keep.number': ", keep.number)
    }
    max.expected.length <- c(none = 4L, x = 2L, y = 2L, xy = 1L)[pool.along]
    if (length(keep.fraction) > max.expected.length) {
      if (max.expected.length == 4L) {
        stop("Length of 'keep.fraction' should not exceed 4")
      } else {
        warning("'keep.fraction' is too long, did you forget to set 'pool.along'?")
      }
    }
    if (length(keep.number) > max.expected.length) {
      if (max.expected.length == 4L) {
        stop("Length of 'keep.number' should not exceed 4")
      } else {
        warning("'keep.number' is too long, did you forget to set 'pool.along'?")
      }
    }

    ggplot2::layer(
      stat = StatDens2dFilterG, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    keep.fraction = keep.fraction,
                    keep.number = keep.number,
                    keep.sparse = keep.sparse,
                    keep.these = keep.these,
                    exclude.these = exclude.these,
                    these.target = these.target,
                    pool.along = pool.along,
                    xintercept = xintercept,
                    yintercept = yintercept,
                    invert.selection = invert.selection,
                    h = h,
                    n = n,
                    return.density = return.density,
                    ...)
    )
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
#      dens2d_flt_compute_fun,
# see below for duplicated code to ensure 'covr' sees it
  function(data,
           scales,
           keep.fraction,
           keep.number,
           keep.sparse,
           keep.these,
           exclude.these,
           these.target,
           pool.along,
           xintercept,
           yintercept,
           invert.selection,
           h,
           n,
           return.density) {

    force(data)

    keep.these <- these2logical(these = keep.these,
                                data = data,
                                these.target = these.target)

    exclude.these <- these2logical(these = exclude.these,
                                   data = data,
                                   these.target = these.target)

    # discard redundant splits
    if (pool.along != "xy") {
      if (pool.along == "y" &&
          !(xintercept < max(data[["x"]]) &&
            xintercept > min(data[["x"]]))) {
        pool.along <- "xy"
      } else if (pool.along == "x" &&
                 !(yintercept < max(data[["y"]]) &&
                   yintercept > min(data[["y"]]))) {
        pool.along <- "xy"
      } else if (pool.along == "none") {
        if (!(xintercept < max(data[["x"]]) &&
              xintercept > min(data[["x"]])) &&
            !(yintercept < max(data[["y"]]) &&
              yintercept > min(data[["y"]]))) {
          pool.along <- "xy"
        } else if (!(xintercept < max(data[["x"]]) &&
                     xintercept > min(data[["x"]]))) {
          pool.along <- "x"
        } else if (!(yintercept < max(data[["y"]]) &&
                     yintercept > min(data[["y"]]))) {
          pool.along <- "y"
        }
      }
    }

    # make list of logical vectors
    if (pool.along == "y") {
      selectors <-list(q12 = data[["x"]] <= xintercept,
                       q34 = data[["x"]] > xintercept)
      if (length(keep.fraction) != 2L) {
        keep.fraction <- rep_len(keep.fraction, length.out = 2)
      }
      if (length(keep.number) != 2L) {
        if (length(keep.number) == 1L) {
          keep.number <- keep.number %/% 2
        }
        keep.number <- rep_len(keep.number, length.out = 2)
      }
      num.rows <- sapply(selectors, sum) # selectors are logical
    } else if (pool.along == "x") {
      selectors <-list(q23 = data[["y"]] <= yintercept,
                       q41 = data[["y"]] > yintercept)
      if (length(keep.fraction) != 2L) {
        keep.fraction <- rep_len(keep.fraction, length.out = 2)
      }
      if (length(keep.number) != 2L) {
        if (length(keep.number) == 1L) {
          keep.number <- keep.number %/% 2
        }
        keep.number <- rep_len(keep.number, length.out = 2)
      }
      num.rows <- sapply(selectors, sum) # selectors are logical
    } else if (pool.along == "none") {
      selectors <-list(q1 = data[["y"]] >= yintercept & data[["x"]] >= xintercept,
                       q2 = data[["y"]] < yintercept & data[["x"]] >= xintercept,
                       q3 = data[["y"]] < yintercept & data[["x"]] < xintercept,
                       q4 = data[["y"]] > yintercept & data[["x"]] < xintercept)
      if (length(keep.fraction) != 4L) {
        keep.fraction <- rep_len(keep.fraction, length.out = 4)
      }
      if (length(keep.number) != 4L) {
        if (length(keep.number) == 1L) {
          keep.number <- keep.number %/% 4
        }
        keep.number <- rep_len(keep.number, length.out = 4)
      }
      num.rows <- sapply(selectors, sum) # selectors are logical
    } else {
      keep.fraction <- keep.fraction[[1]] # can be a vector or a list
      keep.number <- keep.number[[1]]
      num.rows <- nrow(data)
      selectors <- list(all = rep.int(TRUE, times = num.rows))
    }

    # vectorized
    too.large.frac <- num.rows * keep.fraction > keep.number
    keep.fraction[too.large.frac] <-
      keep.number[too.large.frac] / num.rows[too.large.frac]

    # estimate 2D density
    # data with fewer than 2 rows needs to be treated as a special case as
    # density cannot be estimated
    if (length(unique(data$x)) >= 2L &&
        length(unique(data$y)) >= 2L) {
      if (is.null(h)) {
        h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
      }
      if (is.null(n)) {
        n <- trunc(sqrt(nrow(data))) * 8L
      }
      kk <-  MASS::kde2d(
        data[["x"]], data[["y"]], h = h, n = n,
        lims = c(scales$x$dimension(), scales$y$dimension()))
      dimnames(kk[["z"]]) <- list(kk[["x"]], kk[["y"]])

      # compute 2D density at each observation's coordinates
      kx <- cut(data$x, kk$x, labels = FALSE, include.lowest = TRUE)
      ky <- cut(data$y, kk$y, labels = FALSE, include.lowest = TRUE)
      kz <- sapply(seq_along(kx), function(i) kk$z[kx[i], ky[i]])
    } else {
      if (nrow(data) > 1L) {
        message("Density not computed, too few distinct values in 'x' and/or 'y'")
      }
      kz <- rep_len(1, nrow(data))
    }
    # we construct one logical vector by adding observations/label to be kept
    # we may have a list of 1, 2, or 4 logical vectors
    keep <- logical(nrow(data))
    for (i in seq_along(selectors)) {
      if (keep.fraction[i] == 1) {
        keep[ selectors[[i]] ] <- TRUE
      } else if (keep.fraction[i] != 0  && length(selectors[[i]]) >= 2L) {
        if (keep.sparse) {
          keep[ selectors[[i]] ] <-
            kz[ selectors[[i]] ] < stats::quantile(kz[ selectors[[i]] ],
                                                   keep.fraction[i], names = FALSE)
        } else {
          keep[ selectors[[i]] ] <-
            kz[ selectors[[i]] ] >= stats::quantile(kz[ selectors[[i]] ],
                                                    1 - keep.fraction[i], names = FALSE)
        }
      }
    }
    keep <- keep | keep.these & !exclude.these

    if (invert.selection) {
      keep <- !keep
    }

    if (return.density) {
      data[["keep.obs"]] <- keep
      data[["density"]] <- kz
    }

    data[keep, ]

  },
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
      #      dens2d_flt_compute_fun,
      # see above for duplicated code to ensure 'covr' sees it
      function(data,
               scales,
               keep.fraction,
               keep.number,
               keep.sparse,
               keep.these,
               exclude.these,
               these.target,
               pool.along,
               xintercept,
               yintercept,
               invert.selection,
               h,
               n,
               return.density) {

        force(data)

        keep.these <- these2logical(these = keep.these,
                                    data = data,
                                    these.target = these.target)

        exclude.these <- these2logical(these = exclude.these,
                                       data = data,
                                       these.target = these.target)

        # discard redundant splits
        if (pool.along != "xy") {
          if (pool.along == "y" &&
              !(xintercept < max(data[["x"]]) &&
                xintercept > min(data[["x"]]))) {
            pool.along <- "xy"
          } else if (pool.along == "x" &&
                     !(yintercept < max(data[["y"]]) &&
                       yintercept > min(data[["y"]]))) {
            pool.along <- "xy"
          } else if (pool.along == "none") {
            if (!(xintercept < max(data[["x"]]) &&
                  xintercept > min(data[["x"]])) &&
                !(yintercept < max(data[["y"]]) &&
                  yintercept > min(data[["y"]]))) {
              pool.along <- "xy"
            } else if (!(xintercept < max(data[["x"]]) &&
                         xintercept > min(data[["x"]]))) {
              pool.along <- "x"
            } else if (!(yintercept < max(data[["y"]]) &&
                         yintercept > min(data[["y"]]))) {
              pool.along <- "y"
            }
          }
        }

        # make list of logical vectors
        if (pool.along == "y") {
          selectors <-list(q12 = data[["x"]] <= xintercept,
                           q34 = data[["x"]] > xintercept)
          if (length(keep.fraction) != 2L) {
            keep.fraction <- rep_len(keep.fraction, length.out = 2)
          }
          if (length(keep.number) != 2L) {
            if (length(keep.number) == 1L) {
              keep.number <- keep.number %/% 2
            }
            keep.number <- rep_len(keep.number, length.out = 2)
          }
          num.rows <- sapply(selectors, sum) # selectors are logical
        } else if (pool.along == "x") {
          selectors <-list(q23 = data[["y"]] <= yintercept,
                           q41 = data[["y"]] > yintercept)
          if (length(keep.fraction) != 2L) {
            keep.fraction <- rep_len(keep.fraction, length.out = 2)
          }
          if (length(keep.number) != 2L) {
            if (length(keep.number) == 1L) {
              keep.number <- keep.number %/% 2
            }
            keep.number <- rep_len(keep.number, length.out = 2)
          }
          num.rows <- sapply(selectors, sum) # selectors are logical
        } else if (pool.along == "none") {
          selectors <-list(q1 = data[["y"]] >= yintercept & data[["x"]] >= xintercept,
                           q2 = data[["y"]] < yintercept & data[["x"]] >= xintercept,
                           q3 = data[["y"]] < yintercept & data[["x"]] < xintercept,
                           q4 = data[["y"]] > yintercept & data[["x"]] < xintercept)
          if (length(keep.fraction) != 4L) {
            keep.fraction <- rep_len(keep.fraction, length.out = 4)
          }
          if (length(keep.number) != 4L) {
            if (length(keep.number) == 1L) {
              keep.number <- keep.number %/% 4
            }
            keep.number <- rep_len(keep.number, length.out = 4)
          }
          num.rows <- sapply(selectors, sum) # selectors are logical
        } else {
          keep.fraction <- keep.fraction[[1]] # can be a vector or a list
          keep.number <- keep.number[[1]]
          num.rows <- nrow(data)
          selectors <- list(all = rep.int(TRUE, times = num.rows))
        }

        # vectorized
        too.large.frac <- num.rows * keep.fraction > keep.number
        keep.fraction[too.large.frac] <-
          keep.number[too.large.frac] / num.rows[too.large.frac]

        # estimate 2D density
        # data with fewer than 2 rows is as a special case as density() fails
        if (length(unique(data$x)) >= 2L &&
            length(unique(data$y)) >= 2L) {
          if (is.null(h)) {
            h <- c(MASS::bandwidth.nrd(data$x), MASS::bandwidth.nrd(data$y))
          }
          if (is.null(n)) {
            n <- trunc(sqrt(nrow(data))) * 8L
          }
          kk <-  MASS::kde2d(
            data[["x"]], data[["y"]], h = h, n = n,
            lims = c(scales$x$dimension(), scales$y$dimension()))
          dimnames(kk[["z"]]) <- list(kk[["x"]], kk[["y"]])

          # compute 2D density at each observation's coordinates
          kx <- cut(data$x, kk$x, labels = FALSE, include.lowest = TRUE)
          ky <- cut(data$y, kk$y, labels = FALSE, include.lowest = TRUE)
          kz <- sapply(seq_along(kx), function(i) kk$z[kx[i], ky[i]])
        } else {
          if (nrow(data) > 1L) {
            message("Density not computed, too few distinct values in 'x' and/or 'y'")
          }
          kz <- rep_len(1, nrow(data))
        }

        # we construct one logical vector by adding observations/label to be kept
        # we may have a list of 1, 2, or 4 logical vectors
        keep <- logical(nrow(data))
        for (i in seq_along(selectors)) {
          if (keep.fraction[i] == 1) {
            keep[ selectors[[i]] ] <- TRUE
          } else if (keep.fraction[i] != 0 && length(selectors[[i]]) >= 2L) {
            if (keep.sparse) {
              keep[ selectors[[i]] ] <-
                kz[ selectors[[i]] ] < stats::quantile(kz[ selectors[[i]] ],
                                                       keep.fraction[i], names = FALSE)
            } else {
              keep[ selectors[[i]] ] <-
                kz[ selectors[[i]] ] >= stats::quantile(kz[ selectors[[i]] ],
                                                        1 - keep.fraction[i], names = FALSE)
            }
          }
        }
        keep <- keep | keep.these & !exclude.these

        if (invert.selection){
          keep <- !keep
        }

        if (return.density) {
          data[["keep.obs"]] <- keep
          data[["density"]] <- kz
        }

        data[keep, ]

      },
    required_aes = c("x", "y")
  )


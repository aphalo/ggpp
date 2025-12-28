#' @title Filter observations by local 1D density
#'
#' @description \code{stat_dens1d_filter} Filters-out/filters-in observations in
#'   regions of a plot panel with high density of observations, based on the
#'   values mapped to one of \code{x} and \code{y} aesthetics.
#'   \code{stat_dens1d_filter_g} does the same filtering by group instead of by
#'   panel. This second stat is useful for highlighting observations, while the
#'   first one tends to be most useful when the aim is to prevent clashes among
#'   text labels. By default the data are handled all together, but it is also
#'   possible to control labeling separately in each tail.
#'
#' @details The 1D density of observations of \emph{x} or \emph{y} is computed
#'   with function \code{\link[stats]{density}} and used to select observations,
#'   passing to the geom a subset of the rows in its \code{data} input. The
#'   default is to select observations in sparse regions of the plot, but the
#'   selection can be inverted so that only observations in the densest regions
#'   are returned. Specific observations can be protected from being deselected
#'   and "kept" by passing a suitable argument to \code{keep.these}. Logical and
#'   integer vectors work as indexes to rows in \code{data}, while a values in a
#'   character vector are compared to the character values mapped to the
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
#'   observations selected, it is obeyed. If `xintercept` is a finite value
#'   within the \emph{x} range of the data and \code{pool.along}
#'   is passed \code{"none"} the data as are split into two groups
#'   and \code{keep.number} and \code{keep.fraction} are applied separately to
#'   each tail with density still computed jointly from all observations. If the
#'   length of \code{keep.number} and \code{keep.fraction} is one, this value
#'   is used for both tails, if their length is two, the first value is use
#'   for the left tail and the second value for the right tail.
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
#' @note Which points are kept and which not depends on how dense and flexible
#'   is the density curve estimate. This depends on the values passed as
#'   arguments to parameters \code{n}, \code{bw} and \code{kernel}. It is
#'   also important to be aware that both \code{geom_text()} and
#'   \code{geom_text_repel()} can avoid over plotting by discarding labels at
#'   the plot rendering stage, i.e., what is plotted may differ from what is
#'   returned by this statistic.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override
#'   the plot defaults.
#' @param geom The geometric object to use display the data.
#' @param keep.fraction numeric vector of length 1 or 2 [0..1]. The fraction of
#'   the observations (or rows) in \code{data} to be retained.
#' @param keep.number integer vector of length 1 or 2. Set the maximum number of
#'   observations to retain, effective only if obeying \code{keep.fraction}
#'   would result in a larger number.
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
#' @param xintercept numeric The split point for the data filtering. If
#'   \code{NA} the data are not split.
#' @param invert.selection logical If \code{TRUE}, the complement of the
#'   selected rows are returned.
#' @param bw numeric or character The smoothing bandwidth to be used. If
#'   numeric, the standard deviation of the smoothing kernel. If character, a
#'   rule to choose the bandwidth, as listed in \code{\link[stats]{bw.nrd}}.
#' @param adjust numeric A multiplicative bandwidth adjustment. This makes it
#'   possible to adjust the bandwidth while still using the a bandwidth
#'   estimator through an argument passed to \code{bw}. The larger the value
#'   passed to \code{adjust} the stronger the smoothing, hence decreasing
#'   sensitivity to local changes in density.
#' @param kernel character See \code{\link{density}} for details.
#' @param n numeric Number of equally spaced points at which the density is to
#'   be estimated for applying the cut point. See \code{\link{density}} for
#'   details.
#' @param return.density logical vector of lenght 1. If \code{TRUE} add columns
#'   \code{"density"} and \code{"keep.obs"} to the returned data frame.
#' @param orientation	character The aesthetic along which density is computed.
#'   Given explicitly by setting orientation to either \code{"x"} or \code{"y"}.
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
#' @param na.rm	a logical value indicating whether \code{NA} values should be
#'   stripped before the computation proceeds.
#'
#' @return A plot layer instance. Using as output \code{data} a subset of the
#'   rows in input \code{data} retained based on a 1D filtering criterion.
#'
#' @seealso \code{\link[stats]{density}} used internally.
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
#' d$xg <- d$x
#' d$xg[51:100] <- d$xg[51:100] + 1
#'
#' # highlight the 1/10 of observations in sparsest regions of the plot
#' ggplot(data = d, aes(x, y)) +
#'   geom_point() +
#'   geom_rug(sides = "b") +
#'   stat_dens1d_filter(colour = "red") +
#'   stat_dens1d_filter(geom = "rug", colour = "red", sides = "b")
#'
#' # highlight the 1/4 of observations in densest regions of the plot
#' ggplot(data = d, aes(x, y)) +
#'   geom_point() +
#'   geom_rug(sides = "b") +
#'   stat_dens1d_filter(colour = "blue",
#'                      keep.fraction = 1/4, keep.sparse = FALSE) +
#'   stat_dens1d_filter(geom = "rug", colour = "blue",
#'                      keep.fraction = 1/4, keep.sparse = FALSE,
#'                      sides = "b")
#'
#' # switching axes
#' ggplot(data = d, aes(x, y)) +
#'   geom_point() +
#'   geom_rug(sides = "l") +
#'   stat_dens1d_filter(colour = "red", orientation = "y") +
#'   stat_dens1d_filter(geom = "rug", colour = "red", orientation = "y",
#'                      sides = "l")
#'
#' # highlight 1/10 plus 1/10 observations in high and low density regions
#' ggplot(data = d, aes(x, y)) +
#'   geom_point() +
#'   geom_rug(sides = "b") +
#'   stat_dens1d_filter(colour = "red") +
#'   stat_dens1d_filter(geom = "rug", colour = "red", sides = "b") +
#'   stat_dens1d_filter(colour = "blue", keep.sparse = FALSE) +
#'   stat_dens1d_filter(geom = "rug",
#'                      colour = "blue", keep.sparse = FALSE, sides = "b")
#'
#' # selecting the 1/10 observations in sparsest regions and their complement
#' ggplot(data = d, aes(x, y)) +
#'   stat_dens1d_filter(colour = "red") +
#'   stat_dens1d_filter(geom = "rug", colour = "red", sides = "b") +
#'   stat_dens1d_filter(colour = "blue", invert.selection = TRUE) +
#'   stat_dens1d_filter(geom = "rug",
#'                      colour = "blue", invert.selection = TRUE, sides = "b")
#'
#' # density filtering done jointly across groups
#' ggplot(data = d, aes(xg, y, colour = group)) +
#'   geom_point() +
#'   geom_rug(sides = "b", colour = "black") +
#'   stat_dens1d_filter(shape = 1, size = 3, keep.fraction = 1/4, adjust = 2)
#'
#' # density filtering done independently for each group
#' ggplot(data = d, aes(xg, y, colour = group)) +
#'   geom_point() +
#'   geom_rug(sides = "b") +
#'   stat_dens1d_filter_g(shape = 1, size = 3, keep.fraction = 1/4, adjust = 2)
#'
#' # density filtering done jointly across groups by overriding grouping
#' ggplot(data = d, aes(xg, y, colour = group)) +
#'   geom_point() +
#'   geom_rug(sides = "b") +
#'   stat_dens1d_filter_g(colour = "black",
#'                        shape = 1, size = 3, keep.fraction = 1/4, adjust = 2)
#'
#' # label observations
#' ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'   geom_point() +
#'   stat_dens1d_filter(geom = "text", hjust = "outward")
#'
#' # looking under the hood with gginnards::geom_debug_group()
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#' if (gginnards.installed) {
#'   library(gginnards)
#'
#'   ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'     stat_dens1d_filter(geom = "debug_group")
#' }
#'
#' if (gginnards.installed) {
#'   ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'     stat_dens1d_filter(geom = "debug_group", return.density = TRUE)
#' }
#' @export
#'
stat_dens1d_filter <-
  function(mapping = NULL,
           data = NULL,
           geom = "point",
           position = "identity",
           ...,
           keep.fraction = 0.10,
           keep.number = Inf,
           keep.sparse = TRUE,
           keep.these = FALSE,
           exclude.these = FALSE,
           these.target = "label",
           pool.along = c("x", "none"),
           xintercept = 0,
           invert.selection = FALSE,
           bw = "SJ",
           kernel = "gaussian",
           adjust = 1,
           n = 512,
           return.density = FALSE,
           orientation = c("x", "y"),
           na.rm = TRUE,
           show.legend = FALSE,
           inherit.aes = TRUE) {

    pool.along <- rlang::arg_match(pool.along)
    orientation <- rlang::arg_match(orientation)

    if (any(is.na(keep.fraction) | keep.fraction < 0 | keep.fraction > 1)) {
      stop("Out of range or missing value for 'keep.fraction': ", keep.fraction)
    }
    if (any(is.na(keep.number) | keep.number < 0)) {
      stop("Out of range or missing value for 'keep.number': ", keep.number)
    }

    ggplot2::layer(
      stat = StatDens1dFilter, data = data, mapping = mapping, geom = geom,
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
                    invert.selection = invert.selection,
                    bw = bw,
                    adjust = adjust,
                    kernel = kernel,
                    n = n,
                    return.density = return.density,
                    orientation = orientation,
                    ...)
    )
  }

#' @rdname stat_dens1d_filter
#'
#' @export
#'
stat_dens1d_filter_g <-
  function(mapping = NULL, data = NULL,
           geom = "point", position = "identity",
           keep.fraction = 0.10,
           keep.number = Inf,
           keep.sparse = TRUE,
           keep.these = FALSE,
           exclude.these = FALSE,
           these.target = "label",
           pool.along = c("x", "none"),
           xintercept = 0,
           invert.selection = FALSE,
           na.rm = TRUE, show.legend = FALSE,
           inherit.aes = TRUE,
           bw = "SJ",
           adjust = 1,
           kernel = "gaussian",
           n = 512,
           return.density = FALSE,
           orientation = c("x", "y"),
           ...) {

    pool.along <- rlang::arg_match(pool.along)
    orientation <- rlang::arg_match(orientation)

    if (any(is.na(keep.fraction) | keep.fraction < 0 | keep.fraction > 1)) {
      stop("Out of range or missing value for 'keep.fraction': ", keep.fraction)
    }
    if (any(is.na(keep.number) | keep.number < 0)) {
      stop("Out of range or missing value for 'keep.number': ", keep.number)
    }

    ggplot2::layer(
      stat = StatDens1dFilterG, data = data, mapping = mapping, geom = geom,
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
                    invert.selection = invert.selection,
                    bw = bw,
                    kernel = kernel,
                    adjust = adjust,
                    n = n,
                    return.density = return.density,
                    orientation = orientation,
                    ...)
    )
  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDens1dFilter <-
  ggplot2::ggproto(
    "StatDens1dFilter",
    ggplot2::Stat,
    compute_panel =
#      dens1d_flt_compute_fun,
# code duplicated below to ensure it is all seen by 'covr'
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
           invert.selection,
           bw,
           kernel,
           adjust,
           n,
           orientation,
           return.density) {

    force(data)

    keep.these <- these2logical(these = keep.these,
                                data = data,
                                these.target = these.target)

    exclude.these <- these2logical(these = exclude.these,
                                   data = data,
                                   these.target = these.target)

    # discard redundant splits and make list of logical vectors
    if (pool.along != "x" &&
        xintercept < max(data[[orientation]]) &&
        xintercept > min(data[[orientation]])) {

      selectors <-list(low.tail = data[[orientation]] <= xintercept,
                       high.tail = data[[orientation]] > xintercept)
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

    # density on a grid
    # data with fewer than 2 rows is as a special case as density() fails
    if (length(unique(data[[orientation]])) >= 2L) {
      dens <-
        stats::density(data[[orientation]],
                       bw = bw, kernel = kernel, adjust = adjust, n = n,
                       from = scales[[orientation]]$dimension()[1],
                       to = scales[[orientation]]$dimension()[2])

      # estimate density at each observations coordinates
      fdens <- stats::splinefun(dens$x, dens$y) # y contains estimate of density
      dens <- fdens(data[[orientation]])
    } else {
      if (nrow(data) > 1L) {
        message("Density not computed, too few distinct values in '", orientation, "'")
      }
      dens <- rep_len(1, nrow(data))
    }

    # we construct one logical vector by adding observations/label to be kept
    # we may have a list of 1 or 2 logical vectors
    keep <- logical(nrow(data))
    for (i in seq_along(selectors)) {
      if (keep.fraction[i] == 1 ||
          (length(selectors[[i]]) < 2L && keep.fraction[i] >= 0.5)) {
        keep[ selectors[[i]] ] <- TRUE
      } else if (keep.fraction[i] != 0 && length(selectors[[i]]) >= 2L) {
        if (keep.sparse) {
          keep[ selectors[[i]] ] <-
            dens[ selectors[[i]] ] < stats::quantile(dens[ selectors[[i]] ],
                                                     keep.fraction[i],
                                                     names = FALSE,
                                                     type = 8)
        } else {
          keep[ selectors[[i]] ] <-
            dens[ selectors[[i]] ] >= stats::quantile(dens[ selectors[[i]] ],
                                                      1 - keep.fraction[i],
                                                      names = FALSE,
                                                      type = 8)
        }
      }
    }
    keep <- (keep | keep.these) & !exclude.these

    if (invert.selection){
      keep <- !keep
    }

    if (return.density) {
      data[["keep.obs"]] <- keep
      data[["density"]] <- dens
    }

    data[keep, ]
  },

    required_aes = "x|y"
  )

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDens1dFilterG <-
  ggplot2::ggproto(
    "StatDens1dFilterG",
    ggplot2::Stat,
    compute_group =
      #      dens1d_flt_compute_fun,
      # code duplicated above to ensure it is all seen by 'covr'
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
               invert.selection,
               bw,
               kernel,
               adjust,
               n,
               orientation,
               return.density) {

        force(data)

        keep.these <- these2logical(these = keep.these,
                                    data = data,
                                    these.target = these.target)

        exclude.these <- these2logical(these = exclude.these,
                                       data = data,
                                       these.target = these.target)

        # discard redundant splits and make list of logical vectors
        if (pool.along != "x" &&
            xintercept < max(data[[orientation]]) &&
            xintercept > min(data[[orientation]])) {

          selectors <-list(low.tail = data[[orientation]] <= xintercept,
                           high.tail = data[[orientation]] > xintercept)
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

        # density on a grid
        # data with fewer than 2 rows is as a special case as density() fails
        if (length(unique(data[[orientation]])) >= 2L) {
          dens <-
            stats::density(data[[orientation]],
                           bw = bw, kernel = kernel, adjust = adjust, n = n,
                           from = scales[[orientation]]$dimension()[1],
                           to = scales[[orientation]]$dimension()[2])

          # estimate density at each observations coordinates
          fdens <- stats::splinefun(dens$x, dens$y) # y contains estimate of density
          dens <- fdens(data[[orientation]])
        } else {
          if (nrow(data) > 1L) {
            message("Density not computed, too few distinct values in '", orientation, "'")
          }
          dens <- rep_len(1, nrow(data))
        }

        # we construct one logical vector by adding observations/label to be kept
        # we may have a list of 1 or 2 logical vectors
        keep <- logical(nrow(data))
        for (i in seq_along(selectors)) {
          if (keep.fraction[i] == 1) {
            keep[ selectors[[i]] ] <- TRUE
          } else if (keep.fraction[i] != 0 && length(selectors[[i]]) >= 2L) {
            if (keep.sparse) {
              keep[ selectors[[i]] ] <-
                 dens[ selectors[[i]] ] < stats::quantile(dens[ selectors[[i]] ],
                                                         keep.fraction[i],
                                                         names = FALSE,
                                                         type = 8)
            } else {
              keep[ selectors[[i]] ] <-
                dens[ selectors[[i]] ] >= stats::quantile(dens[ selectors[[i]] ],
                                                          1 - keep.fraction[i],
                                                          names = FALSE,
                                                          type = 8)
            }
          }
        }
        keep <- (keep | keep.these) & !exclude.these

        if (invert.selection){
          keep <- !keep
        }

        if (return.density) {
          data[["keep.obs"]] <- keep
          data[["density"]] <- dens
        }

        data[keep, ]
      },

    required_aes = "x|y"
  )

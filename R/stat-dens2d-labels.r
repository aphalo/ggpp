#' @title Replace labels in data based on 2D density
#'
#' @description \code{stat_dens2d_labels()} Sets values mapped to the
#'   \code{label} aesthetic to \code{""} or a user provided character string
#'   based on the local density in regions of a plot panel. Its main use is
#'   together with repulsive geoms from package \code{\link[ggrepel]{ggrepel}}.
#'   If there is no mapping to \code{label} in \code{data}, the mapping is set
#'   to \code{rownames(data)}, with a message.
#'
#' @details \code{stat_dens2d_labels()} is designed to work together with
#'   geometries from package 'ggrepel'. To avoid text labels being plotted over
#'   unlabelled points all the rows in data need to be retained but
#'   labels replaced with the empty character string, \code{""}. Function
#'   \code{\link{stat_dens2d_filter}} cannot be used with the repulsive geoms
#'   from 'ggrepel' because it drops observations.
#'
#'   \code{stat_dens2d_labels()} can be useful also in other situations, as the
#'   substitution character string can be set by the user by passing an argument
#'   to \code{label.fill}. If this argument is \code{NULL} the unselected rows
#'   are filtered out identically as by \code{stat_dens2d_filter}.
#'
#'   The local density of observations in 2D (\emph{x} and \emph{y}) is computed
#'   with function \code{\link[MASS]{kde2d}} and used to select observations,
#'   passing to the geom all the rows in its \code{data} input but with with the
#'   text of labels replaced in those "not kept". The default is to select
#'   observations in sparse regions of the plot, but the selection can be
#'   inverted so that only observations in the densest regions are returned.
#'   Specific observations can be protected from having the label replaced by
#'   passing a suitable argument to \code{keep.these}. Logical and integer
#'   vectors function as indexes to rows in \code{data}, while a character
#'   vector is compared to values in the variable mapped to the \code{label}
#'   aesthetic. A function passed as argument to \code{keep.these} will receive
#'   as its first argument the values in the variable mapped to \code{label} and
#'   should return a character, logical or numeric vector as described above.
#'
#'   How many labels are retained intact in addition to those in
#'   \code{keep.these} is controlled with arguments passed to \code{keep.number}
#'   and \code{keep.fraction}. \code{keep.number} sets the maximum number of
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
#' @param n Number of grid points in each direction. Can be scalar or a length-2
#'   integer vector
#' @param label.fill character vector of length 1, a function or \code{NULL}.
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
#' @return A plot layer instance. Using as output \code{data} the input
#'   \code{data} after value substitution based on a 2D the filtering criterion.
#'
#' @seealso \code{\link{stat_dens2d_filter}} and \code{\link[MASS]{kde2d}} used
#'   internally. Parameters \code{n}, \code{h} in this statistic correspond to
#'   the parameters with the same name in this imported function. Limits are set
#'   to the limits of the plot scales.
#'
#' @family statistics returning a subset of data
#'
#' @export
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
#' # using defaults
#' ggplot(data = d, aes(x, y, label = lab)) +
#'   geom_point() +
#'   stat_dens2d_labels()
#'
#' ggplot(data = d, aes(x, y, label = lab)) +
#'   geom_point() +
#'   stat_dens2d_labels(keep.these = "zoujdg")
#'
#' ggplot(data = d, aes(x, y, label = lab)) +
#'   geom_point() +
#'   stat_dens2d_labels(keep.these = function(x) {grepl("^z", x)})
#'
#' ggplot(data = d, aes(x, y, label = lab)) +
#'   geom_point() +
#'   stat_dens2d_labels(geom = "text_s",
#'                      position = position_nudge_center(x = 0.1, y = 0.1,
#'                                                       center_x = mean,
#'                                                       center_y = mean),
#'                      vjust = "outward_mean", hjust = "outward_mean") +
#'   expand_limits(x = c(-4, 4.5))
#'
#' ggrepel.installed <- requireNamespace("ggrepel", quietly = TRUE)
#' if (ggrepel.installed) {
#'   library(ggrepel)
#'
#'   ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'     geom_point() +
#'     stat_dens2d_labels(geom = "text_repel")
#'
#'   ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'     geom_point() +
#'     stat_dens2d_labels(geom = "text_repel", label.fill = NA)
#'
#' # we keep labels starting with "a" across the whole plot, but all in sparse
#' # regions. To achieve this we pass as argument to label.fill a fucntion
#' # instead of a character string.
#'   label.fun <- function(x) {ifelse(grepl("^a", x), x, "")}
#'   ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'     geom_point() +
#'     stat_dens2d_labels(geom = "text_repel", label.fill = label.fun)
#' }
#' # Using geom_debug() we can see that all 100 rows in \code{d} are
#' # returned. But only those labelled in the previous example still contain
#' # the original labels.
#'
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#' if (gginnards.installed) {
#'   library(gginnards)
#'
#'   ggplot(data = d, aes(x, y, label = lab)) +
#'     geom_point() +
#'     stat_dens2d_labels(geom = "debug")
#'
#'   ggplot(data = d, aes(x, y, label = lab)) +
#'     geom_point() +
#'     stat_dens2d_labels(geom = "debug", return.density = TRUE)
#'
#'   ggplot(data = d, aes(x, y, label = lab)) +
#'     geom_point() +
#'     stat_dens2d_labels(geom = "debug", label.fill = NULL)
#'
#'   ggplot(data = d, aes(x, y, label = lab)) +
#'     geom_point() +
#'     stat_dens2d_labels(geom = "debug", label.fill = FALSE, return.density = TRUE)
#'
#'   ggplot(data = d, aes(x, y, label = lab)) +
#'     geom_point() +
#'     stat_dens2d_labels(geom = "debug", label.fill = NULL, return.density = TRUE)
#'
#'   ggplot(data = d, aes(x, y)) +
#'     geom_point() +
#'     stat_dens2d_labels(geom = "debug")
#' }
#'
stat_dens2d_labels <-
  function(mapping = NULL,
           data = NULL,
           geom = "text",
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
           h = NULL,
           n = NULL,
           label.fill = "",
           return.density = FALSE,
           na.rm = TRUE,
           show.legend = FALSE,
           inherit.aes = TRUE) {

    pool.along <- match.arg(pool.along)

    if (length(label.fill) > 1L) {
      stop("Length for 'label.fill' is not 1: ", label.fill)
    }
    if (is.numeric(label.fill)) {
      stop("'label.fill' should not be a 'numeric' value: ", label.fill)
    }
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
      stat = StatDens2dLabels, data = data, mapping = mapping, geom = geom,
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
                    label.fill = label.fill,
                    return.density = return.density,
                    ...)
    )
  }


#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatDens2dLabels <-
  ggplot2::ggproto(
    "StatDens2dLabels",
    ggplot2::Stat,
    compute_panel =
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
               label.fill,
               return.density) {

        force(data)
        if (!exists("label", data) && !is.null(label.fill)) {
          data[["label"]] <- rownames(data)
        }

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
        # data with fewer than 2 distinct values preventgs density() estimation
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
                                                       keep.fraction[i],
                                                       names = FALSE,
                                                       type = 8)
            } else {
              keep[ selectors[[i]] ] <-
                kz[ selectors[[i]] ] >= stats::quantile(kz[ selectors[[i]] ],
                                                        1 - keep.fraction[i],
                                                        names = FALSE,
                                                        type = 8)
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

        if (is.null(label.fill)) {
          data <- data[keep, ]
        } else if (is.function(label.fill)) {
          data[["label"]][!keep] <- label.fill(data[["label"]][!keep])
        } else if (is.na(label.fill)) {
          # NA_logical_, the default NA, cannot always be assigned to character
          label.fill <- NA_character_
          data[["label"]][!keep] <- label.fill
        } else if (is.character(label.fill)) {
          data[["label"]][!keep] <- label.fill
        } else if (is.logical(label.fill)) {
          if (label.fill) {
            data[["label"]][!keep] <- ""
          } # if FALSE data is not modified
        } else {
          stop("'label.fill' is : ", mode(label.fill),
               " instead of 'character' or 'function'.")
        }
        data
      },

    required_aes = c("x", "y")
  )


# Utils for stats that subset data

#' Convert keep.these argument into logical vector
#'
#' @param keep.these character vector, integer vector, logical vector or
#'   function that takes the variable mapped to the \code{label} aesthetic as
#'   first argument and returns a character vector or a logical vector. These
#'   rows from \code{data} are selected irrespective of the local density.
#' @param data data.frame The plot layer's data set.
#'
#' @keywords internal
#'
these2logical<- function(these,
                         data,
                         these.target = "label") {
  if (length(these)) {
    if (is.character(these) || is.function(these)) {
      if (is.character(these.target) && any(these.target == "label") &&
          !exists("label", where = data, mode = "character", inherits = FALSE)) {
        data$label <- rownames(data)
      }
      if (is.character(these.target)) {
        orig.num.targets <- length(unique(these.target))
        these.target <- intersect(these.target, colnames(data))
        if (length(these.target) == 0L) {
          stop("Variables in 'these.target' not in 'data'")
        } else if (orig.num.targets > length(these.target)) {
          warning("Some variables in 'these.target' not in 'data'")
        }
      }
    }
    if (is.function(these)) {
      these <- these(data[ , these.target, drop = TRUE]) # any vector
    }
    if (is.character(these)) {
      stopifnot(is.character(data[[these.target[1]]]))
      these <- data[[these.target[1]]] %in% these # logical vector
    }
    if (is.numeric(these)) { # positional indices
      temp <- rep_len(FALSE, length.out = nrow(data))
      temp[these] <- TRUE
      these <- temp
    }
    if (is.logical(these)) { # logical indices, if short recycle
      if (length(these) >= 1L && length(these) < nrow(data)) {
        these <- rep_len(these, length.out = nrow(data))
      } else if (length(these) > nrow(data)) {
        stop("Logical vector 'keep.these' or 'exclude.these' longer than data")
      }
    }
    if (anyNA(these)) {
      warning("Discarding 'NA's in 'keep.these' or 'exclude-these'")
      these[is.na(these)] <- FALSE
    }
  } else { # replace NULL and vectors with length zero with FALSE
    these <- rep_len(FALSE, length.out = nrow(data))
  }
  these
}

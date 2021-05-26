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
#'   statistics from package 'ggrepel'. To avoid text labels being plotted over
#'   unlabelled points the corresponding rows in data need to be retained but
#'   labels replaced with the empty character string, \code{""}. This makes
#'   \code{\link{stat_dens2d_filter}} unsuitable for the task. Non-the-less
#'   \code{stat_dens2d_labels()} could be useful in some other cases, as the
#'   substitution character string can be set by the user.
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
#' @param invert.selection logical If \code{TRUE}, the complement of the
#'   selected rows are returned.
#' @param h vector of bandwidths for x and y directions. Defaults to normal
#'   reference bandwidth (see bandwidth.nrd). A scalar value will be taken to
#'   apply to both directions.
#' @param n Number of grid points in each direction. Can be scalar or a length-2
#'   integer vector
#' @param label.fill character vector of length 1 or a function.
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
#' @seealso \code{\link[MASS]{kde2d}} used internally.
#'
#' @family statistics returning a subset of data
#'
#' @export
#'
#' @examples
#'
#' library(ggrepel)
#' library(gginnards)
#'
#' random_string <- function(len = 6) {
#' paste(sample(letters, len, replace = TRUE), collapse = "")
#' }
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
#' # Using geom_debug() we can see that all 100 rows in \code{d} are
#' # returned. But only those labelled in the previous example still contain
#' # the original labels.
#' ggplot(data = d, aes(x, y, label = lab)) +
#'   geom_point() +
#'   stat_dens2d_labels(geom = "debug")
#'
#' ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'   geom_point() +
#'   stat_dens2d_labels(geom = "text_repel")
#'
#' ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'   geom_point() +
#'   stat_dens2d_labels(geom = "text_repel", label.fill = NA)
#'
#' # we keep labels starting with "a" across the whole plot, but all in sparse
#' # regions. To achieve this we pass as argument to label.fill a fucntion
#' # instead of a character string.
#' label.fun <- function(x) {ifelse(grepl("^a", x), x, "")}
#' ggplot(data = d, aes(x, y, label = lab, colour = group)) +
#'   geom_point() +
#'   stat_dens2d_labels(geom = "text_repel", label.fill = label.fun)
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
           invert.selection = FALSE,
           h = NULL,
           n = NULL,
           label.fill = "",
           na.rm = TRUE,
           show.legend = FALSE,
           inherit.aes = TRUE) {
    ggplot2::layer(
      stat = StatDens2dLabels, data = data, mapping = mapping, geom = geom,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm,
                    keep.fraction = keep.fraction,
                    keep.number = keep.number,
                    keep.sparse = keep.sparse,
                    invert.selection = invert.selection,
                    h = h,
                    n = n,
                    label.fill = label.fill,
                    ...)
    )
  }

dens2d_labs_compute_fun <-
  function(data,
           scales,
           keep.fraction,
           keep.number,
           keep.sparse,
           invert.selection,
           h,
           n,
           label.fill) {

    if (length(label.fill) != 1L) {
      stop("Length for 'label.fill' is not 1: ", label.fill)
    }

    if (is.na(keep.fraction) || keep.fraction < 0 || keep.fraction > 1) {
      stop("Out of range or missing value for 'keep.fraction': ", keep.fraction)
    }
    if (is.na(keep.number) || keep.number < 0) {
      stop("Out of range or missing value for 'keep.number': ", keep.number)
    }

    force(data)
    if (nrow(data) * keep.fraction > keep.number) {
      keep.fraction <- keep.number / nrow(data)
    }

    if (!exists("label", data)) {
      message("Mapping missing 'label' aesthetic to 'rownames(data)'.")
      data[["label"]] <- rownames(data)
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

    if (is.function(label.fill)) {
      if (invert.selection){
        data[["label"]] <- ifelse(!keep,
                                  data[["label"]],
                                  label.fill(data[["label"]]))
      } else {
        data[["label"]] <- ifelse(keep,
                                  data[["label"]],
                                  label.fill(data[["label"]]))
      }
    } else {
      if (!is.character(label.fill)) {
        if (is.na(label.fill)) {
          # NA_logical_, the default NA, cannot be assigned to character
          label.fill <- NA_character_
        } else {
          stop("'label.fill' is :", mode(label.fill),
               "instead of 'character' or 'function'.")
        }
      }
      if (invert.selection){
        data[["label"]] <- ifelse(!keep,
                                  data[["label"]],
                                  label.fill)
      } else {
        data[["label"]] <- ifelse(keep,
                                  data[["label"]],
                                  label.fill)
      }
    }

    data
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
      dens2d_labs_compute_fun,
    required_aes = c("x", "y")
  )

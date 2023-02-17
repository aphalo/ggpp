#' Convert an R object into a tibble
#'
#' This functions tries to convert any R object into a data.frame object.
#' If \code{x} is already a data.frame, it is returned as is. If it is
#' a list or a vector it is converted by means of \code{as.data.frame()}.
#' If of any other type, a conversion into an object of class \code{xts} is
#' attempted by means of \code{try.xts()} and if successful the \code{xts}
#' object is converted into a data frame with a variable \code{time}
#' containing times as \code{POSIXct} and the remaining data columns with
#' the time series data. In this conversion row names are stripped.
#'
#' @param x An R object
#' @param time.resolution character The time unit to which the returned time
#'   values will be rounded.
#' @param as.numeric logical If TRUE convert time to numeric, expressed as
#'   fractional calendar years.
#' @param col.names character vector
#'
#' @return A \code{tibble::tibble} object, derived from \code{data.frame}.
#'
#' @note This function can be used to easily convert time series data into a
#'   format that can be easily plotted with package \code{ggplot2}.
#'   \code{try_tibble} is another name for \code{try_data_frame} which tracks
#'   the separation and re-naming of \code{data_frame} into
#'   \code{tibble::tibble} in the imported packages.
#'
#' @section Warning!: The time zone was set to "UTC" by try.xts() in the test
#'   cases I used. Setting TZ to "UTC" can cause some trouble as several
#'   frequently used functions have as default the local or system TZ and will
#'   apply a conversion before printing or plotting time data, which in addition
#'   is affected by summer/winter time transitions. This should be taken into
#'   account as even for yearly data when conversion is to POSIXct a day (1st of
#'   January) will be set, but then shifted some hours if printed on a TZ
#'   different from "UTC". I recommend reading the documentation of package
#'   \code{\link[lubridate]{lubridate-package}} where the irregularities of time
#'   data and the difficulties they cause are very well described. In many cases
#'   when working with time series with yearly observations it is best to work
#'   with numeric values for years.
#'
#' @export
#'
#' @examples
#' class(lynx)
#' try_tibble(lynx)
#' try_tibble(lynx, as.numeric = TRUE)
#' try_tibble(lynx, "year")
#' class(austres)
#' try_tibble(austres)
#' try_tibble(austres, as.numeric = TRUE)
#' try_tibble(austres, "quarter")
#' class(cars)
#' try_tibble(cars)
#'
try_data_frame <- function(x,
                           time.resolution = "month",
                           as.numeric = FALSE,
                           col.names = NULL) {
  if (inherits(x, "data.frame")) {
    return(x)
  }
  if (!xts::xtsible(x) &&
      (is.list(x) || is.factor(x) || is.vector(x) || is.matrix(x))) {
    return(as.data.frame(x))
  }
  if (xts::is.xts(x)) {
    data.xts <- x
  } else {
    stopifnot(xts::xtsible(x))
    data.xts <- xts::as.xts(x)
  }
  times.raw <- zoo::index(data.xts) # because TZ = "UTC"
  if (as.numeric) {
    if (is.numeric(times.raw)) {
      times <- times.raw
    } else if (inherits(times.raw, "yearmon")) {
      times <- as.numeric(times.raw)
    } else if (inherits(times.raw, "yearqtr")) {
      times <- zoo::as.Date(times.raw)
      times <- lubridate::decimal_date(times)
    } else {
      times <- lubridate::decimal_date(times.raw)
    }
    times <- as.double(times) # remove "ts" class attribute
  } else {
    if (lubridate::is.POSIXct(times.raw) || lubridate::is.Date(times.raw)) {
      times <- times.raw
    } else if (inherits(times.raw, "yearmon")) {
      times <- as.POSIXct(format(times.raw, "%Y-%m-01"))
    } else if (inherits(times.raw, "yearqtr")) {
      times <- zoo::as.Date(times.raw)
    } else if (is.numeric(times.raw)) {
      times <- lubridate::date_decimal(times.raw, "UTC") # handles conversion from classes in xts and zoo
      times <- lubridate::round_date(times, unit = time.resolution)
    } else {
      stop("class '", class(times.raw), "' used as index is not supported")
    }
    if (lubridate::tz(times) == "") {
      times <- lubridate::with_tz(times, "UTC")
    }
    if (!is.null(time.resolution) && !is.na(time.resolution)) {
      times <- lubridate::round_date(times, unit = time.resolution)
    }
  }
  if (is.null(names(x))) {
    data.names <- paste(as.character(substitute(x)), sep = "")
  } else {
    data.names <- names(x)
  }

  z <- data.frame(time = times)
  z <- cbind(z, as.data.frame(as.numeric(data.xts)))
  if (length(col.names) == ncol(z)) {
    colnames(z) <- col.names
  } else {
    names(z)[-1] <- data.names
  }
  z
}

#' @export
#'
#' @rdname try_data_frame
try_tibble <- function(x,
                       time.resolution = "month",
                       as.numeric = FALSE,
                       col.names = NULL) {
  z <- try_data_frame(x = x,
                      time.resolution = time.resolution,
                      as.numeric = as.numeric,
                      col.names = col.names)
  rownames(z) <- NULL
  tibble::as_tibble(z)
}

#' Create a new ggplot plot from time series data
#'
#' \code{ggplot()} initializes a ggplot object. It can be used to
#' declare the input spectral object for a graphic and to optionally specify the
#' set of plot aesthetics intended to be common throughout all
#' subsequent layers unless specifically overridden.
#'
#' \code{ggplot()} is typically used to construct a plot
#' incrementally, using the + operator to add layers to the
#' existing ggplot object. This is advantageous in that the
#' code is explicit about which layers are added and the order
#' in which they are added. For complex graphics with multiple
#' layers, initialization with \code{ggplot} is recommended.
#'
#' There are three common ways to invoke \code{ggplot}:
#' \itemize{
#'    \item \code{ggplot(ts, aes(x, y, <other aesthetics>))}
#'    \item \code{ggplot(ts)}
#'   }
#' The first method is recommended if all layers use the same
#' data and the same set of aesthetics, although this method
#' can also be used to add a layer using data from another
#' data frame. See the first example below. The second
#' method specifies the default spectrum object to use for the plot, and the
#' units to be used for y in the plot,
#' but no aesthetics are defined up front. This is useful when
#' one data frame is used predominantly as layers are added,
#' but the aesthetics may vary from one layer to another. The
#' third method specifies the default spectrum object to use for the plot,
#' but no aesthetics are defined up front. This is useful when
#' one spectrum is used predominantly as layers are added,
#' but the aesthetics may vary from one layer to another.
#'
#' @param data Default spectrum dataset to use for plot. If not a spectrum, the
#'   methods used will be those defined in package \code{ggplot2}. See \code{\link[ggplot2]{ggplot}}.
#'   If not specified,
#'   must be suppled in each layer added to the plot.
#' @param mapping Default list of aesthetic mappings to use for plot.
#'   If not specified, in the case of spectral objects, a default mapping will
#'   be used.
#' @param time.resolution character The time unit to which the returned time
#'   values will be rounded.
#' @param as.numeric logical If TRUE convert time to numeric, expressed as
#'   fractional calendar years.
#' @param ... Other arguments passed on to methods. Not currently used.
#' @param environment If an variable defined in the aesthetic mapping is not
#'   found in the data, ggplot will look for it in this environment. It defaults
#'   to using the environment in which \code{ggplot()} is called.
#'
#' @return A \code{"ggplot"} object.
#'
#' @export
#' @examples
#' ggplot(lynx) + geom_line()
#'
#' @note Current implementation does not merge default mapping with user
#' supplied mapping. If user supplies a mapping, it is used as is.
#' To add to the default mapping, aes() can be used by itself to compose
#' the ggplot.
#'
#' @name ggplot
#'
ggplot.ts <-
  function(data, mapping = NULL, ...,
           time.resolution = "day",
           as.numeric = TRUE,
           environment = parent.frame()) {
    data.name <- substitute(data)
    if (!is.name(data.name)) {
      # likely an expression
      data.name <- as.name("y")
    }
    time.name <- as.name("time")
    data.df <- try_tibble(data,
                          time.resolution = time.resolution,
                          as.numeric = as.numeric,
                          col.names = c(time.name, data.name)
                          )
    if (is.null(mapping)) {
      mapping <- ggplot2::aes(x = {{time.name}}, y = {{data.name}})
    }
    ggplot2::ggplot(data = data.df,
                    mapping =  mapping,
                    ... = ...,
                    environment = environment)
  }

#' @rdname ggplot
#'
#' @export
#'
ggplot.xts <- ggplot.ts

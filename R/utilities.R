#'  Check default.colour argument
#'
#' if possible use ink from theme as default colour
#'
#' @param default.colour The value to be checked
#' @param theme The theme from where to fetch a replacement value when
#'   \code{default.colour} is \code{NULL}.
#'
#' @details The replacement value is fetched from the theme only if
#'   'ggplot2' >= 4.0.0 and otherwise \code{"black"} is used. \code{NA}
#'   values are passed through as they can be used to disable rendering
#'   of grob elements.
#'
#' @return A colour definition or NA
#
#' @keywords internal
#'
check_default_colour <-
  function(default.colour, theme = ggplot2::get_theme()) {
    if (is.null(default.colour)) {
      if (utils::packageVersion("ggplot2") >= "4.0.0") {
        ggplot2::calc_element("geom", theme)@ink
      } else {
        "black"
      }
    } else {
      default.colour[[1L]] # trim vectors or lists
    }
  }

# utility functions not exported by 'ggplot2'
# constant copied from geom-.R
.pt <- 72.27 / 25.4

# function copied from ggplot2's utilities.R

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Parse takes a vector of n lines and returns m expressions.
#' See https://github.com/tidyverse/ggplot2/issues/2864 for discussion.
#'
#' parse(text = c("alpha", "", "gamma"))
#' #> expression(alpha, gamma)
#'
#' parse_safe(text = c("alpha", "", "gamma"))
#' #> expression(alpha, NA, gamma)
#'
#' @noRd
parse_safe <- function(text) {
  stopifnot(is.character(text))
  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out
}

# Helpers to convert degrees to radians and vice versa
rad2deg <- function(rad) rad * 180 / pi
deg2rad <- function(deg) deg * pi / 180


# function from ggplot2, needed in annotate() but not exported
compact <- function (x)
{
  null <- vapply(x, is.null, logical(1))
  x[!null]
}

# function from ggplot2 (not current), needed in annotate() but not exported
new_data_frame <- function (x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    rlang::abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0)
      0
    else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n)
      next
    if (lengths[i] != 1) {
      rlang::abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }
  tibble::as_tibble(x)
}

# Used in annotations to ensure printed even when no
# global data
# copied from ggplot2's utilities.r
dummy_data <- function() new_data_frame(list(x = NA), n = 1)

# This function is used to vectorise the following pattern:
#
# obj$name1 <- obj$name1 %||% value
# obj$name2 <- obj$name2 %||% value
#
# and express this pattern as:
#
# replace_null(obj, name1 = value, name2 = value)
replace_null <- function(obj, ..., env = rlang::caller_env()) {
  # Collect dots without evaluating
  dots <- rlang::enexprs(...)
  # Select arguments that are null in `obj`
  nms  <- names(dots)
  nms  <- nms[vapply(obj[nms], is.null, logical(1))]
  # Replace those with the evaluated dots
  obj[nms] <- rlang::inject(list(!!!dots[nms]), env = env)
  obj
}

# From 'ggplot2' 4.0.0
descent_cache <- new.env(parent = emptyenv())
# Important: This function is not vectorized. Do not use to look up multiple
# font descents at once.
font_descent <- function(family = "", face = "plain", size = 12, cex = 1) {
  cur_dev <- names(grDevices::dev.cur())
  if (cur_dev == "null device") {
    cache <- FALSE   # don't cache if no device open
  } else {
    cache <- TRUE
  }
  key <- paste0(cur_dev, ':', family, ':', face, ":", size, ":", cex)
  # we only look up the first result; this function is not vectorized
  key <- key[1]

  descent <- descent_cache[[key]]

  if (is.null(descent)) {
    descent <- convertHeight(grobDescent(textGrob(
      label = "gjpqyQ",
      gp = gg_par(
        fontsize = size,
        cex = cex,
        fontfamily = family,
        fontface = face
      )
    )), 'inches')

    if (cache) {
      descent_cache[[key]] <- descent
    }
  }

  descent
}

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
replace_null <- function(obj, ..., env = caller_env()) {
  # Collect dots without evaluating
  dots <- enexprs(...)
  # Select arguments that are null in `obj`
  nms  <- names(dots)
  nms  <- nms[vapply(obj[nms], is.null, logical(1))]
  # Replace those with the evaluated dots
  obj[nms] <- inject(list(!!!dots[nms]), env = env)
  obj
}

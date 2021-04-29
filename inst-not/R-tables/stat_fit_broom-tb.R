# broom::tidy as tibble -----------------------------------------------------

#' @title Return a tibble within a tibble with fitted parameter estimates.
#'
#' @description \code{stat_fit_tb} fits a model and returns a "tidy" version
#'   of the model's summary, using package 'broom'.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
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
#' @param na.rm	a logical indicating whether NA values should be stripped
#'   before the computation proceeds.
#' @param method character.
#' @param method.args list of arguments to pass to \code{method}.
#' @param tb.type character One of "fit.summary", "fit.anova" or "fit.coefs".
#' @param digits integer indicating the number of significant digits to be used.
#' @param label.x.npc,label.y.npc \code{numeric} with range 0..1 or character.
#'   Coordinates to be used for positioning the output, expressed in "normalized
#'   parent coordinates" or character string. If too short they will be recycled.
#' @param label.x,label.y \code{numeric} Coordinates (in data units) to be used
#'   for absolute positioning of the output. If too short they will be recycled.
#'
#' @section Computed variables: The output of \code{\link[broom]{tidy}} is
#'   returned as a single "cell" in a tibble (i.e. a tibble nested within a
#'   tibble). The returned \code{data} object contains a single row, containing
#'   the
#'   result from a single model fit to all data in a panel. If grouping is
#'   present, it is ignored.
#'
#' @export
#'
stat_fit_tb <- function(mapping = NULL, data = NULL, geom = "table",
                        method = "lm",
                        method.args = list(formula = y ~ x),
                        tb.type = "fit.summary",
                        digits = 3,
                        label.x.npc = "center", label.y.npc = "top",
                        label.x = NULL, label.y = NULL,
                        position = "identity",
                        na.rm = FALSE, show.legend = FALSE,
                        inherit.aes = TRUE,
                        ...) {
  ggplot2::layer(
    stat = StatFitTb, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(method = method,
                  method.args = method.args,
                  tb.type = tb.type,
                  digits = digits,
                  label.x.npc = label.x.npc,
                  label.y.npc = label.y.npc,
                  label.x = label.x,
                  label.y = label.y,
                  na.rm = na.rm,
                  ...)
  )
}

# Defined here to avoid a note in check --as-cran as the imports from 'broom'
# are not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpmisc-ggproto
#'
#' @format NULL
#' @usage NULL
#'
fit_tb_compute_panel_fun <- function(data,
                                     scales,
                                     method,
                                     method.args,
                                     tb.type,
                                     digits,
                                     label.x.npc,
                                     label.y.npc,
                                     label.x,
                                     label.y) {
  force(data)

  if (length(unique(data$x)) < 2) {
    # Not enough data to perform fit
    return(data.frame())
  }

  # support setting of table position per panel
  panel.idx <- as.integer(as.character(data$PANEL[1]))
  if (length(label.x.npc) >= panel.idx) {
    label.x.npc <- label.x.npc[panel.idx]
  } else if (length(label.x.npc) > 0) {
    label.x.npc <- label.x.npc[1]
  }
  if (length(label.y.npc) >= panel.idx) {
    label.y.npc <- label.y.npc[panel.idx]
  } else if (length(label.y.npc) > 0) {
    label.y.npc <- label.y.npc[1]
  }

  if (length(label.x) >= panel.idx) {
    label.x <- label.x[panel.idx]
  } else if (length(label.x) > 0) {
    label.x <- label.x[1]
  }
  if (length(label.y) >= panel.idx) {
    label.y <- label.y[panel.idx]
  } else if (length(label.y) > 0) {
    label.y <- label.y[1]
  }

  method.args <- c(method.args, list(data = quote(data)))
  if (is.character(method)) method <- match.fun(method)
  mf <- do.call(method, method.args)

  if (length(label.x) > 0) {
    x.out <- label.x
    hjust.out <- 0.5
  } else if (length(label.x.npc) > 0) {
    if (is.numeric(label.x.npc)) {
      # if (any(label.x.npc < 0 | label.x.npc > 1)) {
      #   warning("'label.x.npc' argument is numeric but outside range 0..1.")
      # }
      x.out <- scales$x$dimension()[1] + label.x.npc *
        diff(scales$x$dimension())
      hjust.out <- 0.5
    } else if (is.character(label.x.npc)) {
      if (label.x.npc == "right") {
        x.out <- scales$x$dimension()[2]
        hjust.out <- 1
      } else if (label.x.npc %in% c("center", "centre", "middle")) {
        x.out <- mean(scales$x$dimension())
        hjust.out <- 0.5
      } else if (label.x.npc == "left") {
        x.out <- scales$x$dimension()[1]
        hjust.out <- 0
      } else {
        stop("'label.x.npc' argument '", label.x.npc, " unsupported")
      }
    } else {
      stop("'label.x.npc' argument is neither numeric nor character")
    }
  }

  if (length(label.y) > 0) {
    y.out <- label.y
    vjust.out <- 0.5
  } else if (length(label.y.npc) > 0) {
    if (is.numeric(label.y.npc)) {
      # if (any(label.y.npc < 0 | label.y.npc > 1)) {
      #   warning("'label.y.npc' argument is numeric but outside range 0..1.")
      # }
      y.out <- scales$y$dimension()[1] + label.y.npc *
        diff(scales$y$dimension())
      vjust.out <- 0.5
    } else if (is.character(label.y.npc)) {
      if (label.y.npc == "bottom") {
        y.out <- scales$y$dimension()[1]
        vjust.out <- 0
      } else if (label.y.npc %in% c("center", "centre", "middle")) {
        y.out <- mean(scales$y$dimension())
        vjust.out <- 0.5
      } else if (label.y.npc == "top") {
        y.out <- scales$y$dimension()[2]
        vjust.out <- 1
      } else {
        stop("'label.y.npc' argument '", label.y.npc, " unsupported")
      }
    } else {
      stop("'label.y.npc' argument is neither numeric nor character")
    }
  }

  if (tolower(tb.type) %in% c("fit.anova", "anova")) {
    mf_tb <- broom::tidy(stats::anova(mf))
  } else if (tolower(tb.type) %in% c("fit.summary", "summary")) {
    mf_tb <- broom::tidy(mf)
  } else if (tolower(tb.type) %in% c("fit.coefs", "coefs")) {
    mf_tb <- broom::tidy(mf)[c("term", "estimate")]
  }

  num.cols <- sapply(mf_tb, is.numeric)
  mf_tb[num.cols] <- signif(mf_tb[num.cols], digits = digits)

  # we need to enclose the tibble in a list to mannualy nest the table in
  # data.
  tibble::tibble(x = x.out, y = y.out,
                 hjust = hjust.out, vjust = vjust.out,
                 mf_tb = list(mf_tb))
}

#' @rdname ggpmisc-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFitTb <-
  ggplot2::ggproto("StatFitTb", ggplot2::Stat,
                   compute_panel = fit_tb_compute_panel_fun,
                   default_aes =
                     ggplot2::aes(hjust = stat(hjust.., vjust = ..vjust),
                                  label = stat(mf_tb)),
                   required_aes = c("x", "y")
  )


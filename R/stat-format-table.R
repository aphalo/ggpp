#' @title Select and slice a tibble nested in \code{data}
#'
#' @description \code{stat_fmt_tb} selects, reorders and/or renames columns and
#'   or rows of a tibble nested in \code{data}. It can also apply user supplied
#'   functions to data columns. This stat is intended to be used to pre-process
#'   \code{tibble} objects mapped to the \code{label} aesthetic before adding
#'   them to a plot with \code{geom_table}.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
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
#' @param na.rm	a logical indicating whether \code{NA} values should be stripped
#'   before the computation proceeds.
#' @param digits integer indicating the number of significant digits to be
#'   retained in data. Use \code{digits = Inf} to skip.
#' @param tb.vars,tb.rows character or numeric vectors, optionally named, used
#'   to select and/or rename the columns or rows in the table
#'   returned.
#' @param tb.funs named list of functions to be applied to \code{data} columns.
#' @param table.theme NULL, list or function A 'gridExtra' \code{ttheme}
#'   definition, or a constructor for a \code{ttheme} or \code{NULL} for
#'   default.
#' @param table.rownames,table.colnames logical flag to enable or disabling
#'   printing of row names and column names.
#' @param table.hjust numeric Horizontal justification for the core and column
#'   headings of the table.
#' @param parse If \code{TRUE}, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#'
#' @details One or more functions to be applied can be passed in a named list to
#'   parameter `tb.funs`. Functions are matched by name to columns, after column
#'   selection and renaming have been applied.
#'
#' @seealso See \code{\link{geom_table}} for details on how tables respond
#'   to mapped aesthetics and table themes. For details on predefined table
#'   themes see \code{\link{ttheme_gtdefault}}.
#'
#' @section Computed variables: The output of sequentially applying
#'   \code{\link[dplyr]{slice}} with \code{tb.rows} as argument and
#'   \code{\link[dplyr]{select}} with \code{tb.vars} to a list variable
#'   list mapped to \code{label} and containing a single tibble per row
#'   in \code{data}.
#'
#' @return A plot layer instance. Using as output \code{data} a copy of the
#'   input \code{data} in which the data frames mapped to \code{label} have been
#'   modified.
#'
#' @export
#'
#' @examples
#' my.df <-
#'   tibble::tibble(
#'     x = c(1, 2),
#'     y = c(0, 4),
#'     group = c("A", "B"),
#'     tbs = list(a = tibble::tibble(Xa = 1:6, Y = rep(c("x", "y"), 3)),
#'                b = tibble::tibble(Xb = 1:3, Y = "x"))
#'   )
#'
#' ggplot(my.df, aes(x, y, label = tbs)) +
#'   stat_fmt_tb() +
#'   expand_limits(x = c(0,3), y = c(-2, 6))
#'
#' # Hide column names, diplay row names
#' ggplot(my.df, aes(x, y, label = tbs)) +
#'   stat_fmt_tb(table.colnames = FALSE,
#'               table.rownames = TRUE) +
#'   expand_limits(x = c(0,3), y = c(-2, 6))
#'
#' # Use a theme for the table
#' ggplot(my.df, aes(x, y, label = tbs)) +
#'   stat_fmt_tb(table.theme = ttheme_gtlight) +
#'   expand_limits(x = c(0,3), y = c(-2, 6))
#'
#' # selection and renaming by column position
#' ggplot(my.df, aes(x, y, label = tbs)) +
#'   stat_fmt_tb(tb.vars = c(value = 1, group = 2),
#'                tb.rows = 1:3) +
#'   expand_limits(x = c(0,3), y = c(-2, 6))
#'
#' # apply functions to columns
#' ggplot(my.df, aes(x, y, label = tbs)) +
#'   stat_fmt_tb(tb.vars = c(value = 1, group = 2),
#'                tb.rows = 1:3,
#'                tb.funs = list(group = function(x) {sprintf("italic(%s)", x)},
#'                               value = function(x) {ifelse(x > 2, "bold(zz)", x)}),
#'                parse = TRUE) +
#'   expand_limits(x = c(0,3), y = c(-2, 6))
#'
#' # selection, reordering and renaming by column position
#' ggplot(my.df, aes(x, y, label = tbs)) +
#'   stat_fmt_tb(tb.vars = c(group = 2, value = 1),
#'               tb.rows = 1:3) +
#'   expand_limits(x = c(0,3), y = c(-2, 6))
#'
#' # selection and renaming, using partial matching to column name
#' ggplot(my.df, aes(x, y, label = tbs)) +
#'   stat_fmt_tb(tb.vars = c(value = "X", group = "Y"),
#'               tb.rows = 1:3) +
#'   expand_limits(x = c(0,3), y = c(-2, 6))
#'
stat_fmt_tb <- function(mapping = NULL,
                        data = NULL,
                        geom = "table",
                        tb.vars = NULL,
                        tb.rows = NULL,
                        tb.funs = list(),
                        digits = 3,
                        position = "identity",
                        table.theme = NULL,
                        table.rownames = FALSE,
                        table.colnames = TRUE,
                        table.hjust = 0.5,
                        parse = FALSE,
                        na.rm = FALSE,
                        show.legend = FALSE,
                        inherit.aes = TRUE,
                        ...) {

  # replace NULL with default
  if (is.null(table.theme)) {
    table.theme <-
      getOption("ggpmisc.ttheme.default",
                default = ggpp::ttheme_gtdefault)
  }

  ggplot2::layer(
    stat = StatFmtTb, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(tb.vars = tb.vars,
                  tb.rows = tb.rows,
                  tb.funs = tb.funs,
                  digits = digits,
                  table.theme = table.theme,
                  table.rownames = table.rownames,
                  table.colnames = table.colnames,
                  table.hjust = table.hjust,
                  parse = parse,
                  na.rm = na.rm,
                  ...)
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatFmtTb <-
  ggplot2::ggproto("StatFmtTb", ggplot2::Stat,
                   compute_group =   function(data,
                                              scales,
                                              tb.vars = NULL,
                                              tb.rows = NULL,
                                              tb.funs = list(),
                                              digits = 3) {
                     stopifnot(is.list(data$label))

                     for (tb.idx in seq_along(data$label)) {
                       temp_tb <- data$label[tb.idx][[1]]

                       if (!is.data.frame(temp_tb)) {
                         message("Skipping object of class ", class(temp_tb))
                         next()
                       }

                       num.cols <- sapply(temp_tb, is.numeric)
                       temp_tb[num.cols] <-
                         signif(temp_tb[num.cols], digits = digits)

                       if (!is.null(tb.vars)) {
                         if (is.character(tb.vars)) {
                           idxs <- pmatch(tb.vars, colnames(temp_tb))
                           if (length(idxs) < length(tb.vars) || anyNA(idxs)) {
                             warning("Attempt to select nonexistent columns")
                             idxs <- stats::na.omit(idxs)
                             # no renaming possible, as we do not know which name was not matched
                             tb.vars <- unname(tb.vars)
                           }
                         } else {
                           idxs <- unname(tb.vars)
                           if (any(idxs > ncol(temp_tb))) {
                             warning("Attempt to select nonexistent columns")
                             idxs <- idxs[idxs <= ncol(temp_tb)]
                             tb.vars <- tb.vars[idxs]
                           }
                         }
                         # if (length(idxs) < ncol(temp_tb)) {
                         #   message("Dropping column(s) from table.")
                         # }
                         if (length(idxs) < 1L) {
                           message("No matching column(s).")
                           temp_tb <- NULL
                         } else {
                           temp_tb <- temp_tb[ , idxs]
                           if (!is.null(names(tb.vars))) {
                             # support renaming of only some selected columns
                             selector <- names(tb.vars) != ""
                             colnames(temp_tb)[selector] <- names(tb.vars)[selector]
                           }
                         }
                       }

                       if (!is.null(tb.rows) && !is.null(temp_tb)) {
                         if (is.character(tb.rows)) {
                           idxs <- pmatch(tb.rows, rownames(temp_tb))
                           if (length(idxs) < length(tb.rows) || anyNA(idxs)) {
                             warning("Attempt to select nonexistent rows")
                             idxs <- stats::na.omit(idxs)
                             # no renaming possible, as we do not know which name was not matched
                             tb.rows <- unname(tb.rows)
                           }
                         } else {
                           idxs <- unname(tb.rows)
                           if (any(idxs > nrow(temp_tb))) {
                             warning("Attempt to select nonexistent rows")
                             idxs <- idxs[idxs <= nrow(temp_tb)]
                             tb.rows <- tb.rows[idxs]
                           }
                         }
                         # if (length(idxs) < nrow(temp_tb)) {
                         #   message("Dropping row(s) from table.")
                         # }
                         if (length(idxs) < 1L) {
                           warning("No matching row(s).")
                           temp_tb <- NULL
                         } else {
                           temp_tb <- temp_tb[idxs, ]
                           if (!is.null(names(tb.rows))) {
                             # support renaming of only some selected rows
                             selector <- names(tb.rows) != ""
                             colnames(temp_tb)[selector] <- names(tb.rows)[selector]
                           }
                         }
                       }

                       funs2apply <-
                         intersect(names(tb.funs), colnames(temp_tb))

                       for (f in funs2apply) {
                         if (is.character(tb.funs[[f]])) {
                           tb.funs[[f]] <- match.fun(tb.funs[[f]])
                         }
                         if (is.function(tb.funs[[f]])) {
                           temp_tb[[f]] <- tb.funs[[f]](temp_tb[[f]])
                         } else {
                           warning("'tb.funs[[", f, "]]' is not a function or a visible function name", sep = "")
                           temp_tb[[f]] <- NA
                         }
                       }

                       data$label[tb.idx] <- list(temp_tb)

                     }

                     data
                   },

                   required_aes = c("x", "y", "label")
)

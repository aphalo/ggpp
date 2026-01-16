#' Inset tables
#'
#' \code{geom_table} and \code{geom_table_npc} add data frames as table insets
#' to the base ggplot, using syntax similar to that of
#' \code{\link[ggplot2]{geom_text}} and \code{\link{geom_text_s}}. In most
#' respects they behave as any other ggplot geometry: they add a layer
#' containing one or more grobs and grouping and faceting works as usual. The
#' most common use of \code{geom_table} is to add data labels that are whole
#' tables rather than text. \code{\link{geom_table_npc}} is used to add tables
#' as annotations to plots, but contrary to layer function \code{annotate},
#' \code{\link{geom_table_npc}} is data driven and respects grouping and facets,
#' thus plot insets can differ among panels.
#'
#' @details \code{geom_table} and \code{geom_table_npc} expect a list of data frames
#'   (\code{"data.frame"} class or derived) to be mapped to the \code{label}
#'   aesthetic. These geoms work with tibbles or data frames as \code{data} as
#'   they both support \code{list} objects as member variables.
#'
#'   A table is built with function \code{gridExtra::gtable} for each
#'   data frame in the list, and formatted according to a \code{ttheme} (table
#'   theme) list object or \code{ttheme} constructor function passed as argument
#'   to parameter \code{table.theme}. If the value passed as argument to
#'   \code{table.theme} is \code{NULL} the table theme used is that set as
#'   default through R option \code{ggpmisc.ttheme.default} at the time the plot
#'   is rendered or the \code{\link{ttheme_gtdefault}} constructor function if
#'   not set.
#'
#'   If the argument passed to \code{table.theme} or set through R option
#'   \code{ggpmisc.ttheme.default} is a constructor function (passing its name
#'   without parenthesis), the values mapped to \code{size}, \code{colour},
#'   \code{fill}, \code{alpha}, and \code{family} aesthetics will the passed to
#'   this theme constructor for each individual table. In contrast, if a ready
#'   constructed \code{ttheme} stored as a list object is passed as argument
#'   (e.g., by calling the constructor, using constructor name followed by
#'   parenthesis), it is used as is, with mappings to aesthetics \code{colour},
#'   \code{fill}, \code{alpha}, and \code{family} ignored if present. By default
#'   the constructor \code{ttheme_gtdefault} is used and \code{colour} and
#'   \code{fill}, are mapped to \code{NA}. Mapping these aesthetics to \code{NA}
#'   triggers the use the values set in the \code{ttheme}. As the table is built
#'   with function \code{gridExtra::gtable()}, for details, please, consult
#'   \code{\link[gridExtra]{tableGrob}} and \code{\link{ttheme_gtdefault}}.
#'
#'   The character strings in the data frame can be parsed into R expressions so
#'   the inset tables can include maths. With \code{parse = TRUE} parsing is
#'   attempted on each table cell, but failure triggers fall-back to rendering
#'   without parsing, on a cell by cell basis. Thus, a table can contain a
#'   mixture cells and/or headings that require parsing or not (see the
#'   documentation in \link[gridExtra]{gridExtra-package} for details).
#'
#'   The \code{x} and \code{y} aesthetics determine the position of the whole
#'   inset table, similarly to that of a text label, justification is
#'   interpreted as indicating the position of the inset table with respect to
#'   its \emph{horizontal} and \emph{vertical} axes (rows and columns in the
#'   data frame), and \code{angle} is used to rotate the inset table as a whole.
#'   The default for the \code{colour} and \code{fill} aesthetics is to retrieve
#'   them from the table theme, while \code{family} and \code{size} are
#'   retrieved from the \code{geom} element of the ggplot theme.
#'
#'   Of these two geoms only \code{\link{geom_table}} supports the plotting of
#'   connecting segments or arrows when its position has been modified by a
#'   \code{position} function. This is because \code{\link{geom_table_npc}} uses
#'   a coordinate system that is unrelated to data units, scales or data in
#'   other plot layers. In the case of \code{geom_table_npc}, \code{npcx} and
#'   \code{npcy} pseudo-aesthetics determine the position of the inset table. By
#'   default, \code{hjust} and \code{vjust} are set to \code{"inward"} to avoid
#'   clipping at the edge of the plot canvas.
#'
#' @inheritSection geom_text_s Alignment
#'
#' @inheritSection geom_text_s Position functions
#'
#' @inheritSection geom_grob Plot boundaries and clipping
#'
#' @note Complex tables with annotations or different colouring of rows or cells
#'   can be constructed with functions in package 'gridExtra' or in any other
#'   way as long as they can be saved as grid graphical objects and then added
#'   to a ggplot as a new layer with \code{\link{geom_grob}}.
#'
#' @inherit geom_grob note return seealso references
#'
#' @seealso Formatting of tables \code{stat_fmt_table},
#'   \code{\link{ttheme_gtdefault}}, \code{\link{ttheme_set}},
#'   \code{\link[gridExtra]{tableGrob}}.
#'
#' @inheritParams geom_grob
#'
#' @param table.theme NULL, list or function A gridExtra ttheme defintion, or
#'   a constructor for a ttheme or NULL for default. If \code{nULL} the theme
#'   is retrieved from R option \code{} at the time the plot is rendered.
#' @param table.rownames,table.colnames logical flag to enable or disable
#'   printing of row names and column names.
#' @param table.hjust numeric Horizontal justification for the core and column
#'   headings of the table.
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#' @param default.colour,default.color A colour definition to use for elements
#'   not targeted by the colour aesthetic. If \code{NA} the colours in the table
#'   theme are not modified.
#' @param colour.target,color.target A vector of character strings with one or
#'   more of \code{"all"}, \code{"table"}, \code{"table.text"},
#'   \code{"table.rules"}, \code{"segment"} or \code{"none"}.
#' @param default.alpha numeric in [0..1] A transparency value to use for
#'   elements not targeted by the alpha aesthetic. If \code{NA} the alpha
#'   channel of the colour definitions is not modified.
#' @param alpha.target A vector of character strings with one or
#'   more of \code{"all"}, \code{"table"}, \code{"table.text"},
#'   \code{"table.rules"} and \code{"table.canvas"}, \code{"segment"} or
#'   \code{"none"}.
#' @param fontsize.scaling A scaling factor to apply to the \emph{size}
#'   aesthetic retrieved from the theme or mapped, applied to table text.
#'
#' @aesthetics GeomTable
#' @aesthetics GeomTableNpc
#'
#' @references This geometry is inspired on answers to two questions in
#'   Stackoverflow. In contrast to these earlier examples, the current geom
#'   obeys the grammar of graphics, and attempts to be consistent with the
#'   behaviour of 'ggplot2' geometries.
#'   \url{https://stackoverflow.com/questions/12318120/adding-table-within-the-plotting-region-of-a-ggplot-in-r}
#'   \url{https://stackoverflow.com/questions/25554548/adding-sub-tables-on-each-panel-of-a-facet-ggplot-in-r?}
#'
#' @family geometries adding layers with insets
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' theme_set(theme_bw())
#'
#' mtcars |>
#'   group_by(cyl) |>
#'   summarize(wt = mean(wt), mpg = mean(mpg)) |>
#'   ungroup() |>
#'   mutate(wt = sprintf("%.2f", wt),
#'          mpg = sprintf("%.1f", mpg)) -> tb
#'
#' df <- data.frame(x = 5.45, y = 34, tb = I(list(tb)))
#'
#' # using defaults
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df,
#'              aes(x = x, y = y, label = tb))
#'
#' # settings aesthetics to constants
#' ggplot(mtcars,
#'        aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df,
#'              aes(x = x, y = y, label = tb),
#'              color = "red",
#'              fill = "#FFCCCC",
#'              family = "serif", size = 5,
#'              angle = 90, vjust = 0)
#'
#' # passing a theme constructor as argument
#' ggplot(mtcars,
#'        aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df,
#'              aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtstripes) +
#'   theme_classic()
#'
#' # transparency
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df,
#'              aes(x = x, y = y, label = tb),
#'              alpha = 0.5) +
#'   theme_bw()
#'
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df,
#'              aes(x = x, y = y, label = tb),
#'              alpha = 0.5, alpha.target = "table.canvas")
#'
#' df2 <- tibble(x = 5.45,
#'               y = c(34, 29, 24),
#'               x1 = c(2.29, 3.12, 4.00),
#'               y1 = c(26.6, 19.7, 15.1),
#'               cyl = c(4, 6, 8),
#'               tb = list(tb[1, 1:3], tb[2, 1:3], tb[3, 1:3]))
#'
#' # mapped aesthetics
#' ggplot(mtcars,
#'        aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df2,
#'              inherit.aes = TRUE,
#'              mapping = aes(x = x, y = y, label = tb))
#'
#' ggplot(mtcars,
#'        aes(wt, mpg, color = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df2,
#'              inherit.aes = TRUE,
#'              colour.target = "table.rules",
#'              mapping = aes(x = x, y = y, label = tb))
#'
#' # nudging and segments
#' ggplot(mtcars,
#'        aes(wt, mpg, color = factor(cyl))) +
#'   geom_point(show.legend = FALSE) +
#'   geom_table(data = df2,
#'              inherit.aes = TRUE,
#'              mapping = aes(x = x1, y = y1, label = tb),
#'              nudge_x = 0.7, nudge_y = 3,
#'              vjust = 0.5, hjust = 0.5,
#'              arrow = arrow(length = unit(0.5, "lines"))) +
#'   theme_classic()
#'
#' ggplot(mtcars,
#'        aes(wt, mpg, color = factor(cyl))) +
#'   geom_point(show.legend = FALSE) +
#'   geom_table(data = df2,
#'              inherit.aes = TRUE,
#'              mapping = aes(x = x1, y = y1, label = tb),
#'              nudge_x = 0.7, nudge_y = 3,
#'              vjust = 0.5, hjust = 0.5,
#'              arrow = arrow(length = unit(0.5, "lines")),
#'              colour.target = c("table.rules", "segment")) +
#'   theme_classic()
#'
#' # Using native plot coordinates instead of data coordinates
#' dfnpc <- tibble(x = 0.95, y = 0.95, tb = list(tb))
#'
#' ggplot(mtcars,
#'        aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table_npc(data = dfnpc,
#'                  aes(npcx = x, npcy = y, label = tb))
#'
geom_table <- function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       ...,
                       nudge_x = 0,
                       nudge_y = 0,
                       default.colour = NA,
                       default.color = default.colour,
                       colour.target = "table.text",
                       color.target = colour.target,
                       default.alpha = 1,
                       alpha.target = "all",
                       fontsize.scaling = 0.825,
                       add.segments = TRUE,
                       box.padding = 0.25,
                       point.padding = 1e-06,
                       segment.linewidth = 0.5,
                       min.segment.length = 0,
                       arrow = NULL,
                       table.theme = NULL,
                       table.rownames = FALSE,
                       table.colnames = TRUE,
                       table.hjust = 0.5,
                       parse = FALSE,
                       na.rm = FALSE,
                       show.legend = FALSE,
                       inherit.aes = FALSE) {

  colour.target <-
    rlang::arg_match(color.target,
                     values = c("segment", "all", "box", "text", "table",
                                "table.text", "table.rules", "none"),
                     multiple = TRUE)
  alpha.target <-
    rlang::arg_match(alpha.target,
                     values = c("segment", "all", "box", "text", "table",
                                "table.text", "table.rules", "table.canvas",
                                "none"),
                     multiple = TRUE)

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position) && position != "identity") {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }
    # We do not keep the original positions if they will not be used
    position <-
      position_nudge_center(nudge_x, nudge_y,
                            kept.origin = ifelse(add.segments,
                                                 "original", "none"))
  }

  if (is.character(table.hjust)) {
    table.hjust <- switch(table.hjust,
                          left = 0,
                          middle = 0.5,
                          center = 0.5,
                          right = 1,
                          0.5)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTable,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      default.colour = default.color,
      colour.target = colour.target,
      default.alpha = default.alpha,
      alpha.target = alpha.target,
      fontsize.scaling = fontsize.scaling,
      add.segments = add.segments,
      box.padding = box.padding,
      point.padding = point.padding,
      segment.linewidth = segment.linewidth,
      min.segment.length = min.segment.length,
      arrow = arrow,
      table.theme = table.theme,
      table.rownames = table.rownames,
      table.colnames = table.colnames,
      table.hjust = table.hjust,
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTable <-
  ggplot2::ggproto("GeomTable", ggplot2::Geom,
                   required_aes = c("x", "y", "label"),
                   default_aes = ggplot2::aes(
                     colour = NA,
                     fill = NA,
                     family = "",
                     size = 3.2,
                     angle = 0,
                     hjust = "inward",
                     vjust = "inward",
                     alpha = NA,
                     fontface = 1,
                     lineheight = 1.2
                   ),

          draw_panel = function(data,
                                panel_params,
                                coord,
                                add.segments = TRUE,
                                box.padding = 0.25,
                                point.padding = 1e-06,
                                segment.linewidth = 0.5,
                                min.segment.length = 0,
                                arrow = NULL,
                                table.theme = NULL,
                                table.rownames = FALSE,
                                table.colnames = TRUE,
                                table.hjust = 0.5,
                                parse = FALSE,
                                default.colour = NA,
                                colour.target = "table.text",
                                default.alpha = NA,
                                alpha.target = "all",
                                fontsize.scaling = 0.825,
                                na.rm = FALSE) {

            if (nrow(data) == 0) {
              return(grid::nullGrob())
            }

            if (!is.data.frame(data$label[[1]])) {
              warning("Skipping as object mapped to 'label' is not a list of ",
                      "\"tibble\" or \"data.frame\" objects.")
              return(grid::nullGrob())
            }

            # replace NULL with default at time of rendering!
            if (is.null(table.theme)) {
              table.theme <-
                getOption("ggpmisc.ttheme.default",
                          default = ggpp::ttheme_gtdefault)
            }

            default.colour <- check_default_colour(default.colour)

            add.segments <-
              add.segments && all(c("x_orig", "y_orig") %in% colnames(data))

            # should be called only once!
            data <- coord$transform(data, panel_params)
            if (add.segments) {
              data_orig <- data.frame(x = data$x_orig, y = data$y_orig)
              data_orig <- coord$transform(data_orig, panel_params)
              data$x_orig <- data_orig$x
              data$y_orig <- data_orig$y
            }

            if (is.character(data$vjust)) {
              data$vjust <-
                compute_just2d(data = data,
                               coord = coord,
                               panel_params = panel_params,
                               just = data$vjust,
                               a = "y", b = "x")
            }
            if (is.character(data$hjust)) {
              data$hjust <-
                compute_just2d(data = data,
                               coord = coord,
                               panel_params = panel_params,
                               just = data$hjust,
                               a = "x", b = "y")
            }
            if (add.segments) {
              segments.data <-
                shrink_segments(data,
                                point.padding = point.padding,
                                box.padding = box.padding,
                                min.segment.length = min.segment.length)
            }
            # for the table itself default.colour NA uses the table theme
            # box and segment use "black" as default as in earlier versions
            if (is.na(default.colour)) {
              off.table.default.colour <- "black"
            } else {
              off.table.default.colour <- default.colour
            }

            # loop needed as gpar is not vectorized
            all.grobs <- grid::gList()
            for (row.idx in seq_len(nrow(data))) {
              row <- data[row.idx, , drop = FALSE]
              # Build the table
              if (is.function(table.theme)) {
                # tableGrob puts all the padding on the same side unless
                # just = this makes it difficult to compute a suitable value
                # for table.x without knowing the width of the column. The code
                # here at least ensures that whatever its length the whole text
                # is always displayed.
                table.x <- table.hjust
                # alpha
                text.alpha <-
                  ifelse(any(alpha.target %in% c("all", "table", "table.text", "text")),
                         row$alpha, default.alpha)
                rules.alpha <-
                  ifelse(any(alpha.target %in% c("all", "table", "table.rules")),
                         row$alpha, default.alpha)
                canvas.alpha <-
                  ifelse(any(alpha.target %in% c("all", "table", "table.canvas")),
                         row$alpha, default.alpha)
                segment.alpha <-
                  ifelse(any(alpha.target %in% c("all", "segment")),
                         row$alpha, default.alpha)
                box.alpha <-
                  ifelse(any(alpha.target %in% c("all", "box")),
                         row$alpha, default.alpha)
                # colour
                text.colour <-
                  ifelse(any(colour.target %in% c("all", "table", "table.text", "text")),
                         row$colour, default.colour)
                # text.alpha applied in ttheme constructor
                rules.colour <-
                  ifelse(any(colour.target %in% c("all", "table", "table.rules")),
                         row$colour, default.colour) |>
                  ggplot2::alpha(rules.alpha)
                segment.colour <-
                  ifelse(any(colour.target %in% c("all", "segment")),
                         row$colour, off.table.default.colour) |>
                  ggplot2::alpha(segment.alpha)
                box.colour <-
                  ifelse(any(colour.target %in% c("all", "box")),
                         row$colour, off.table.default.colour) |>
                  ggplot2::alpha(box.alpha)
                # fill
                canvas.fill <- row$fill |>
                  ggplot2::alpha(canvas.alpha)

                if (is.na(canvas.fill)) {
                  core.params <-
                    list(fg_params = list(hjust = table.hjust, x = table.x))
                  rowhead.params <- list(fg_params = list(hjust = 1, x = 0.9))
                  colhead.params <- list(fg_params = list(hjust = table.hjust,
                                                          x = table.x))
                } else {
                  # override ttheme fill for background canvas
                  core.params <-
                    list(fg_params = list(hjust = table.hjust, x = table.x),
                         bg_params = list(fill = canvas.fill))
                  rowhead.params <-
                    list(fg_params = list(hjust = 1, x = 0.9),
                         bg_params = list(fill = canvas.fill))
                  colhead.params <- list(fg_params = list(hjust = table.hjust,
                                                          x = table.x),
                                         bg_params = list(fill = canvas.fill))
                }
                # override ttheme colour for background rules
                if (!is.na(rules.colour)) {
                  rules.colour <- ggplot2::alpha(rules.colour, rules.alpha)
                  core.params$bg_params$col <- rules.colour
                  rowhead.params$bg_params$col <- rules.colour
                  colhead.params$bg_params$col <- rules.colour
                }
                if (is.na(text.colour)) {
                  # use theme's default base_colour
                  this.table.theme <-
                    table.theme(base_size = row$size * fontsize.scaling * .pt,
                                base_family = row$family,
                                parse = parse,
                                text.alpha = text.alpha,
                                canvas.alpha = canvas.alpha,
                                rules.alpha = rules.alpha,
                                rowhead = rowhead.params,
                                colhead = colhead.params,
                                core = core.params)
                } else {
                  # use colour from data$colour
                  this.table.theme <-
                    table.theme(base_size = row$size * fontsize.scaling * .pt,
                                base_colour = text.colour,
                                base_family = row$family,
                                parse = parse,
                                text.alpha = text.alpha,
                                canvas.alpha = canvas.alpha,
                                rules.alpha = rules.alpha,
                                rowhead = rowhead.params,
                                colhead = colhead.params,
                                core = core.params)
                }
              } else if (is.list(table.theme)) {
                this.table.theme <- table.theme
              }

              table.tb <- data[["label"]][[row.idx]]
              user.grob <-
                gridExtra::tableGrob(
                  d = table.tb,
                  theme = this.table.theme,
                  rows = if (table.rownames) rownames(table.tb) else NULL,
                  cols = if (table.colnames) colnames(table.tb) else NULL
                )

              user.grob$vp <-
                grid::viewport(x = grid::unit(row$x, "native"),
                               y = grid::unit(row$y, "native"),
                               width = sum(user.grob$widths),
                               height = sum(user.grob$heights),
                               just = c(row$hjust, row$vjust),
                               angle = row$angle,
                               name = paste("inset.table.vp", row$PANEL,
                                            "row", row.idx, sep = "."))

              # give unique name to each grob
              user.grob$name <- paste("inset.table", row.idx, sep = ".")

              if (add.segments) {
                segment.row <- segments.data[row.idx, , drop = FALSE]
                if (segment.row$too.short) {
                  segment.grob <- grid::nullGrob()
                } else {
                  segment.grob <-
                    grid::segmentsGrob(x0 = segment.row$x,
                                       y0 = segment.row$y,
                                       x1 = segment.row$x_orig,
                                       y1 = segment.row$y_orig,
                                       arrow = arrow,
                                       gp = grid::gpar(
                                         col = if (segment.linewidth == 0) NA else
                                           # lwd = 0 is invalid in 'grid'
                                           segment.colour,
                                         lwd = if (segment.linewidth == 0) 0.5 else
                                           segment.linewidth * ggplot2::.stroke),
                                       name = paste("table.s.segment",
                                                    row$group,
                                                    row.idx, sep = "."))
                }
                all.grobs <- grid::gList(all.grobs, segment.grob, user.grob)
              } else {
                all.grobs <- grid::gList(all.grobs, user.grob)
              }
            }
            #    grid::grobTree(children = all.grobs, name = "geom.table.panel")
            grid::grobTree(children = all.grobs)

          },

          draw_key = function(...) {
            grid::nullGrob()
          }
  )

#' @rdname geom_table
#' @export
#'
geom_table_npc <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           default.colour = NA,
                           default.color = default.colour,
                           colour.target = "table.text",
                           color.target = colour.target,
                           default.alpha = 1,
                           alpha.target = "all",
                           fontsize.scaling = 0.825,
                           table.theme = NULL,
                           table.rownames = FALSE,
                           table.colnames = TRUE,
                           table.hjust = 0.5,
                           parse = FALSE,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = FALSE) {

  colour.target <-
    rlang::arg_match(color.target,
                     values = c("segment", "all", "box", "text", "table",
                                "table.text", "table.rules", "none"),
                     multiple = TRUE)
  alpha.target <-
    rlang::arg_match(alpha.target,
                     values = c("segment", "all", "box", "text", "table",
                                "table.text", "table.rules", "table.canvas",
                                "none"),
                     multiple = TRUE)

  if (is.character(table.hjust)) {
    table.hjust <- switch(table.hjust,
                          left = 0,
                          middle = 0.5,
                          center = 0.5,
                          right = 1,
                          0.5)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTableNpc,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      default.colour = default.color,
      colour.target = colour.target,
      default.alpha = default.alpha,
      alpha.target = alpha.target,
      fontsize.scaling = fontsize.scaling,
      table.theme = table.theme,
      table.rownames = table.rownames,
      table.colnames = table.colnames,
      table.hjust = table.hjust,
      parse = parse,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTableNpc <-
  ggplot2::ggproto("GeomTableNpc", ggplot2::Geom,
          required_aes = c("npcx", "npcy", "label"),

          default_aes = ggplot2::aes(
            colour = NA,
            fill = NA,
            size = 3.2,
            angle = 0,
            hjust = "inward",
            vjust = "inward",
            alpha = NA,
            family = "",
            fontface = 1,
            lineheight = 1.2
          ),

          draw_panel =
            function(data,
                     panel_params,
                     coord,
                     table.theme = NULL,
                     table.rownames = FALSE,
                     table.colnames = TRUE,
                     table.hjust = 0.5,
                     parse = FALSE,
                     default.colour = NA,
                     colour.target = "table.text",
                     default.alpha = NA,
                     alpha.target = "all",
                     fontsize.scaling = 0.825,
                     na.rm = FALSE) {

              if (nrow(data) == 0) {
                return(grid::nullGrob())
              }

              if (!is.data.frame(data$label[[1]])) {
                warning("Skipping as object mapped to 'label' is not a list of ",
                        "\"tibble\" or \"data.frame\" objects.")
                return(grid::nullGrob())
              }

              # replace NULL with default
              if (is.null(table.theme)) {
                table.theme <-
                  getOption("ggpmisc.ttheme.default",
                            default = ggpp::ttheme_gtdefault)
              }

              default.colour <- check_default_colour(default.colour)

              data$npcx <- compute_npcx(data$npcx)
              data$npcy <- compute_npcy(data$npcy)

              if (is.character(data$vjust)) {
                data$vjust <- compute_just(data$vjust, data$npcy)
              }
              if (is.character(data$hjust)) {
                data$hjust <- compute_just(data$hjust, data$npcx)
              }

              # for the table itself default.colour NA uses the table theme
              # box and segment use "black" as default as in earlier versions
              if (is.na(default.colour)) {
                off.table.default.colour <- "black"
              } else {
                off.table.default.colour <- default.colour
              }

              tb.grobs <- grid::gList()
              for (row.idx in seq_len(nrow(data))) {
                row <- data[row.idx, , drop = FALSE]
                # Build the table
                if (is.function(table.theme)) {
                  # tableGrob puts all the padding on the same side unless just = 0.5
                  # this makes it difficult to compute a suitable value for table.x
                  # without knowing the width of the column. The code here at least
                  # ensures that whatever its length the whole text is always displayed.
                  table.x <- table.hjust
                  # text position in cell depends on hjust
#                  table.x <- if(table.hjust == 0.5) 0.5 else table.hjust * 0.8 + 0.1
                  # alpha
                  text.alpha <-
                    ifelse(any(alpha.target %in% c("all", "table", "table.text", "text")),
                           row$alpha, default.alpha)
                  rules.alpha <-
                    ifelse(any(alpha.target %in% c("all", "table", "table.rules")),
                           row$alpha, default.alpha)
                  canvas.alpha <-
                    ifelse(any(alpha.target %in% c("all", "table", "table.canvas")),
                           row$alpha, default.alpha)
                  box.alpha <-
                    ifelse(any(alpha.target %in% c("all", "box")),
                           row$alpha, default.alpha)
                  # colour
                  text.colour <-
                    ifelse(any(colour.target %in% c("all", "table", "table.text", "text")),
                           row$colour, default.colour) |>
                    ggplot2::alpha(text.alpha)
                  rules.colour <-
                    ifelse(any(colour.target %in% c("all", "table", "table.rules")),
                           row$colour, default.colour) |>
                    ggplot2::alpha(rules.alpha)
                  box.colour <-
                    ifelse(any(colour.target %in% c("all", "box")),
                           row$colour, off.table.default.colour) |>
                    ggplot2::alpha(box.alpha)
                  # fill
                  canvas.fill <- row$fill |>
                    ggplot2::alpha(canvas.alpha)

                  if (is.na(canvas.fill)) {
                    core.params <-
                      list(fg_params = list(hjust = table.hjust, x = table.x))
                    rowhead.params <- list(fg_params = list(hjust = 1, x = 0.9))
                    colhead.params <- list(fg_params = list(hjust = table.hjust,
                                                            x = table.x))
                  } else {
                    # override ttheme fill for background canvas
                    core.params <-
                      list(fg_params = list(hjust = table.hjust, x = table.x),
                           bg_params = list(fill = canvas.fill))
                    rowhead.params <-
                      list(fg_params = list(hjust = 1, x = 0.9),
                           bg_params = list(fill = canvas.fill))
                    colhead.params <- list(fg_params = list(hjust = table.hjust,
                                                            x = table.x),
                                           bg_params = list(fill = canvas.fill))
                  }
                  # override ttheme colour for background rules
                  if (!is.na(rules.colour)) {
                    rules.colour <- ggplot2::alpha(rules.colour, rules.alpha)
                    core.params$bg_params$col <- rules.colour
                    rowhead.params$bg_params$col <- rules.colour
                    colhead.params$bg_params$col <- rules.colour
                  }
                  if (is.na(text.colour)) {
                    # use theme's default base_colour
                    this.table.theme <-
                      table.theme(base_size = row$size * fontsize.scaling * .pt,
                                  base_family = row$family,
                                  parse = parse,
                                  text.alpha = text.alpha,
                                  canvas.alpha = canvas.alpha,
                                  rules.alpha = rules.alpha,
                                  rowhead = rowhead.params,
                                  colhead = colhead.params,
                                  core = core.params)
                  } else {
                    # use colour from data$colour
                    this.table.theme <-
                      table.theme(base_size = row$size * fontsize.scaling * .pt,
                                  base_colour = text.colour,
                                  base_family = row$family,
                                  parse = parse,
                                  text.alpha = text.alpha,
                                  canvas.alpha = canvas.alpha,
                                  rules.alpha = rules.alpha,
                                  rowhead = rowhead.params,
                                  colhead = colhead.params,
                                  core = core.params)
                  }
                } else if (is.list(table.theme)) {
                  this.table.theme <- table.theme
                }

                table.tb <- data[["label"]][[row.idx]]
                gtb <-
                  gridExtra::tableGrob(
                    d = table.tb,
                    theme = this.table.theme,
                    rows = if (table.rownames) rownames(table.tb) else NULL,
                    cols = if (table.colnames) colnames(table.tb) else NULL
                  )

                gtb$vp <-
                  grid::viewport(x = grid::unit(data$npcx[row.idx], "native"),
                                 y = grid::unit(data$npcy[row.idx], "native"),
                                 width = sum(gtb$widths),
                                 height = sum(gtb$heights),
                                 just = c(data$hjust[row.idx], data$vjust[row.idx]),
                                 angle = data$angle[row.idx],
                                 name = paste("geom_table.panel", data$PANEL[row.idx],
                                              "row", row.idx, sep = "."))

                # give unique name to each table
                gtb$name <- paste("table", row.idx, sep = ".")

                tb.grobs[[row.idx]] <- gtb
              }

              grid::grobTree(children = tb.grobs)
            },

          draw_key = function(...) {
            grid::nullGrob()
          }
  )

#' Table themes
#'
#' Additional theme constructors for use with \code{\link{geom_table}}.
#'
#' @details These wrapper functions are table theme (\code{ttheme}) constructors
#'   making it easier to change the style of tables created with
#'   \code{\link[gridExtra]{tableGrob}}. When passed as argument to
#'   \code{\link{geom_table}} the table theme's \code{base_colour},
#'   \code{base_family}, \code{base_colour} and \code{base_size} function as
#'   defaults for the text in the body of the table. They are overriden if the
#'   corresponding text-related aesthetics are mapped or set to a constant
#'   through the usual 'ggplot2' mechanisms. On the other hand the properties
#'   of the background fill, rules and column and row headings can be set only
#'   through the theme. The \code{ttheme} constructors defined in
#'   'ggpp' have formal parameters for \code{alpha} transparency of the
#'   text, background and rules. Transparency is useful as plot insets can
#'   accidentally overlap observations hiding them from view depending on the
#'   stacking order of plot layers.
#'
#'   These theme constructors are wrappers on the constructors
#'   \code{gridExtra::ttheme_default()} and \code{gridExtra::ttheme_minimal()}.
#'   They can also be used directly with \code{\link[gridExtra]{grid.table}} if
#'   desired.
#'
#' @param base_size numeric, default font size of text in table.
#' @param base_colour	default font colour for text in table.
#' @param base_family	default font family for text in table.
#' @param parse	logical, behaviour for parsing text as plotmath.
#' @param padding length-2 unit vector specifying the horizontal and vertical
#'   padding of text within each cell.
#' @param text.alpha,canvas.alpha,rules.alpha numeric in [0..1] Transparency
#'   applied to table \code{base_colour}, to table body background
#'   \code{fill} and rules \code{colour}, respectively.
#' @param ... further arguments to control the gtable.
#'
#' @return A \code{list} object that can be used as \code{ttheme} in the
#'   construction of tables with functions from package 'gridExtra'.
#'
#' @export
#'
#' @family geometries for adding insets to ggplots
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' mtcars |>
#'   group_by(cyl) |>
#'   summarize(wt = mean(wt), mpg = mean(mpg)) |>
#'   ungroup() |>
#'   mutate(wt = sprintf("%.2f", wt),
#'          mpg = sprintf("%.1f", mpg)) -> tb
#'
#' df <- tibble(x = 5.45, y = 34, tb = list(tb))
#'
#' # Same as the default theme constructor
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtdefault) +
#'   theme_classic()
#'
#' # Minimal theme constructor
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtminimal) +
#'   theme_classic()
#'
#' # A theme with white background
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtbw) +
#'   theme_bw()
#'
#' # Base colour of theme overridden by aesthetic constant
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtbw, colour = "darkblue") +
#'   theme_bw()
#'
#' # A theme with dark background
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtdark) +
#'   theme_dark()
#'
#' # Base colour of theme overridden by aesthetic constant
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtdark, colour = "yellow") +
#'   theme_dark()
#'
#' # A theme with light background
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtlight)
#'
#' # Base colour of theme overridden by aesthetic constant
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtlight, colour = "darkred")
#'
#' # Base colour of theme overridden by aesthetic constant
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtsimple)
#'
#' # Default colour of theme superceded by aesthetic constant
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtstripes) +
#'   theme_dark()
#'
#' # Transparency of table background fill and grid lines colour
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = 1.5, y = y, label = tb),
#'              table.theme = ttheme_gtplain(canvas.alpha = 0.5,
#'                                           rules.alpha = 0.2)) +
#'   theme_classic()
#'
#' # Transparency of table background fill and grid lines colour
#' # and table text base colour: black with 50% transparency
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = 1.5, y = y, label = tb),
#'              table.theme = ttheme_gtplain(text.alpha = 0.5)) +
#'   theme_classic()
#'
ttheme_gtdefault <- function(base_size = 10,
                             base_colour = "black",
                             base_family = "",
                             parse = FALSE,
                             padding = grid::unit(c(0.8, 0.6), "char"),
                             text.alpha = NA,
                             rules.alpha = NA,
                             canvas.alpha = NA,
                             ...) {
  core <-
    list(bg_params =
           list(fill = ggplot2::alpha(c("grey95", "grey90"), canvas.alpha),
                col = ggplot2::alpha("white", rules.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  colhead <-
    list(bg_params = list(fill = ggplot2::alpha("grey80", canvas.alpha),
                          col = ggplot2::alpha("white", rules.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  rowhead <-
    list(bg_params = list(fill = ggplot2::alpha("grey80", canvas.alpha),
                          col = ggplot2::alpha("white", rules.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))

  default <-
    gridExtra::ttheme_default(base_size = base_size,
                              base_colour = base_colour,
                              base_family = base_family,
                              parse = parse,
                              padding = padding,
                              core = core,
                              colhead = colhead,
                              rowhead = rowhead,
                              ...)

  utils::modifyList(default, list(...))
}

#' @rdname ttheme_gtdefault
#'
#' @export
#'
ttheme_gtminimal <- function(base_size = 10,
                             base_colour = "black",
                             base_family = "",
                             parse = FALSE,
                             padding = grid::unit(c(0.5, 0.4), "char"),
                             text.alpha = NA,
                             rules.alpha = NA,
                             canvas.alpha = NA,
                             ...) {
  default <-
    gridExtra::ttheme_minimal(base_size = base_size,
                              base_colour = base_colour,
                              base_family = base_family,
                              parse = parse,
                              padding = padding,
                              ...)

  utils::modifyList(default, list(...))
}

#' @rdname ttheme_gtdefault
#'
#' @export
#'
ttheme_gtbw <- function(base_size = 10,
                        base_colour = "black",
                        base_family = "",
                        parse = FALSE,
                        padding = grid::unit(c(1, 0.6), "char"),
                        text.alpha = NA,
                        rules.alpha = NA,
                        canvas.alpha = NA,
                        ...) {
  core <-
    list(bg_params =
           list(fill = ggplot2::alpha("white", canvas.alpha),
                lwd = 1.5,
                col = ggplot2::alpha("grey90", rules.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  colhead <- rowhead <-
    list(bg_params = list(fill = ggplot2::alpha("grey80", canvas.alpha),
                          lwd = 1.5,
                          col = ggplot2::alpha("grey90", rules.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))

  default <-
    gridExtra::ttheme_default(base_size = base_size,
                              base_colour = base_colour,
                              base_family = base_family,
                              parse = parse,
                              padding = padding,
                              core = core,
                              colhead = colhead,
                              rowhead = rowhead)

  utils::modifyList(default, list(...))
}

#' @rdname ttheme_gtdefault
#'
#' @export
#'
ttheme_gtplain <- function(base_size = 10,
                           base_colour = "black",
                           base_family = "",
                           parse = FALSE,
                           padding = grid::unit(c(0.8, 0.6), "char"),
                           text.alpha = NA,
                           rules.alpha = NA,
                           canvas.alpha = NA,
                           ...) {
  core <-
    list(bg_params =
           list(fill = ggplot2::alpha("white", canvas.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  colhead <- rowhead <-
    list(bg_params = list(fill = ggplot2::alpha("grey90", canvas.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  default <-
    gridExtra::ttheme_default(base_size = base_size,
                              base_colour = base_colour,
                              base_family = base_family,
                              parse = parse,
                              padding = padding,
                              core = core,
                              colhead = colhead,
                              rowhead = rowhead)

  utils::modifyList(default, list(...))
}

#' @rdname ttheme_gtdefault
#'
#' @export
#'
ttheme_gtdark <- function(base_size = 10,
                          base_colour = "grey90",
                          base_family = "",
                          parse = FALSE,
                          padding = grid::unit(c(0.8, 0.6), "char"),
                          text.alpha = NA,
                          rules.alpha = NA,
                          canvas.alpha = NA,
                          ...) {
  core <-
    list(bg_params =
           list(fill = ggplot2::alpha("grey30", canvas.alpha),
                lwd = 1.5,
                col = ggplot2::alpha(base_colour, rules.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  colhead <- rowhead <-
    list(bg_params = list(fill = ggplot2::alpha("black", canvas.alpha),
                          lwd = 1.5,
                          col = ggplot2::alpha(base_colour, rules.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  default <-
    gridExtra::ttheme_default(base_size = base_size,
                              base_colour = base_colour,
                              base_family = base_family,
                              parse = parse,
                              padding = padding,
                              core = core,
                              colhead = colhead,
                              rowhead = rowhead)

  utils::modifyList(default, list(...))
}

#' @rdname ttheme_gtdefault
#'
#' @export
#'
ttheme_gtlight <- function(base_size = 10,
                           base_colour = "grey10",
                           base_family = "",
                           parse = FALSE,
                           padding = grid::unit(c(0.8, 0.6), "char"),
                           text.alpha = NA,
                           rules.alpha = NA,
                           canvas.alpha = NA,
                           ...) {
  core <-
    list(bg_params =
           list(fill = ggplot2::alpha("white", canvas.alpha),
                lwd = 1.5,
                col = ggplot2::alpha(base_colour, rules.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  colhead <- rowhead <-
    list(bg_params = list(fill = ggplot2::alpha("grey80", canvas.alpha),
                          lwd = 1.5,
                          col = ggplot2::alpha(base_colour, rules.alpha)),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))

  default <-
    gridExtra::ttheme_default(base_size = base_size,
                              base_colour = base_colour,
                              base_family = base_family,
                              parse = parse,
                              padding = padding,
                              core = core,
                              colhead = colhead,
                              rowhead = rowhead)

  utils::modifyList(default, list(...))
}

#' @rdname ttheme_gtdefault
#'
#' @export
#'
ttheme_gtsimple <- function(base_size = 10,
                            base_colour = "grey10",
                            base_family = "",
                            parse = FALSE,
                            padding = grid::unit(c(0.5, 0.4), "char"),
                            text.alpha = NA,
                            rules.alpha = NA,
                            canvas.alpha = NA,
                            ...) {
  core <-
    list(bg_params =
           list(fill = ggplot2::alpha("white", canvas.alpha),
                lwd = 0,
                col = NA),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  colhead <- rowhead <-
    list(bg_params = list(fill = ggplot2::alpha("grey80", canvas.alpha),
                          lwd = 0,
                          col = NA),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))

  default <-
    gridExtra::ttheme_default(base_size = base_size,
                              base_colour =base_colour,
                              base_family = base_family,
                              parse = parse,
                              padding = padding,
                              core = core,
                              colhead = colhead,
                              rowhead = rowhead)

  utils::modifyList(default, list(...))
}

#' @rdname ttheme_gtdefault
#'
#' @export
#'
ttheme_gtstripes <- function(base_size = 10,
                             base_colour = "grey10",
                             base_family = "",
                             parse = FALSE,
                             padding = grid::unit(c(0.8, 0.6), "char"),
                             text.alpha = NA,
                             rules.alpha = NA,
                             canvas.alpha = NA,
                             ...) {
  core <-
    list(bg_params =
           list(fill = ggplot2::alpha(c("white", "grey90"), canvas.alpha),
                lwd = 0,
                col = NA),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  rowhead <- colhead <-
    list(bg_params = list(fill = ggplot2::alpha("grey75", canvas.alpha),
                          lwd = 0,
                          col = NA),
         fg_params =
           list(col = ggplot2::alpha(base_colour, text.alpha)))
  # colhead <-
  #   list(bg_params = list(fill = ggplot2::alpha("white", canvas.alpha),
  #                         lwd = 0,
  #                         col = NA))
  default <-
    gridExtra::ttheme_default(base_size = base_size,
                              base_colour = base_colour,
                              base_family = base_family,
                              parse = parse,
                              padding = padding,
                              core = core,
                              colhead = colhead,
                              rowhead = rowhead)

  utils::modifyList(default, list(...))
}

#' Set default table theme
#'
#' Set R option to the theme to use as current default. This function is
#' implemented differently but is used in the same way as
#' \code{ggplot2::theme_set()} but affects the default table-theme instead
#' of the plot theme.
#'
#' @note The ttheme is set when a plot object is constructed, and consequently
#' the option setting does not affect rendering of ready built plot objects.
#'
#' @param table.theme NULL, list or function A gridExtra ttheme defintion, or
#'   a constructor for a ttheme or NULL for default.
#'
#' @return A named list with the previous value of the option.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#'
#' mtcars |>
#'   group_by(cyl) |>
#'   summarize(wt = mean(wt), mpg = mean(mpg)) |>
#'   ungroup() |>
#'   mutate(wt = sprintf("%.2f", wt),
#'          mpg = sprintf("%.1f", mpg)) -> tb
#'
#' df <- tibble(x = 5.45, y = 34, tb = list(tb))
#'
#' # Same as the default theme constructor
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb))
#'
#' # set a new default
#' old_ttheme <- ttheme_set(ttheme_gtstripes)
#'
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb))
#'
#' # restore previous setting
#' ttheme_set(old_ttheme)
#'
ttheme_set <- function(table.theme = NULL) {
  stopifnot("Bad argument passed to 'table.theme'" =
              is.null(table.theme) ||
              is.function(table.theme) ||
              is.list(table.theme))
  invisible(options(ggpmisc.ttheme.default = table.theme)[[1]])
}

#' @rdname ttheme_set
#'
set_ttheme <- ttheme_set

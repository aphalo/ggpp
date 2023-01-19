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
#' @details You can modify the size of inset tables with the \code{vp.width} and
#'   \code{vp.height} aesthetics. These can take a number between 0 (smallest
#'   possible inset) and 1 (whole plotting area width or height). The default
#'   value for for both of these aesthetics is 1/5. Thus, in contrast to
#'   \code{\link[ggplot2]{geom_text}} and \code{\link{geom_text_s}} the size of
#'   the insets remains the same relative to the size of the plotting area
#'   irrespective of how the plot is rendered. The aspect ratio of insets is
#'   preserved and size is adjusted until the whole inset fits within the
#'   viewport.
#'
#'   By default this geom uses \code{\link{position_nudge_center}} which is
#'   backwards compatible with \code{\link[ggplot2]{position_nudge}} but
#'   provides additional control on the direction of the nudging. In contrast to
#'   \code{\link[ggplot2]{position_nudge}}, \code{\link{position_nudge_center}}
#'   and all other position functions defined in packages 'ggpp' and 'ggrepel'
#'   keep the original coordinates thus allowing the plotting of connecting
#'   segments and arrows.
#'
#'   This geom works only with tibbles as \code{data}, as its expects a list of
#'   data frames (or tibbles) to be mapped to the \code{label} aesthetic.
#'   A table is built with function \code{gridExtra::gtable} for each
#'   data frame in the list, and formatted according to a table theme or
#'   \code{ttheme}. The character strings in the data frame can be parsed into
#'   R expressions so the inset tables can include maths.
#'
#'   If the argument passed to \code{table.theme} is a constructor function
#'   (passing its name without parenthesis), the values mapped to \code{size},
#'   \code{colour}, \code{fill}, \code{alpha}, and \code{family} aesthetics will
#'   the passed to this theme constructor for each individual table. In
#'   contrast, if a ready constructed \code{ttheme} stored as a list object is
#'   passed as argument (e.g., by calling the constructor, using constructor
#'   name followed by parenthesis), it will be used as is, i.e., mappings to
#'   aesthetics such as \code{colour} are ignored if present. By default the
#'   constructor \code{ttheme_gtdefault} is used and \code{colour} and
#'   \code{fill}, are mapped to \code{NA}. Mapping these aesthetics to \code{NA}
#'   triggers the use of the default \code{base_colour} of the \code{ttheme}.
#'   As the table is built with function \code{gridExtra::gtable()}, for
#'   formatting details, please, consult \code{\link[gridExtra]{tableGrob}}.
#'
#'   The \code{x} and \code{y} aesthetics determine the position of the whole
#'   inset table, similarly to that of a text label, justification is
#'   interpreted as indicating the position of the inset table with respect to
#'   its \emph{horizontal} and \emph{vertical} axes (rows and columns in the
#'   data frame), and \code{angle} is used to rotate the inset table as a whole.
#'
#'   In the case of \code{geom_table_npc}, \code{npcx} and \code{npcy}
#'   aesthetics determine the position of the inset table. Justification as
#'   described above for .
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
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}}. Only needs to be set at the layer level if you
#'   are overriding the plot defaults.
#' @param data A layer specific data set - only needed if you want to override
#'   the plot defaults.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param position Position adjustment, either as a string, or the result of a
#<'   call to a position adjustment function.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param table.theme NULL, list or function A gridExtra ttheme defintion, or
#'   a constructor for a ttheme or NULL for default.
#' @param table.rownames,table.colnames logical flag to enable or disable
#'   printing of row names and column names.
#' @param table.hjust numeric Horizontal justification for the core and column
#'   headings of the table.
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label. The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param default.colour A colour definition to use for elements not targeted by
#'   the colour aesthetic.
#' @param colour.target A vector of character strings; \code{"all"},
#'   \code{"text"}, \code{"box"} and \code{"segment"}.
#' @param default.alpha numeric in [0..1] A transparency value to use for
#'   elements not targeted by the alpha aesthetic.
#' @param alpha.target A vector of character strings; \code{"all"},
#'   \code{"text"}, \code{"segment"}, \code{"box"}, \code{"box.line"}, and
#'   \code{"box.fill"}.
#' @param add.segments logical Display connecting segments or arrows between
#'   original positions and displaced ones if both are available.
#' @param box.padding,point.padding numeric By how much each end of the segments
#'   should shortened in mm.
#' @param segment.linewidth numeric Width of the segments or arrows in mm.
#' @param min.segment.length numeric Segments shorter that the minimum length
#'   are not rendered, in mm.
#' @param arrow specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}
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
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarize(wt = mean(wt), mpg = mean(mpg)) %>%
#'   ungroup() %>%
#'   mutate(wt = sprintf("%.2f", wt),
#'          mpg = sprintf("%.1f", mpg)) -> tb
#'
#' df <- tibble(x = 5.45, y = 34, tb = list(tb))
#'
#' # using defaults
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df,
#'              aes(x = x, y = y, label = tb))
#'
#' ggplot(mtcars,
#'        aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df,
#'              aes(x = x, y = y, label = tb),
#'              table.rownames = TRUE,
#'              table.theme = ttheme_gtstripes)
#'
#' # settings aesthetics to constants
#' ggplot(mtcars,
#'        aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df,
#'              aes(x = x, y = y, label = tb),
#'              color = "red", fill = "#FFCCCC",
#'              family = "serif", size = 5,
#'              angle = 90, vjust = 0)
#'
#' # passing a theme constructor as argument
#' ggplot(mtcars,
#'        aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df,
#'              aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtminimal) +
#'   theme_classic()
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
#' # Using native plot coordinates instead of data coordinates
#' dfnpc <- tibble(x = 0.95, y = 0.95, tb = list(tb))
#'
#' ggplot(mtcars,
#'        aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table_npc(data = dfnpc,
#'                  aes(npcx = x, npcy = y, label = tb))
#'
geom_table <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       nudge_x = 0,
                       nudge_y = 0,
                       default.colour = "black",
                       colour.target = "segment",
                       default.alpha = 1,
                       alpha.target = "segment",
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
      default.colour = default.colour,
      colour.target = colour.target,
      default.alpha = default.alpha,
      alpha.target = alpha.target,
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

# Defined here to avoid a note in check --as-cran as the imports from 'broom'
# are not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpp-ggproto
#'
#' @format NULL
#' @usage NULL
#'
gtb_draw_panel_fun <-
  function(data,
           panel_params,
           coord,
           add.segments = TRUE,
           box.padding = 0.25,
           point.padding = 1e-06,
           segment.linewidth = 1,
           min.segment.length = 0,
           arrow = NULL,
           table.theme = NULL,
           table.rownames = FALSE,
           table.colnames = TRUE,
           table.hjust = 0.5,
           parse = FALSE,
           default.colour = "black",
           colour.target = "all",
           default.alpha = 1,
           alpha.target = "all",
           na.rm = FALSE) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (!is.data.frame(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not a list of ",
              "\"tibble\" or \"data.frame\" objects.")
      return(grid::nullGrob())
    }

    add.segments <- add.segments && all(c("x_orig", "y_orig") %in% colnames(data))

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

    # replace NULL with default
    if (is.null(table.theme)) {
      table.theme <-
        getOption("ggpmisc.ttheme.default", default = ttheme_gtdefault)
    }

    # loop needed as gpar is not vectorized
    all.grobs <- grid::gList()

    for (row.idx in 1:nrow(data)) {
      row <- data[row.idx, , drop = FALSE]
      table.alpha <-
        ifelse(any(alpha.target %in% c("all", "table")),
               row$alpha, default.alpha)
      segment.alpha <-
        ifelse(any(alpha.target %in% c("all", "segment")),
               row$alpha, default.alpha)

      # Build the table
      if (is.function(table.theme)) {
        # tableGrob puts all the padding on the same side unless just = 0.5
        # this makes it difficult to compute a suitable value for table.x
        # without knowing the width of the column. The code here at least
        # ensures that whatever its length the whole text is always displayed.
        table.x <- table.hjust
        if (is.na(row$fill)) {
          core.params <-
            list(fg_params = list(hjust = table.hjust, x = table.x))
        } else {
          core.params <-
            list(fg_params = list(hjust = table.hjust, x = table.x),
                 bg_params = list(fill = row$fill))
        }
        if (is.na(row$colour)) {
          # use theme's default base_colour
          this.table.theme <-
            table.theme(base_size = row$size * .pt,
                        base_family = row$family,
                        parse = parse,
                        rowhead = list(fg_params = list(hjust = 1, x = 0.9)),
                        colhead = list(fg_params = list(hjust = table.hjust,
                                                        x = table.x)),
                        core = core.params)
        } else {
          this.table.theme <-
            # use colour from data$colour
            table.theme(base_size = row$size * .pt,
                        base_colour = ggplot2::alpha(row$colour, row$alpha),
                        base_family = row$family,
                        parse = parse,
                        rowhead = list(fg_params = list(hjust = 1, x = 0.9)),
                        colhead = list(fg_params = list(hjust = table.hjust,
                                                        x = table.x)),
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
                                 col = if (segment.linewidth == 0) NA else # lwd = 0 is invalid in 'grid'
                                   ifelse(any(colour.target %in% c("all", "segment")),
                                          ggplot2::alpha(row$colour, segment.alpha),
                                          ggplot2::alpha(default.colour, segment.alpha)),
                                 lwd = (if (segment.linewidth == 0) 0.5 else segment.linewidth) * .stroke),
                               name = paste("table.s.segment", row$group, row.idx, sep = "."))
        }
        all.grobs <- grid::gList(all.grobs, segment.grob, user.grob)
      } else {
        all.grobs <- grid::gList(all.grobs, user.grob)
      }
    }
  #    grid::grobTree(children = all.grobs, name = "geom.table.panel")
    grid::grobTree(children = all.grobs)

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
            size = 3.2,
            angle = 0,
            hjust = "inward",
            vjust = "inward",
            alpha = 1,
            family = "",
            fontface = 1,
            lineheight = 1.2
          ),

          draw_panel = gtb_draw_panel_fun,
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
                           table.theme = NULL,
                           table.rownames = FALSE,
                           table.colnames = TRUE,
                           table.hjust = 0.5,
                           parse = FALSE,
                           na.rm = FALSE,
                           show.legend = FALSE,
                           inherit.aes = FALSE) {
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

# Defined here to avoid a note in check --as-cran as the imports from 'broom'
# are not seen when the function is defined in-line in the ggproto object.
#' @rdname ggpp-ggproto
#'
#' @format NULL
#' @usage NULL
#'
gtbnpc_draw_panel_fun <-
  function(data,
           panel_params,
           coord,
           table.theme = NULL,
           table.rownames = FALSE,
           table.colnames = TRUE,
           table.hjust = 0.5,
           parse = FALSE,
           na.rm = FALSE) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (!is.data.frame(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not a list of ",
              "\"tibble\" or \"data.frame\" objects.")
      return(grid::nullGrob())
    }

    data$npcx <- compute_npcx(data$npcx)
    data$npcy <- compute_npcy(data$npcy)

    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$npcy)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$npcx)
    }

    # replace NULL with default
    if (is.null(table.theme)) {
      table.theme <-
        getOption("ggpmisc.ttheme.default", default = ttheme_gtdefault)
    }

    tb.grobs <- grid::gList()

    for (row.idx in seq_len(nrow(data))) {
      # if needed, construct the table theme
      if (is.function(table.theme)) {
        # text position in cell depends on hjust
        table.x <- if(table.hjust == 0.5) 0.5 else table.hjust * 0.8 + 0.1
        if (is.na(data$fill[row.idx])) {
          core.params <-
            list(fg_params = list(hjust = table.hjust, x = table.x))
        } else {
          core.params <-
            list(fg_params = list(hjust = table.hjust, x = table.x),
                 bg_params = list(fill = data$fill[row.idx]))
        }
        if (is.na(data$colour[row.idx])) {
          # use theme's default base_colour
          this.table.theme <-
            table.theme(base_size = data$size[row.idx] * .pt,
                        base_family = data$family[[row.idx]],
                        parse = parse,
                        rowhead = list(fg_params = list(hjust = 1, x = 0.9)),
                        colhead = list(fg_params = list(hjust = table.hjust,
                                                        x = table.x)),
                        core = core.params)
        } else {
          # use colour from data$colour
          this.table.theme <-
            table.theme(base_size = data$size[row.idx] * .pt,
                        base_colour = ggplot2::alpha(data$colour[row.idx],
                                                     data$alpha[row.idx]),
                        base_family = data$family[[row.idx]],
                        parse = parse,
                        rowhead = list(fg_params = list(hjust = 1, x = 0.9)),
                        colhead = list(fg_params = list(hjust = table.hjust,
                                                        x = table.x)),
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
            alpha = 1,
            family = "",
            fontface = 1,
            lineheight = 1.2
          ),

          draw_panel = gtbnpc_draw_panel_fun,

          draw_key = function(...) {
            grid::nullGrob()
          }
  )

#' Table themes
#'
#' Additional theme constructors for use with \code{\link{geom_table}}.
#'
#' @details Depending on the theme, the base_colour, which is
#'   mapped to the \code{colour} aesthetic if present, is applied to only the
#'   text elements, or to the text elements and rules. The difference is
#'   exemplified below.
#'
#' @param base_size numeric, default font size.
#' @param base_colour	default font colour.
#' @param base_family	default font family.
#' @param parse	logical, default behaviour for parsing text as plotmath.
#' @param padding length-2 unit vector specifying the horizontal and vertical
#'   padding of text within each cell.
#' @param ... further arguments to control the gtable.
#'
#' @note These theme constructors are wrappers on
#'   \code{gridExtra::ttheme_default()} and \code{gridExtra::ttheme_minimal()}.
#'   They can also be used with \code{\link[gridExtra]{grid.table}} if desired.
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
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarize(wt = mean(wt), mpg = mean(mpg)) %>%
#'   ungroup() %>%
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
#' # Default colour of theme superceded by aesthetic constant
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
#' # Default colour of theme superceded by aesthetic constant
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
#' # Default colour of theme superceded by aesthetic constant
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point() +
#'   geom_table(data = df, aes(x = x, y = y, label = tb),
#'              table.theme = ttheme_gtlight, colour = "darkred")
#'
#' # Default colour of theme superceded by aesthetic constant
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
ttheme_gtdefault <- function (base_size = 10,
                              base_colour = "black",
                              base_family = "",
                              parse = FALSE,
                              padding = unit(c(0.8, 0.6), "char"),
                              ...)
{
  gridExtra::ttheme_default(base_size = base_size,
                            base_colour = base_colour,
                            base_family = base_family,
                            parse = parse,
                            padding = padding,
                            ...)
}

#' @rdname ttheme_gtdefault
#'
#' @export
#'
ttheme_gtminimal <- function (base_size = 10,
                              base_colour = "black",
                              base_family = "",
                              parse = FALSE,
                              padding = unit(c(0.5, 0.4), "char"),
                              ...)
{
  gridExtra::ttheme_minimal(base_size = base_size,
                            base_colour = base_colour,
                            base_family = base_family,
                            parse = parse,
                            padding = padding,
                            ...)
}

#' @rdname ttheme_gtdefault
#'
#' @export
#'
ttheme_gtbw <- function (base_size = 10,
                         base_colour = "black",
                         base_family = "",
                         parse = FALSE,
                         padding = unit(c(1, 0.6), "char"),
                         ...)
{
  core <-
    list(bg_params = list(fill = "white", lwd = 1.5, col = "grey90"))
  colhead <-
    list(bg_params = list(fill = "grey80", lwd = 1.5, col = "grey90"))
  rowhead <-
    list(bg_params = list(fill = "grey80", lwd = 1.5, col = "grey90"))

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
ttheme_gtplain <- function (base_size = 10,
                            base_colour = "black",
                            base_family = "",
                            parse = FALSE,
                            padding = unit(c(0.8, 0.6), "char"),
                            ...)
{
  core <-
    list(bg_params = list(fill = "white"))
  colhead <-
    list(bg_params = list(fill = "grey90"))
  rowhead <-
    list(bg_params = list(fill = "grey90"))

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
ttheme_gtdark <- function (base_size = 10,
                           base_colour = "grey90",
                           base_family = "",
                           parse = FALSE,
                           padding = unit(c(0.8, 0.6), "char"),
                           ...)
{
  core <-
    list(bg_params = list(fill = "grey30", lwd = 1.5, col = base_colour))
  colhead <-
    list(bg_params = list(fill = "black", lwd = 1.5, col = base_colour))
  rowhead <-
    list(bg_params = list(fill = "black", lwd = 1.5, col = base_colour))

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
ttheme_gtlight <- function (base_size = 10,
                            base_colour = "grey10",
                            base_family = "",
                            parse = FALSE,
                            padding = unit(c(0.8, 0.6), "char"),
                            ...)
{
  core <-
    list(bg_params = list(fill = "white", lwd = 1.5, col = base_colour))
  colhead <-
    list(bg_params = list(fill = "grey80", lwd = 1.5, col = base_colour))
  rowhead <-
    list(bg_params = list(fill = "grey80", lwd = 1.5, col = base_colour))

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
ttheme_gtsimple <- function (base_size = 10,
                            base_colour = "grey10",
                            base_family = "",
                            parse = FALSE,
                            padding = unit(c(0.5, 0.4), "char"),
                            ...)
{
  core <-
    list(bg_params = list(fill = "white", lwd = 0, col = NA))
  colhead <-
    list(bg_params = list(fill = "grey80", lwd = 0, col = NA))
  rowhead <-
    list(bg_params = list(fill = "grey80", lwd = 0, col = NA))

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
ttheme_gtstripes <- function (base_size = 10,
                              base_colour = "grey10",
                              base_family = "",
                              parse = FALSE,
                              padding = unit(c(0.8, 0.6), "char"),
                              ...)
{
  core <-
    list(bg_params = list(fill = c("white", "grey90"), lwd = 0, col = NA))
  colhead <-
    list(bg_params = list(fill = "grey75", lwd = 0, col = NA))
  rowhead <-
    list(bg_params = list(fill = "grey75", lwd = 0, col = NA))

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
#' mtcars %>%
#'   group_by(cyl) %>%
#'   summarize(wt = mean(wt), mpg = mean(mpg)) %>%
#'   ungroup() %>%
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
  stopifnot(is.null(table.theme) ||
              is.function(table.theme) ||
              is.list(table.theme))
  invisible(options(ggpmisc.ttheme.default = table.theme)[[1]])
}

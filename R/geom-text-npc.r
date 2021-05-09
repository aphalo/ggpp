#' Text with Normalised Parent Coordinates
#'
#' `geom_text_npc()` adds text directly to the plot. `geom_label_npc()` draws a
#' rectangle behind the text, making it easier to read. The difference is that x
#' and y mappings are expected to be given in `npc` graphic units. They are
#' intended to be used for positioning text relative to the physical dimensions
#' of a plot. This can be achieved with `annotate()` except when faceting is
#' used.
#'
#' Note that the "width" and "height" of a text element are 0, so stacking and
#' dodging text will not work by default, and axis limits are not automatically
#' expanded to include all text. Obviously, labels do have height and width, but
#' they are physical units, not data units. The amount of space they occupy on
#' the plot is not constant in data units: when you resize a plot, labels stay
#' the same size, but the size of the axes changes.
#'
#' `geom_text_npc()` and `geom_label_npc()` add labels for each row in the data,
#' even if coordinates x, y are set to single values in the call to
#' `geom_label_npc()` or `geom_text_npc()`. To add labels at specified points
#' use [annotate()] with `annotate(geom = "text_npc", ...)` or `annotate(geom =
#' "label_npc", ...)`.
#'
#' @note This geom is identical to 'ggplot2' \code{geom_text()} except that it
#'   interprets \code{x} and \code{y} positions in \code{npc} units. It
#'   translates \code{x} and \code{y} coordinates from npc units to native data
#'   units and calls functions from 'ggplot2''s GeomText().
#'
#' @section `geom_label_npc()`: Currently `geom_label_npc()` does not support
#'   the `angle` aesthetic and is considerably slower than `geom_text_npc()`.
#'   The `fill` aesthetic controls the background colour of the label.
#'
#' @section Alignment: You can modify text alignment with the `vjust` and
#'   `hjust` aesthetics. These can either be a number between 0 (right/bottom)
#'   and 1 (top/left) or a character (`"left"`, `"middle"`, `"right"`,
#'   `"bottom"`, `"center"`, `"top"`). There are two special alignments:
#'   `"inward"` and `"outward"`. Inward always aligns text towards the center,
#'   and outward aligns it away from the center. When using textual positions
#'   a shift is added based on grouping, however unused levels are not dropped.
#'   In plots with faceting so that not all groups appear in each panel, gaps
#'   will appear in between labels. To solve this pass numeric values for
#'   the npc coordinates of each label instead of character strings.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs
#'   to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific data set - only needed if you want to override
#'   the plot defaults.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param parse If TRUE, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param nudge_x,nudge_y Horizontal and vertical adjustment to nudge labels by.
#'   Useful for offsetting text from points, particularly on discrete scales.
#' @param check_overlap If `TRUE`, text that overlaps previous text in the same
#'   layer will not be plotted.
#'
#' @seealso \code{\link[ggplot2]{geom_text}}
#'
#' @rdname geom_text_npc
#' @export
#' @examples
#'
#' df <- data.frame(
#'   x = c(0, 0, 1, 1, 0.5),
#'   x.chr = c("left", "left", "right", "right", "center"),
#'   y = c(0, 1, 0, 1, 0.5),
#'   y.chr = c("bottom", "top", "bottom", "top", "middle"),
#'   text = c("bottom-left", "top-left", "bottom-right", "top-right", "center-middle")
#' )
#' ggplot(df) +
#'   geom_text_npc(aes(npcx = x, npcy = y, label = text))
#'
#' ggplot(df) +
#'   geom_text_npc(aes(npcx = x.chr, npcy = y.chr, label = text))
#'
#' ggplot(data = mtcars, mapping = aes(wt, mpg)) +
#'   geom_point() +
#'   geom_text_npc(data = df, aes(npcx = x, npcy = y, label = text))
#'
#' ggplot(data = mtcars, mapping = aes(wt, mpg)) +
#'   geom_point() +
#'   geom_text_npc(data = df, aes(npcx = x, npcy = y, label = text)) +
#'   expand_limits(y = 40, x = 6)
#'
#' ggplot(data = mtcars) +
#'   geom_point(mapping = aes(wt, mpg)) +
#'   geom_label_npc(data = df, aes(npcx = x, npcy = y, label = text))
#'
geom_text_npc <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      parse = FALSE,
                      nudge_x = 0,
                      nudge_y = 0,
                      check_overlap = FALSE,
                      na.rm = FALSE,
                      show.legend = FALSE,
                      inherit.aes = FALSE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.", call. = FALSE)
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextNpc,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTextNpc <- ggplot2::ggproto("GeomTextNpc", ggplot2::Geom,
  required_aes = c("npcx", "npcy", "label"),

  default_aes = ggplot2::aes(
    colour = "black", size = 3.88, angle = 0, hjust = "inward",
    vjust = "inward", alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE) {

    data$npcx <- compute_npcx(data$npcx)
    data$npcy <- compute_npcy(data$npcy)

    ranges <- coord$backtransform_range(panel_params)

    data$x <- ranges$x[1] + data$npcx * (ranges$x[2] - ranges$x[1])
    data$y <- ranges$y[1] + data$npcy * (ranges$y[2] - ranges$y[1])

    ggplot2::GeomText$draw_panel(data = data,
                                 panel_params = panel_params,
                                 coord = coord,
                                 parse = parse,
                                 na.rm = na.rm,
                                 check_overlap = check_overlap)
  },

  draw_key = function(...) {
    grid::nullGrob()
  }
)

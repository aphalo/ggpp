#' Number of observations in a plot panel
#'
#' \code{stat_panel_counts()} counts the number of observations in each panel.
#' By default it adds a text label to the top right corner of each panel.
#' Grouping is ignored.
#'
#' @param mapping The aesthetic mapping, usually constructed with
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. Only needs to be
#'   set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset. Rarely used, as you will not want to
#'   override the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use on this layer
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes it if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and should not inherit behaviour from the
#'   default plot specification, e.g., \code{\link[ggplot2]{borders}}.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. This
#'   can include aesthetics whose values you want to set, not map. See
#'   \code{\link[ggplot2]{layer}} for more details.
#' @param na.rm	a logical indicating whether \code{NA} values should be stripped
#'   before the computation proceeds.
#' @param label.x,label.y \code{numeric} Coordinates (in npc units) to be used
#'   for absolute positioning of the labels.
#'
#' @details This statistic can be used to automatically count observations in
#'   each panel of a plot, and by default add these counts as text labels. This
#'   statistic, unlike \code{stat_quadrant_counts()} requires only one of
#'   \emph{x} or \emph{y} aesthetics and can be used together with statistics
#'   that have the same requirement, like \code{stat_density()}.
#'
#'   The default position of the label is in the top right corner. When using
#'   facets even with free limits for \emph{x} and \emph{y} axes, the location
#'   of the labels is consistent across panels. This is achieved by use of
#'   \code{geom = "text_npc"} or \code{geom = "label_npc"}. To pass the
#'   positions in native data units, pass \code{geom = "text"} explicitly as
#'   argument. A vector with the same length as the number of panels in the
#'   figure can be used if needed.
#'
#' @section Computed variables: Data frame with one or more rows, one for each
#'   group of observations for which counts are counted in \code{data}. \describe{
#'   \item{npcx}{x value of label position in npc units}
#'   \item{npcy}{y value of label position in npc units}
#'   \item{count}{number of  observations}
#'   \item{x}{x value of label position in data units}
#'   \item{y}{y value of label position in data units}}.
#'
#'   As shown in one example below \code{\link[gginnards]{geom_debug}} can be
#'   used to print the computed values returned by any statistic. The output
#'   shown includes also values mapped to aesthetics, like \code{label} in the
#'   example. \code{x} and \code{y} are included in the output only if mapped.
#'
#' @return A plot layer instance. Using as output \code{data} the counts of
#'   observations per plot quadrant.
#'
#' @family Functions for quadrant and volcano plots
#'
#' @export
#'
#' @examples
#'
#' # generate artificial data
#' set.seed(67821)
#' x <- 1:100
#' y <- rnorm(length(x), mean = 10)
#' my.data <- data.frame(x, y)
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_panel_counts()
#'
#' # We use geom_debug() to see the computed values
#'
#' gginnards.installed <- requireNamespace("gginnards", quietly = TRUE)
#' if (gginnards.installed) {
#'   library(gginnards)
#'
#'   ggplot(my.data, aes(x, y)) +
#'     geom_point() +
#'     stat_panel_counts(geom = "debug")
#' }
#'
#' ggplot(my.data, aes(x, y)) +
#'  geom_point() +
#'  stat_panel_counts(aes(label = sprintf("%i observations", after_stat(count)))) +
#'  expand_limits(y = 12.7)
#'
#' ggplot(my.data, aes(y)) +
#'   stat_panel_counts(label.x = "left") +
#'   stat_density()
#'
stat_panel_counts <- function(mapping = NULL,
                              data = NULL,
                              geom = "text_npc",
                              position = "identity",
                              label.x = "right",
                              label.y = "top",
                              na.rm = FALSE,
                              show.legend = FALSE,
                              inherit.aes = TRUE, ...) {

  stopifnot(is.null(label.x) || is.numeric(label.x) || is.character(label.x))
  stopifnot(is.null(label.y) || is.numeric(label.y) || is.character(label.y))

  ggplot2::layer(
    stat = StatPanelCounts,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  label.x = label.x,
                  label.y = label.y,
                  ...)
  )
}

#' @rdname ggpp-ggproto
#'
#' @format NULL
#' @usage NULL
#'
compute_panel_counts_fun <- function(data,
                                     scales,
                                     label.x,
                                     label.y) {

  force(data)

  # total count
  z <- tibble::tibble(count = nrow(data),
                      npcx = label.x,
                      npcy = label.y)
  if ("x" %in% colnames(data)) {
    z$x <- max(data$x) - 0.95 * diff(range(data$x))
  }
  if ("y" %in% colnames(data)) {
    z$y <- max(data$y) - 0.95 * diff(range(data$y))
  }
  z
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPanelCounts <-
  ggplot2::ggproto("StatPanelCounts", ggplot2::Stat,
                   compute_panel = compute_panel_counts_fun,
                   default_aes =
                     ggplot2::aes(npcx = ggplot2::after_stat(npcx),
                                  npcy = ggplot2::after_stat(npcy),
                                  label = sprintf("n=%i", ggplot2::after_stat(count)),
                                  hjust = "inward",
                                  vjust = "inward"),
                   required_aes = c("x|y")
  )

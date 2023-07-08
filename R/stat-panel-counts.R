#' Number of observations in a plot panel
#'
#' \code{stat_panel_counts()} counts the number of observations in each panel.
#' \code{stat_group_counts()} counts the number of observations in each group.
#' By default they add one or more text labels to the top right corner of each
#' panel. Grouping is ignored by \code{stat_panel_counts()}. If no grouping
#' exists, the two statistics behave similarly.
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
#' @details These statistics can be used to automatically count observations in
#'   each panel of a plot, and by default add these counts as text labels. These
#'   statistics, unlike \code{stat_quadrant_counts()} requires only one of
#'   \emph{x} or \emph{y} aesthetics and can be used together with statistics
#'   that have the same requirement, like \code{stat_density()}.
#'
#'   The default position of the label is in the top right corner. When using
#'   facets even with free limits for \emph{x} and \emph{y} axes, the location
#'   of the labels is consistent across panels. This is achieved by use of
#'   \code{geom = "text_npc"} or \code{geom = "label_npc"}. To pass the
#'   positions in native data units to \code{label.x} and \code{label.y}, pass
#'   also explicitly \code{geom = "text"}, \code{geom = "label"} or some other
#'   geometry that use the \emph{x} and/or \emph{y} aesthetics. A vector with
#'   the same length as the number of panels in the figure can be used if
#'   needed.
#'
#' @section Computed variables: Data frame with one or more rows, one for each
#'   group of observations for which counts are counted in \code{data}. \describe{
#'   \item{x,npcx}{x value of label position in data- or npc units, respectively}
#'   \item{y,npcy}{y value of label position in data- or npc units, respectively}
#'   \item{count}{number of  observations as an integer}
#'   \item{count.label}{number of observations as character}}
#'
#'   As shown in one example below \code{\link[gginnards]{geom_debug}} can be
#'   used to print the computed values returned by any statistic. The output
#'   shown includes also values mapped to aesthetics, like \code{label} in the
#'   example. \code{x} and \code{y} are included in the output only if mapped.
#'
#' @note  If a factor is mapped to \code{x} or to \code{y} aesthetics each level
#'   of the factor constitutes a group, in this case the default positioning and
#'   geom using NPC pseudo aesthetics will have to be overriden by passing
#'   \code{geom = "text"} and data coordinates used. The default for factors
#'   may change in the future.
#'
#' @return A plot layer instance. Using as output \code{data} the counts of
#'   observations in each plot panel or per group in each plot panel.
#'
#' @family Functions for quadrant and volcano plots
#'
#' @export
#'
#' @examples
#'
#' # generate artificial data with numeric x and y
#' set.seed(67821)
#' x <- 1:100
#' y <- rnorm(length(x), mean = 10)
#' group <- factor(rep(c("A", "B"), times = 50))
#' my.data <- data.frame(x, y, group)
#'
#' # using automatically generated text labels
#'
#' ggplot(my.data, aes(x, y)) +
#'   geom_point() +
#'   stat_panel_counts()
#'
#' ggplot(my.data, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_panel_counts()
#'
#' ggplot(my.data, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_group_counts()
#'
#' ggplot(my.data, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_group_counts(label.x = "left", hstep = 0.06, vstep = 0)
#'
#' ggplot(my.data, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_group_counts(aes(label = after_stat(pc.label)))
#'
#' ggplot(my.data, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_group_counts(aes(label = after_stat(pc.label)), digits = 3)
#'
#' ggplot(my.data, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_group_counts(aes(label = after_stat(fr.label)))
#'
#' ggplot(my.data, aes(x, y, colour = group)) +
#'   geom_point() +
#'   stat_group_counts(aes(label = after_stat(dec.label)))
#'
#' # one of x or y can be a factor
#'
#' ggplot(mpg,
#'        aes(factor(cyl), hwy)) +
#'   stat_boxplot() +
#'   stat_group_counts(geom = "text",
#'                     label.y = 10,
#'                     label.x = 1:4) +
#'   stat_panel_counts()
#'
#' # Numeric values can be used to build labels with alternative formats
#' # Here with sprintf(), but paste() and format() also work.
#'
#' ggplot(my.data, aes(x, y)) +
#'  geom_point() +
#'  stat_panel_counts(aes(label = sprintf("%i observations",
#'                                        after_stat(count)))) +
#'  scale_y_continuous(expand = expansion(mult = c(0.05, 0.12)))
#'
#' ggplot(mpg,
#'        aes(factor(cyl), hwy)) +
#'   stat_boxplot() +
#'   stat_group_counts(geom = "text",
#'                     aes(label = sprintf("(%i)", after_stat(count))),
#'                     label.y = 10,
#'                     label.x = 1:4)
#'
#' ggplot(mpg,
#'        aes(factor(cyl), hwy)) +
#'   stat_boxplot() +
#'   stat_group_counts(aes(label = sprintf("n[%i]~`=`~%i",
#'                                         after_stat(x), after_stat(count))),
#'                     parse = TRUE,
#'                     geom = "text",
#'                     label.y = 10,
#'                     label.x = 1:4) +
#'   stat_panel_counts(aes(label = sprintf("sum(n[i])~`=`~%i",
#'                                         after_stat(count))),
#'                     parse = TRUE)
#'
#' # label position
#'
#' ggplot(my.data, aes(y)) +
#'   stat_panel_counts(label.x = "left") +
#'   stat_density(alpha = 0.5)
#'
#' ggplot(my.data, aes(y, colour = group)) +
#'   stat_group_counts(label.y = "top") +
#'   stat_density(aes(fill = group), alpha = 0.3)
#'
#' # The numeric value can be used as a label as is
#'
#' ggplot(mpg,
#'        aes(factor(cyl), hwy)) +
#'   stat_boxplot() +
#'   stat_group_counts(geom = "text",
#'                     aes(label = after_stat(count)),
#'                     label.x = 1:4,
#'                     label.y = 10) +
#'   annotate(geom = "text", x = 0.55, y = 10, label = "n[i]~`=`", parse = TRUE)
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
#' if (gginnards.installed) {
#'   ggplot(my.data, aes(x, y, colour = group)) +
#'     geom_point() +
#'     stat_group_counts(geom = "debug")
#' }
#'
stat_panel_counts <- function(mapping = NULL,
                              data = NULL,
                              geom = "text_npc",
                              position = "identity",
                              label.x = "right",
                              label.y = "top",
                              na.rm = FALSE,
                              show.legend = FALSE,
                              inherit.aes = TRUE,
                              ...) {

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
                  npc.used = grepl("_npc", geom),
                  ...)
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatPanelCounts <-
  ggplot2::ggproto("StatPanelCounts", ggplot2::Stat,

                   compute_panel = function(data,
                                            scales,
                                            label.x,
                                            label.y,
                                            npc.used) {

                     force(data)
                     # total count (works because NAs have been already removed)
                     z <- tibble::tibble(count = nrow(data))
                     # label position
                     if (is.character(label.x)) {
                       if (npc.used) {
                         margin.npc <- 0.05
                       } else {
                         # margin set by scale
                         margin.npc <- 0
                       }
                       label.x <- compute_npcx(x = label.x)
                       if (!npc.used) {
                         if ("x" %in% colnames(data)) {
                           x.expanse <- abs(diff(range(data$x)))
                           x.min <- min(data$x)
                           label.x <- label.x * x.expanse + x.min
                         } else {
                           if (data$PANEL[1] == 1L) { # show only once
                             message("No 'x' mapping; 'label.x' requires a numeric argument in data units")
                           }
                           label.x <- NA_real_
                         }
                       }
                     }
                     if (is.character(label.y)) {
                       if (npc.used) {
                         margin.npc <- 0.05
                       } else {
                         # margin set by scale
                         margin.npc <- 0
                       }
                       label.y <- compute_npcy(y = label.y)
                       if (!npc.used) {
                         if ("y" %in% colnames(data)) {
                           y.expanse <- abs(diff(range(data$y)))
                           y.min <- min(data$y)
                           label.y <- label.y * y.expanse + y.min
                         } else {
                           if (data$PANEL[1] == 1L) { # show only once
                             message("No 'y' mapping; 'label.y' requires a numeric argument in data units")
                           }
                           label.y <- NA_real_
                         }
                       }
                     }

                     if (npc.used) {
                       z$npcx <- label.x
                       z$x <- NA_real_
                       z$npcy <- label.y
                       z$y <- NA_real_
                     } else {
                       z$x <- label.x
                       z$npcx <- NA_real_
                       z$y <- label.y
                       z$npcy <- NA_real_
                     }
                     z$count.label <- sprintf("n=%i", z$count)
                     z
                   },

                   default_aes =
                     ggplot2::aes(npcx = ggplot2::after_stat(npcx),
                                  npcy = ggplot2::after_stat(npcy),
                                  label = ggplot2::after_stat(count.label),
                                  hjust = "inward",
                                  vjust = "inward"),
                   required_aes = c("x|y")
  )


#' @rdname stat_panel_counts
#'
#' @param hstep,vstep numeric in npc units, the horizontal and vertical step
#'   used between labels for different groups.
#' @param digits integer Number of digits for fraction and percent labels.
#'
#' @export
#'
stat_group_counts <- function(mapping = NULL,
                              data = NULL,
                              geom = "text_npc",
                              position = "identity",
                              label.x = "right",
                              label.y = "top",
                              hstep = 0,
                              vstep = NULL,
                              digits = 2,
                              na.rm = FALSE,
                              show.legend = FALSE,
                              inherit.aes = TRUE,
                              ...) {

  stopifnot(is.null(label.x) || is.numeric(label.x) || is.character(label.x))
  stopifnot(is.null(label.y) || is.numeric(label.y) || is.character(label.y))

  ggplot2::layer(
    stat = StatGroupCounts,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  label.x = label.x,
                  label.y = label.y,
                  digits = digits,
                  hstep = hstep,
                  vstep = ifelse(is.null(vstep),
                                 ifelse(grepl("label", geom),
                                        0.10,
                                        0.05),
                                 vstep),
                  npc.used = grepl("_npc", geom),
                  ...)
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatGroupCounts <-
  ggplot2::ggproto("StatGroupCounts", ggplot2::Stat,

                   compute_panel = function(data,
                                            params,
                                            scales,
                                            label.x,
                                            label.y,
                                            digits = 2,
                                            vstep,
                                            hstep,
                                            npc.used) {

                     force(data)

                     if (exists("grp.label", data)) {
                       if (length(unique(data[["grp.label"]])) > 1L) {
                         warning("Non-unique value in 'data$grp.label' using group index ", data[["group"]][1], " as label.")
                         grp.label <- as.character(unique(data[["group"]]))
                       } else {
                         grp.label <- unique(data[["grp.label"]])
                       }
                     } else {
                       # if nothing mapped to grp.label we use group index as label
                       grp.label <- as.character(unique(data[["group"]]))
                     }

                     # Build group labels
                     group.idx <- abs(unique(data$group))
                     if (length(label.x) >= length(group.idx)) {
                       label.x <- label.x[1:length(group.idx)]
                     } else if (length(label.x) > 0) {
                       label.x <- label.x[1]
                     }
                     if (length(label.y) >= length(group.idx)) {
                       label.y <- label.y[1:length(group.idx)]
                     } else if (length(label.y) > 0) {
                       label.y <- label.y[1]
                     }

                     # Compute number of observations
                     # (works because NAs have been already removed)
                     num.obs <- nrow(data)

                     data %>%
                       dplyr::group_by(.data$group) %>%
                       dplyr::summarise(count = length(.data$x)) %>% # dplyr::n() triggers error
                       dplyr::ungroup() -> z

                     z$total <- num.obs

                     y <- data.frame()
                     cols2copy <- setdiff(colnames(data), colnames(z))
                     for (g in unique(data$group)) {
                       y <- rbind(y, data[data$group == g, cols2copy][ 1, ])
                     }

                     z <- cbind(z, y)

                     z$count.label <- sprintf("n=%i", z$count)
                     z$pc.label <- sprintf("p=%.*f%%",
                                              digits - 2,
                                              z$count / sum(z$count) * 100)
                     z$dec.label <- sprintf("f=%.*f",
                                              digits,
                                              z$count / sum(z$count))
                     z$fr.label <- sprintf("%i / %i",
                                           z$count, z$total)

                     # Compute label positions
                     if (is.character(label.x)) {
                       if (npc.used) {
                         margin.npc <- 0.05
                       } else {
                         # margin set by scale
                         margin.npc <- 0
                       }
                       label.x <- compute_npcx(x = label.x, group = group.idx, h.step = hstep,
                                               margin.npc = margin.npc)
                       if (!npc.used) {
                         if ("x" %in% colnames(data)) {
                           x.expanse <- abs(diff(range(data$x)))
                           x.min <- min(data$x)
                           label.x <- label.x * x.expanse + x.min
                         } else {
                           if (data$PANEL[1] == 1L && group.idx == 1L) { # show only once
                             message("No 'x' mapping; 'label.x' requires a numeric argument in data units")
                           }
                           label.x <- NA_real_
                         }
                       }
                     }
                     if (is.character(label.y)) {
                       if (npc.used) {
                         margin.npc <- 0.05
                       } else {
                         # margin set by scale
                         margin.npc <- 0
                       }
                       label.y <- compute_npcy(y = label.y, group = group.idx, v.step = vstep,
                                               margin.npc = margin.npc)
                       if (!npc.used) {
                         if ("y" %in% colnames(data)) {
                           y.expanse <- abs(diff(range(data$y)))
                           y.min <- min(data$y)
                           label.y <- label.y * y.expanse + y.min
                         } else {
                           if (data$PANEL[1] == 1L && group.idx == 1L) { # show only once
                             message("No 'y' mapping; 'label.y' requires a numeric argument in data units")
                           }
                           label.y <- NA_real_
                         }
                       }
                     }

                     if (npc.used) {
                       z$npcx <- label.x
                       z$x <- NA_real_
                       z$npcy <- label.y
                       z$y <- NA_real_
                     } else {
                       z$x <- label.x
                       z$npcx <- NA_real_
                       z$y <- label.y
                       z$npcy <- NA_real_
                     }
                     z$count.label <- sprintf("n=%i", z$count)
                     z
                   },

                   default_aes =
                     ggplot2::aes(npcx = ggplot2::after_stat(npcx),
                                  npcy = ggplot2::after_stat(npcy),
                                  label = ggplot2::after_stat(count.label),
                                  hjust = "inward",
                                  vjust = "inward"),
                   required_aes = c("x|y")
  )

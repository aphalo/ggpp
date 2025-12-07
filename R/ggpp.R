#' @details Package 'ggpp' provides functions that extend the grammar of
#' graphics as implemented in 'ggplot2'. It attempts to stay true to the
#' original grammar and to respect the naming conventions used in 'ggplot2'.
#'
#' Extensions provided:
#' \itemize{
#' \item Geoms adding support for plot, table and grob insets within the
#' gramamr. Geoms using a parallel pseudo-scale based on native plot coordinates
#' (npc) to allow annotations consistent with the grammar and so supporting
#' facets and grouping. Geoms for annotations on the edges of the plotting
#' area. Geom for easily drawing lines separating the quadrants of a plot.
#' \item Stats for filtering-out/filtering-in observations in regions of a
#' panel or group where the density of observations is high. Statistics
#' simultaneously computing summaries, optionally using different functions,
#' along x and y. Stat computing quadrant counts.
#' \item Position functions implementing multi-directional nudging based on the
#' data.
#' \item Scales. Pseudo-scales supporting npc coordinates for x and y.
#' \item Specializations of the \code{ggplot()} generic accepting time series
#' objects of classes \code{ts} and \code{xts} as data argument.
#' }
#'
#' @section Acknowledgements: We thank Kamil Slowikowski not only for
#'   contributing ideas and code examples to this package but also for adding
#'   new features to his package 'ggrepel' that allow new use cases for
#'   \code{stat_dens2d_labels()}, \code{position_nudge_center()},
#'   \code{position_nudge_line()} and \code{position_nudge_to()} from this
#'   package. This package includes code copied and/or modified from
#'   that in package 'ggplot2'.
#'
#' @references
#' Package 'ggplot2' documentation is available at
#' \url{https://ggplot2.tidyverse.org/}\cr Package 'ggplot2' source code at
#' \url{https://github.com/tidyverse/ggplot2}
#'
#' @import scales grid ggplot2
#' @importFrom ggplot2 ggplot
#' @importFrom rlang .data
#'
#' @importFrom dplyr mutate
#' @importFrom polynom polynomial
#' @importFrom MASS kde2d
#'
"_PACKAGE"

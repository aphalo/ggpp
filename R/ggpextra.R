#' @details The new facilities for cleanly defining new stats and geoms added to
#' 'ggplot2' in version 2.0.0 and the support for nested tibbles and new syntax
#' for mapping computed values to aesthetics added to
#' 'ggplot2' in version 3.0.0 are used in this package's code. This
#' means that 'ggpmisc' (>= 0.3.0) requires version 3.0.0 or later of ggplot2.
#'
#' Extensions provided:
#' \itemize{
#' \item Stats for filtering-out/filtering-in observations in regions of a
#' panel or group where the density of observations is high.
#' \item Geoms.
#' \item Position functions.
#' \item Scales.
#' }
#'
#' @section Acknowledgements: We thank Kamil Slowikowski not only for
#'   contributing ideas and code examples to this package but also for adding
#'   new features to his package 'ggrepel' that allow new use cases for
#'   \code{stat_dens2d_labels} from this package.
#'
#' @references Package suite 'r4photobiology' web site at
#' \url{https://www.r4photobiology.info/}\cr Package 'ggplot2' documentation at
#' \url{https://ggplot2.tidyverse.org/}\cr Package 'ggplot2' source code at
#' \url{https://github.com/tidyverse/ggplot2}
#'
#' @import scales grid ggplot2
#' @importFrom ggplot2 ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @note The signatures of \code{stat_peaks()} and \code{stat_valleys()} are
#'   identical to those of \code{stat_peaks} and \code{stat_valleys} from
#'   package \code{photobiology} but the variables returned are a subset as
#'   values related to light spectra are missing. Furthermore the stats from
#'   package \code{ggpmisc} work correctly when the x aesthetic uses a date or
#'   datetime scale, while those from package \code{photobiology} do not
#'   generate correct labels in this case.
#'
#' @examples
#' # library(tibble)
#'
"_PACKAGE"

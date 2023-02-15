#' Chose between dark and light color
#'
#' Chose between a pair of contrasting dark and light colors based on a weighted
#' mean of RGB channels of a color. This function implements a simple approach
#' to the choice for a color of a plot element to ensure it is visible against a
#' background color.
#'
#' @param colors A vector of color definitions or color names in the background.
#' @param threshold numeric A value of luminance in [0..1] indicating the switch
#'   point between dark and light background.
#' @param dark.color,light.color A color definition or color name to return as
#'   dark and light colors to contrast light and dark backgrounds respectively.
#'
#' @details The switch between dark and light color is based on a quick and
#'   dirty approximation of the luminance of colors computed from RGB values.
#'   This easily computed approximation seems to work well enough. The default
#'   threshold chosen for a switch between black and white may need to be
#'   adjusted for other pairs of colors. Graphic devices can differ in the color
#'   spaces they support, but this is unlikely to affect the choice between
#'   black and white or other pairs of colors with large differences in
#'   luminance.
#'
#' @note The current implementation of \code{dark_or_light()} ignores
#'   \code{alpha}, the transparency component, of all its arguments.
#'
#' @export
#'
#' @examples
#'
#' dark_or_light("yellow")
#' dark_or_light("darkblue")
#' dark_or_light(c("darkblue", "yellow", "red"))
#' dark_or_light("#FFFFFF")
#' dark_or_light("#FFFFFF", dark.color = "darkblue", light.color = "lightgrey")
#' dark_or_light("#000000", dark.color = "darkblue", light.color = "lightgrey")
#'
dark_or_light <- function(colors,
                          threshold = 0.45,
                          dark.color = "black",
                          light.color = "white")
{
  if (!length(colors))
    return(character())
  stopifnot(length(threshold) == 1L && threshold >= 0 && threshold <= 1)
  threshold <- trunc(threshold * 255)
  # approximate luminance in 0..255
  lum <- sapply(colors, function(x) {
    y <- grDevices::col2rgb(x)
    sum(y * c(1.5, 2.5, 1))/5},
    USE.NAMES = FALSE)
  out <- rep_len(dark.color, length.out = length(colors))
  out[lum <= threshold] <- light.color
  out
}

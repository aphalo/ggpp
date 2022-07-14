#' Chose dark between a light color
#'
#' Chose between a pair of contrasting dark and light colors based on a weighted mean
#' of RGB channels of a color. This function implements a simple approach to the
#' choice for a color of a plot element to ensure it is visible against a background color.
#'
#' @param colors A vector of color definitions or color names in the background.
#' @param threshold numeric A value in [0..1] indicating the switch point between
#'   dark and light background.
#' @param dark.color,light.color A color definition or color name to return for
#'   dark and light objects (orveplotted on light and dark backgrounds, respectively).
#'
#' @export
#'
#' @examples
#'
#' dark_or_light("yellow")
#' dark_or_light("darkblue")
#' dark_or_light("#FFFFFF")
#' dark_or_light("#FFFFFF", dark.color = "darkblue", light.color = "lightgrey")
#' dark_or_light("#000000", dark.color = "darkblue", light.color = "lightgrey")
#'
dark_or_light <- function (colors,
                           threshold = 0.45,
                           dark.color = "black",
                           light.color = "white")
{
  if (!length(colors))
    return(character())
  threshold <- trunc(threshold * 255)
  lum <- function(colors) {
    sapply(colors, function(x) {
      y <- grDevices::col2rgb(x)
      sum(y * c(1.5, 2.5, 1))/5
    }, USE.NAMES = FALSE)
  }
  ifelse(lum(colors) > threshold, dark.color, light.color)
}

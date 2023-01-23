#' Compute npc coordinates
#'
#' Convert character-encoded positions to npc units and shift positions to
#' avoid overlaps when grouping is active. If numeric, validate npc values.
#'
#' @details These functions use NPC (normalized plot coordinates) instead of
#'   data coordinates. They translate named positions into numeric values in
#'   [0..1] and they can also shift the position according to the group, e.g.,
#'   for each increase in the group number displace the position inwards or
#'   outwards, by a user-supplied distance. They make it possible to set
#'   automatically set default positions for grouped text labels.
#'
#'   Out of bounds numeric values are constrained to [0..1]. Unrecognized
#'   character values are silently converted into \code{NA_integer_}.
#'
#' @note These functions are used by several layer functions in packages
#'   'ggpp' and 'ggpmisc', and can be useful to developers of other 'ggplot2'
#'   extensions.
#'
#' @param x numeric or if character, one of "right", "left", "centre",
#'  "center" or "middle".
#' @param y numeric or if character, one of "top", "bottom", "centre",
#'  "center" or "middle".
#' @param group integer vector, ggplot's group id. Used to shift coordinates to
#'   avoid overlaps.
#' @param h.step,v.step numeric [0..1] The step size for shifting coordinates
#'   in npc units. Usually << 1.
#' @param margin.npc numeric [0..1] The margin added towards the nearest
#'   plotting area edge when converting character coordinates into npc. Usually
#'   << 1.
#' @param each.len integer The number of steps per group.
#'
#' @return A numeric vector with values in the range [0..1] representing
#'   npc coordinates.
#'
#' @examples
#' compute_npcx("right")
#' compute_npcx(c("left", "right"))
#' compute_npcy("bottom")
#' compute_npcy("bottom", group = 1L:3L)
#' compute_npcy("bottom", group = 2L)
#' compute_npcx(0.5)
#' compute_npcx(1)
#'
#' @export
#'
compute_npcx <- function(x,
                         group = 1L,
                         h.step = 0.1,
                         margin.npc = 0.05,
                         each.len = 1) {
  group <- abs(group)
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x)) {
    # we must handle character vectors with length > 1
    map <- c(right = 1 - margin.npc,
             left = 0 + margin.npc,
             centre = 0.5,
             center = 0.5,
             middle = 0.5,
             NA_real_)
    x <- unname(map[x])
  }
  expanded.group <- integer()
  for (i in seq_along(group)) {
    temp <- seq(from = 1, by = 1, length.out = each.len) +
      (group[i] - 1) * each.len
    expanded.group <- c(expanded.group, temp)
  }
  if (any(expanded.group > 0L) && h.step != 0) {
    x <- x + (expanded.group - 1) * h.step * ifelse(x < 0.5, 1, -1)
  }
  x <- ifelse(x > 1, 1, x)
  x <- ifelse(x < 0, 0, x)
  x
}

#' @rdname compute_npcx
#'
#' @export
#'
compute_npcy <- function(y,
                         group = 1L,
                         v.step = 0.1,
                         margin.npc = 0.05,
                         each.len = 1) {
  group <- abs(group)
  if (is.factor(y)) {
    y <- as.character(y)
  }
  if (is.character(y)) {
    # we must handle character vectors with length > 1
    map <- c(top = 1 - margin.npc,
             bottom = 0 + margin.npc,
             centre = 0.5,
             center = 0.5,
             middle = 0.5,
             NA_real_)
    y <- unname(map[y])
  }
  expanded.group <- integer()
  for (i in seq_along(group)) {
    temp <- seq(from = 1, by = 1, length.out = each.len) +
      (group[i] - 1) * each.len
    expanded.group <- c(expanded.group, temp)
  }
  if (any(expanded.group > 1L) && v.step != 0) {
    y <- y + (expanded.group - 1L) * v.step * ifelse(y < 0.5, 1, -1)
  }
  y <- ifelse(y > 1, 1, y)
  y <- ifelse(y < 0, 0, y)
  y
}

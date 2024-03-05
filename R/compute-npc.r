#' Compute NPC coordinates
#'
#' Translate and/or compute NPC (Normalised Parent Coordinates) for use with
#' aesthetics \code{x} and \code{y}.
#'
#' @details Functions \code{compute_npcx} and \code{compute_npcy} convert
#'   character-encoded positions to npc units and shift positions to avoid
#'   overlaps when grouping is active. If numeric, they validate the npc values.
#'   Function \code{compute_npcx} does the translation either for both \code{x}
#'   and \code{y} aesthetics, but does not implement a shift for grpups.
#'   Functions \code{as_npcx()},  \code{as_npcy()} and \code{as_npc()} are
#'   wrappers on these functions that return the value as objects of class
#'   \code{"AsIs"} so that in 'ggplot2' >= 3.5.0 they can be used with any layer
#'   function.
#'
#'   These functions use NPC (Normalised Parent Coordinates) instead of data
#'   coordinates. They translate named positions into numeric values in [0..1]
#'   and they can also shift the position according to the group, e.g., for each
#'   increase in the group number displace the position inwards or outwards, by
#'   a user-supplied distance. They make it possible to set automatically set
#'   default positions for grouped text labels.
#'
#'   Out of bounds numeric values are constrained to [0..1]. Unrecognized
#'   character values are silently converted into \code{NA_integer_}.
#'
#' @note The \emph{as_npc()} functions make it easier the use of NPC coordinates
#'   with 'ggplot2' >= 3.5.0. The _compute_ functions are used by several layer
#'   functions in packages 'ggpp' and 'ggpmisc', are compatible with 'ggplot2'
#'   <= 3.4.4 and can be useful to developers of other 'ggplot2' extensions.
#'
#' @param x numeric or if character, one of "right", "left", "maximum",
#'   "minimum", "centre", "center" or "middle".
#' @param y numeric or if character, one of "top", "bottom", "maximum",
#'   "minimum", "centre", "center" or "middle".
#' @param a numeric or if character, one of "right", "left", "top", "maximum",
#'   "minimum", "bottom", "centre", "center" or "middle".
#' @param group integer vector, ggplot's group id. Used to shift coordinates to
#'   avoid overlaps.
#' @param h.step,v.step numeric [0..1] The step size for shifting coordinates
#'   in npc units. Usually << 1.
#' @param margin.npc numeric [0..1] The margin added towards the nearest
#'   plotting area edge when converting character coordinates into npc. Usually
#'   << 1.
#' @param each.len integer The number of steps per group.
#' @param ... named arguments passed to \code{compute_npcx()} or
#'   \code{compute_npcy()}.
#'
#' @return A numeric vector with values in the range [0..1] representing
#'   npc coordinates.
#'
#' @examples
#' compute_npcx("right")
#' compute_npcx(c("left", "right"))
#' compute_npcx(c("minimum", "maximum"))
#' compute_npcx(c("left", "right"), margin.npc = 0)
#' compute_npcy("bottom")
#' compute_npcy("bottom", group = 1L:3L)
#' compute_npcy("bottom", group = 1L:3L, v.step = 0.2)
#' compute_npcy("bottom", group = 2L)
#' compute_npcx(0.5)
#' compute_npcx(1)
#' compute_npcx(-2)
#'
#' as_npc("right")
#' class(as_npc("right"))
#' class(compute_npcx("right"))
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
             maximum = 1,
             minimum = 0,
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
             maximum = 1,
             minimum = 0,
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

#' @rdname compute_npcx
#'
#' @export
#'
as_npcx <- function(x, ...) {
  I(compute_npcx(x = x, ...))
}

#' @rdname compute_npcx
#'
#' @export
#'
as_npcy <- function(y, ...) {
  I(compute_npcy(y = y, ...))
}

#' @rdname compute_npcx
#'
#' @export
#'
compute_npc <- function(a,
                        margin.npc = 0.05) {
  if (is.factor(a)) {
    a <- as.character(a)
  }
  if (is.character(a)) {
    # we must handle character vectors with length > 1
    map <- c(top = 1 - margin.npc,
             bottom = 0 + margin.npc,
             right = 1 - margin.npc,
             left = 0 + margin.npc,
             maximum = 1,
             minimum = 0,
             centre = 0.5,
             center = 0.5,
             middle = 0.5,
             NA_real_)
    a <- unname(map[a])
  } else if (is.numeric(a)) {
    a <- ifelse(a > 1, 1, a)
    a <- ifelse(a < 0, 0, a)
    a
  }
  a
}

#' @rdname compute_npcx
#'
#' @export
#'
as_npc <- function(a,
                   margin.npc = 0.05) {
  I(compute_npc(a = a, margin.npc = margin.npc))
}

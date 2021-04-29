#' Compute npc coordinates
#'
#' Convert character-encoded positions to npc units and shift postions to
#' avoid overlaps when grouping is active. If numeric validate npc values.
#'
#' @param x,y numeric or character vector of coordinates.
#' @param group integer ggplot's group id. Used to shift coordinates to avoid
#'   overlaps.
#' @param h.step,v.step numeric [0..1] The step size for shifting coordinates
#'   in npc units.
#' @param margin.npc numeric [0..1] The margin added towards the nearest
#'   plotting area edge when converting character coordinates into npc.
#'
#' @return A numeric vector with values in the range [0..1] representing
#'   npc coordinates.
#'
#' @keywords internal
#'
#' @export
#'
compute_npcx <- function(x, group = 1L, h.step = 0.1, margin.npc = 0.05) {
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
  if (any(group > 1L) && h.step != 0) {
    x <- x + (group - 1L) * h.step * ifelse(x < 0.5, 1, -1)
  }
  x <- ifelse(x > 1, 1, x)
  x <- ifelse(x < 0, 0, x)
  x
}

#' @rdname compute_npcx
#'
#' @export
#'
compute_npcy <- function(y, group = 1L, v.step = 0.1, margin.npc = 0.05) {
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
  if (any(group > 1L) && v.step != 0) {
    y <- y + (group - 1L) * v.step * ifelse(y < 0.5, 1, -1)
  }
  y <- ifelse(y > 1, 1, y)
  y <- ifelse(y < 0, 0, y)
  y
}

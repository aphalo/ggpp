context("annotate")
# tests copied from 'ggplot2' to ensure that pass through is working as expected

# Used in annotations to ensure printed even when no
# global data
# copied from ggplot2's utilities.r
dummy_data <- function() new_data_frame(list(x = NA), n = 1)

# Fast data.frame constructor and indexing
# No checking, recycling etc. unless asked for
# copied from ggplot2's performance.r
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

test_that("dates in segment annotation work", {
  dt <- structure(list(month = structure(c(1364774400, 1377993600),
                                         class = c("POSIXct", "POSIXt"), tzone = "UTC"), total = c(-10.3,
                                                                                                   11.7)), .Names = c("month", "total"), row.names = c(NA, -2L), class =
                    "data.frame")

  p <- ggplot(dt, aes(month, total)) +
    geom_point() +
    annotate("segment",
             x = as.POSIXct("2013-04-01"),
             xend = as.POSIXct("2013-07-01"),
             y = -10,
             yend = 10
    )

  expect_true(all(c("xend", "yend") %in% names(layer_data(p, 2))))
})

test_that("segment annotations transform with scales", {
  # Line should match data points
  df <- tibble::tibble(x = c(1, 10), y = c(10, 1))
  plot <- ggplot(df, aes(x, y)) +
    geom_point() +
    annotate("segment", x = 1, y = 10, xend = 10, yend = 1, colour = "red") +
    scale_y_reverse(NULL, breaks = NULL) +
    scale_x_continuous(NULL, breaks = NULL)

  vdiffr::expect_doppelganger("line matches points", plot)
})

test_that("annotation_* has dummy data assigned and don't inherit aes", {
  custom <- annotation_custom(zeroGrob())
  logtick <- annotation_logticks()
  library(maps)
  usamap <- map_data("state")
  map <- annotation_map(usamap)
  rainbow <- matrix(hcl(seq(0, 360, length.out = 50 * 50), 80, 70), nrow = 50)
  raster <- annotation_raster(rainbow, 15, 20, 3, 4)
  dummy <- dummy_data()
  expect_equal(custom$data, dummy)
  expect_equal(logtick$data, dummy)
  expect_equal(map$data, dummy)
  expect_equal(raster$data, dummy)

  expect_false(custom$inherit.aes)
  expect_false(logtick$inherit.aes)
  expect_false(map$inherit.aes)
  expect_false(raster$inherit.aes)
})

context("ggpp_annotate")

test_that("ggpp::annotate works with npc pseudo-aesthetics", {

p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()

vdiffr::expect_doppelganger("annotate_label_npc1",
                            p + annotate("label_npc", npcx = c(0.1, 0.9), npcy = c(0.1, 0.9),
                                         label = c("A", "B"))
                            )

vdiffr::expect_doppelganger("annotate_text_npc1",
                            p + annotate("text_npc", npcx = 0.9, npcy = 0.9, label = "Some text")
)

vdiffr::expect_doppelganger("annotate_text_npc2",
                            p + annotate("text_npc", npcx = "right", npcy = "top", label = "Some text")
)

vdiffr::expect_doppelganger("annotate_table_npc1",
                            p + annotate("table_npc", npcx = 0.9, npcy = 0.9,
                                         label = data.frame(A = 1:2, B = letters[1:2]))
)

vdiffr::expect_doppelganger("annotate_plot_npc1",
                            p + annotate("plot_npc", npcx = 1, npcy = 1,
                                         label = p + theme_bw(9))
)

vdiffr::expect_doppelganger("annotate_plot_npc2",
                            p + annotate("plot_npc", npcx = c(0, 1), npcy = c(0, 1),
                                         label = list(p + theme_bw(9), p + theme_grey(9)),
                                         vp.width = 0.3, vp.height = 0.4)
)

vdiffr::expect_doppelganger("annotate_text",
                            p + annotate("text", x = c(2, 5), y = c(15, 32),
                                         label = c("A", "B"))
)

vdiffr::expect_doppelganger("annotate_label",
                            p + annotate("label", x = c(2, 5), y = c(15, 32),
                                         label = c("A", "B"))
)

vdiffr::expect_doppelganger("annotate_table",
                            p + annotate("table", x = 5, y = 30,
                                         label = data.frame(A = 1:2, B = letters[1:2]))
)

vdiffr::expect_doppelganger("annotate_plot",
                            p + annotate("plot", x = 5.5, y = 34,
                                         label = p + theme_bw(9))
)

})

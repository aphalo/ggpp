context("sf compatibility")

test_that("examples_geom_plot_sf", {

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p1 <- ggplot(nc) +
    geom_sf(aes(fill = AREA))

  p <-
    ggplot(data = mtcars, mapping = aes(wt, mpg)) +
    geom_point()

  df1 <- tibble(x = 0.01, y = 0.01,
                plot = list(p1 +
                              labs(x = NULL, y = NULL) +
                              theme(legend.position = "none")))

  # wrongly positioned, the inset is "seen" as larger than it is along latitudes
  vdiffr::expect_doppelganger("geom_plot_npc_sf1",
                              p +
                                expand_limits(x = 0, y = 0) +
                                geom_plot_npc(data = df1,
                                              aes(npcx = x, npcy = y, label = plot))
  )

  # wrongly positioned, the inset is "seen" as larger than it is along latitudes
  vdiffr::expect_doppelganger("geom_plot_npc_sf2",
                              p +
                                expand_limits(x = 0, y = 0) +
                                geom_plot_npc(data = df1,
                                              aes(npcx = x, npcy = y, label = plot),
                                              angle = 90, vjust = 1)
  )
})

context("annotate maps")

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


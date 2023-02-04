context("geom_plot")

library(tibble)

test_that("data.frame", {
   my.df <- data.frame(x = 1:10, y = 1:10, tb = letters[1:10])
   expect_error(print(ggplot(my.df, aes(x, y, label = tb)) +
                  geom_plot()))
})

test_that("multiple_ggplot", {
  p1 <- ggplot(mpg, aes(displ, cty)) + geom_point()
  p2 <- ggplot(mpg, aes(displ, hwy)) + geom_point()
  p3 <- ggplot(mpg, aes(cyl, displ)) + geom_point()
  my.tb <- tibble(x = 0:2, y = 0:2, tb = list(p1, p2, p3))
#  skip("false positive triggered")
  vdiffr::expect_doppelganger("geom_plot_three_plots",
                              ggplot() +
                                geom_plot(data = my.tb,
                                           mapping = aes(x, y, label = tb)) +
                                lims(x = c(0, 2), y = c(0, 2)))
#  skip("false positive triggered")
  vdiffr::expect_doppelganger("geom_plot_three_angles",
                              ggplot() +
                                geom_plot(data = my.tb,
                                          mapping = aes(x, y, label = tb),
                                          angle = 90, vjust = c(1, 0.5, 0)) +
                                lims(x = c(0, 2), y = c(0, 2)))
  # expect_warning(print(ggplot() +
  #                  geom_plot(data = my.tb[2, ],
  #                            mapping = aes(x, y, label = tb),
  #                            angle = 45) +
  #                  lims(x = c(0, 2), y = c(0, 2))))
})

test_that("examples_geom_plot", {
  p <-
    ggplot(data = mtcars, mapping = aes(wt, mpg)) +
    geom_point()

  df <- tibble(x = 0.01, y = 0.01,
               plot = list(p +
                           coord_cartesian(xlim = c(3, 4),
                                           ylim = c(13, 16)) +
                           labs(x = NULL, y = NULL)))

#  skip("false positive triggered")
  vdiffr::expect_doppelganger("geom_plot_npc_1",
                              p +
                                expand_limits(x = 0, y = 0) +
                                geom_plot_npc(data = df,
                                              aes(npcx = x, npcy = y, label = plot))
  )
#  skip("false positive triggered")
  vdiffr::expect_doppelganger("geom_plot_npc_2",
                              p +
                                expand_limits(x = 0, y = 0) +
                                geom_plot_npc(data = df,
                                              aes(npcx = x, npcy = y, label = plot),
                                              angle = 90, vjust = 1)
  )
})

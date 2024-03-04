library(ggpp)

 nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  ggplot(nc) +
    geom_sf(aes(fill = AREA)) -> p1

  nc_3857 <- sf::st_transform(nc, 3857)
  ggplot() +
    geom_sf(data = nc) +
    geom_sf(data = nc_3857, colour = "red", fill = NA)


  inset.data <-
    data.frame(lab = I(list(p1 + theme(legend.position = "none"))),
               x = 0,
               y = 0)
  p1 +
    geom_plot(data = inset.data,
    mapping = aes(label = lab, x = I(x), y = I(y))) +
    theme(axis.title = element_blank())

  inset.data <-
    data.frame(lab =
                 I(list(data.frame(
                   a = 1:3,
                   b = c("some text",
                         "more text",
                         "alpha^{3} %*%~beta %*%~sqrt(123)")))
                   ),
               x = 0.05,
               y = 0.05)
  p1 +
    geom_table(data = inset.data,
              mapping = aes(label = lab, x = I(x), y = I(y)),
              parse = TRUE) +
    theme(axis.title = element_blank())


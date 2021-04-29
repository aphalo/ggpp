context("geom_text_npc")

test_that("multiple_rows_tb", {
  df <- data.frame(
    x = c(0, 0, 1, 1, 0.5),
    y = c(0, 1, 0, 1, 0.5),
    text = c("bottom-left", "bottom-right", "top-left", "top-right", "center"),
    to.parse = c("alpha^2", "beta[2]", "delta[a]", "gamma^{2345}", "sqrt(a, b)=infinity")
  )
  vdiffr::expect_doppelganger("geom_text_npc1",
                              ggplot(df) +
                                geom_text_npc(aes(npcx = x, npcy = y, label = text)))
  vdiffr::expect_doppelganger("geom_text_npc2",
                              ggplot(data = mtcars, mapping = aes(wt, mpg)) +
                                geom_point() +
                                geom_text_npc(data = df, aes(npcx = x, npcy = y, label = text)))
  vdiffr::expect_doppelganger("geom_text_npc3",
                              ggplot(data = mtcars, mapping = aes(wt, mpg)) +
                                geom_point() +
                                geom_text_npc(data = df, aes(npcx = x, npcy = y, label = text)) +
                                expand_limits(y = 40, x = 6))
  vdiffr::expect_doppelganger("geom_text_npc4",
                              ggplot(data = mtcars) +
                                geom_point(mapping = aes(wt, mpg)) +
                                geom_text_npc(data = df,
                                              mapping = aes(npcx = x, npcy = y, label = to.parse),
                                              parse = TRUE))
})


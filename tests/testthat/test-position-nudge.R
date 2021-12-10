context("positions")

test_that("stacknudge", {
  df <- data.frame(x1 = c(1, 2, 1, 3, -1),
                   x2 = c("a", "a", "b", "b", "b"),
                   grp = c("some long name", "other name", "some name",
                           "another name", "some long name"))

  p <-
    ggplot(data = df, aes(x1, x2, group = grp)) +
    geom_col(aes(fill = grp), width = 0.5) +
    theme(legend.position = "none")

  vdiffr::expect_doppelganger("stack_nudge1",
                              p + geom_text(aes(label = grp),
                                position = position_stacknudge(vjust = 0.5, y = 0.3))
                              )
  vdiffr::expect_doppelganger("stack_nudge2",
                              p + geom_text_s(aes(label = grp),
                                                   vjust = 0,
                                            position = position_stacknudge(vjust = 0.5, y = 0.3))
  )
  vdiffr::expect_doppelganger("stack_nudge3",
                              p + geom_text(aes(label = grp),
                                            hjust = 0,
                                            position = position_stacknudge(vjust = 0, x = 0.1))
  )
  vdiffr::expect_doppelganger("stack_nudge4",
                              p + geom_text(aes(label = grp),
                                            hjust = 0, angle = 90,
                                            position = position_stacknudge(vjust = 0, x = 0.1, y = -0.21))
  )
})

test_that("dodgenudge", {
  df <- data.frame(x1 = c(1, 2, 1, 3, -1),
                   x2 = c("a", "a", "b", "b", "b"),
                   grp = c("some long name", "other name", "some name",
                           "another name", "some long name"))

  p <-
    ggplot(data = df, aes(x2, x1, group = grp)) +
    geom_col(aes(fill = grp),
             width=0.5,
             position = position_dodge(width = 0.5)) +
    theme(legend.position = "none")

  vdiffr::expect_doppelganger("dodge_nudge1",
                              p + geom_text(aes(label = grp),
                                            position = position_dodgenudge(width = 0.5, y = 0.1))
  )
  vdiffr::expect_doppelganger("dodge_nudge2",
                              p + geom_text(aes(label = grp),
                                            position = position_dodgenudge(width = 0.5, y = 0.1,
                                                                                direction = "split"))
  )
  vdiffr::expect_doppelganger("dodge_nudge3",
                              p + geom_text_s(aes(label = grp),
                                            hjust = 0,
                                            position = position_dodgenudge(width = 0.5, y = 0.2,
                                                                                direction = "split"))
  )
  vdiffr::expect_doppelganger("dodge_nudge4",
                              p + geom_text(aes(label = grp),
                                            hjust = "center",
                                            position = position_dodgenudge(width = 0.5, y = -0.05,
                                                                                direction = "split"))
  )
})

test_that("nudge_keep", {
  df <- data.frame(x1 = c(1, 2, 1, 3, -1),
                   x2 = c("a", "b", "c", "d", "e"),
                   grp = c("some long name", "other name", "some name",
                           "another name", "some long name"))

  p <-
    ggplot(data = df, aes(x2, x1, group = grp)) +
    geom_col(aes(fill = grp)) +
    theme(legend.position = "none")

  vdiffr::expect_doppelganger("nudge_keep1",
                              p + geom_text_s(aes(label = grp),
                                                   vjust = 0,
                                            position = position_nudge_keep(y = 0.12))
  )
})

test_that("nudge_center", {
  df <- data.frame(
    x = c(1,3,2,5,4,2.5),
    y = c("abc","cd","d","c","bcd","a")
  )

  vdiffr::expect_doppelganger("nudge_center1",
                              ggplot(df, aes(x, y, label = y)) +
                                geom_point() +
                                geom_text(hjust = 0, vjust = 0,
                                          position = position_nudge_center(x = 0.05, y = 0.07)
                                )
  )
  vdiffr::expect_doppelganger("nudge_center2",
                              ggplot(df, aes(x, y)) +
                                geom_point() +
                                geom_text(aes(label = y),
                                          hjust = "outward", vjust = "outward",
                                          position = position_nudge_center(x = 0.05,
                                                                           y = 0.07,
                                                                           direction = "split"))
  )
  vdiffr::expect_doppelganger("nudge_center3",
                              ggplot(df, aes(x, y)) +
                                geom_point() +
                                geom_text(aes(label = y),
                                          hjust = "outward", vjust = "outward",
                                          position = position_nudge_center(x = 0.05,
                                                                           y = 0.07,
                                                                           direction = "radial"))
  )
  vdiffr::expect_doppelganger("nudge_center4",
                              ggplot(df, aes(x, y)) +
                                geom_point() +
                                geom_text(aes(label = y),
                                          vjust = "outward", hjust = "outward",
                                          position = position_nudge_center(x = 0.06,
                                                                           y = 0.08,
                                                                           center_y = 2,
                                                                           center_x = 1.5,
                                                                           direction = "split"))
  )
  vdiffr::expect_doppelganger("nudge_center5",
                              ggplot(df, aes(x, y)) +
                                geom_point() +
                                geom_text(aes(label = y),
                                          vjust = "outward", hjust = "outward",
                                          position = position_nudge_center(x = 0.06,
                                                                           y = 0.08,
                                                                           center_y = 2,
                                                                           center_x = 1.5,
                                                                           direction = "radial"))
  )
})

test_that("nudge_line", {
  set.seed(16532)
  df <- data.frame(
    x = -10:10,
    y = (-10:10)^2,
    yy = (-10:10)^2 + rnorm(21, 0, 4),
    yyy = (-10:10) + rnorm(21, 0, 4),
    l = letters[1:21]
  )

  vdiffr::expect_doppelganger("nudge_line1",
                              ggplot(df, aes(x, y, label = l)) +
                                geom_line(linetype = "dotted") +
                                geom_point() +
                                geom_text(position = position_nudge_line())
  )
  vdiffr::expect_doppelganger("nudge_line2",
                              ggplot(df, aes(x, y, label = l)) +
                                geom_line(linetype = "dotted") +
                                geom_point() +
                                geom_text(position = position_nudge_line(xy_relative = -0.03))
  )
  vdiffr::expect_doppelganger("nudge_line3",
                              ggplot(df, aes(x, x * 2 + 5, label = l)) +
                                geom_abline(intercept = 5, slope = 2, linetype = "dotted") +
                                geom_point() +
                                geom_text(position = position_nudge_line(abline = c(5, 2)))
  )
  vdiffr::expect_doppelganger("nudge_line4",
                              ggplot(subset(df, x >= 0), aes(x, yyy)) +
                                geom_point() +
                                geom_text(aes(label = l),
                                          position = position_nudge_line(direction = "split"))
  )
  vdiffr::expect_doppelganger("nudge_line5",
                              ggplot(subset(df, x >= 0), aes(y, yy)) +
                                stat_smooth(method = "lm", formula = y ~ x) +
                                geom_point() +
                                geom_text(aes(label = l),
                                          position = position_nudge_line(method = "lm",
                                                                         direction = "split"))
  )
})

test_that("nudge_to", {
  df <- data.frame(
    x = c(1,3,2,5,4,2.5),
    y = c(2, 1, 2.5, 1.8, 2.8, 1.5),
    label = c("abc","cd","d","c","bcd","a")
  )

  vdiffr::expect_doppelganger("nudge_to1",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(y = 3),
                                                 vjust = 0)
  )
  vdiffr::expect_doppelganger("nudge_to2",
                              ggplot(df, aes(x, y, label = label)) +
                                geom_point() +
                                geom_text_s(position = position_nudge_to(x = 6),
                                                 hjust = 0)
  )
})

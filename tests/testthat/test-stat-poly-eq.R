context("stat_poly_eq")

library(tibble)

set.seed(4321)
# generate artificial data
x <- 1:100
y <- (x + x^2 + x^3) + rnorm(length(x), mean = 0, sd = mean(x^3) / 4)
my.data <- data.frame(x,
                      y,
                      group = c("A", "B"),
                      y2 = y * c(0.5,2),
                      block = c("a", "a", "b", "b"),
                      wt = sqrt(x))
formula <- y ~ poly(x, 3, raw = TRUE)

test_that("poly_formulas", {
  vdiffr::expect_doppelganger("stat_poly_eq_formula_1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ 1, parse = TRUE,
                                             mapping =
                                  aes(label = paste(stat(eq.label),
                                                    stat(adj.rr.label),
                                                    stat(AIC.label),
                                                    stat(BIC.label),
                                                    sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_1a",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ 1, parse = TRUE,
                                             mapping =
                                               aes(label = paste(stat(eq.label),
                                                                 stat(adj.rr.label),
                                                                 stat(f.value.label),
                                                                 stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_x",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x, parse = TRUE,
                                             mapping =
                                               aes(label = paste(stat(eq.label),
                                                                 stat(adj.rr.label),
                                                                 stat(f.value.label),
                                                                 stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_xminus1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x - 1, parse = TRUE,
                                             mapping =
                                               aes(label = paste(stat(eq.label),
                                                                 stat(adj.rr.label),
                                                                 stat(f.value.label),
                                                                 stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_poly1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ poly(x, 1), parse = TRUE,
                                             mapping =
                                               aes(label = paste(stat(eq.label),
                                                                 stat(adj.rr.label),
                                                                 stat(f.value.label),
                                                                 stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_poly3",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ poly(x, 3), parse = TRUE,
                                             mapping =
                                               aes(label = paste(stat(eq.label),
                                                                 stat(adj.rr.label),
                                                                 stat(f.value.label),
                                                                 stat(p.value.label),
                                                                 sep = "~~")))
  )

})

test_that("textual_positions", {
  vdiffr::expect_doppelganger("stat_poly_eq_0",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE)
  )
  vdiffr::expect_doppelganger("stat_poly_eq_1",
                               ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "text_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "label_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_3",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "text")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_4",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             geom = "label")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_5",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "text_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_6",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "label_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_7",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "text")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_8",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = "right", label.y = "bottom",
                                             geom = "label")
  )
})

test_that("numeric_positions", {
  vdiffr::expect_doppelganger("stat_poly_eq_n1",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = 0,
                                             geom = "text_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n2",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = 0,
                                             geom = "label_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n3",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = -1e5,
                                             geom = "text")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n4",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 0, label.y = -1e5,
                                             geom = "label")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n5",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 1, label.y = 0.5,
                                             geom = "text_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n6",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 1, label.y = 0.5,
                                             geom = "label_npc")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n7",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 100, label.y = 5e5,
                                             geom = "text")
  )
  vdiffr::expect_doppelganger("stat_poly_eq_n8",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_smooth(method = "lm", formula = formula) +
                                stat_poly_eq(formula = formula, parse = TRUE,
                                             label.x = 100, label.y = 5e5,
                                             geom = "label")
  )
})

test_that("rounding_signif", {
  vdiffr::expect_doppelganger("stat_poly_eq_formula_x_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ x,
                                             parse = TRUE,
                                             rr.digits = 3,
                                             p.digits = 2,
                                             f.digits = 2,
                                             coef.digits = 6,
                                             mapping =
                                               aes(label = paste(stat(eq.label),
                                                                 stat(adj.rr.label),
                                                                 stat(f.value.label),
                                                                 stat(p.value.label),
                                                                 sep = "~~")))
  )

  vdiffr::expect_doppelganger("stat_poly_eq_formula_1_round",
                              ggplot(my.data, aes(x, y)) +
                                geom_point() +
                                stat_poly_eq(formula = y ~ 1,
                                             parse = TRUE,
                                             rr.digits = 3,
                                             p.digits = 2,
                                             f.digits = 2,
                                             coef.digits = 4,
                                             mapping =
                                               aes(label = paste(stat(eq.label),
                                                                 stat(adj.rr.label),
                                                                 stat(f.value.label),
                                                                 stat(p.value.label),
                                                                 sep = "~~")))
  )

})


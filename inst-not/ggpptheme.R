#' Additional complete ggplot2 themes
#'
#' These are complete themes which control all non-data display and some
#' default aesthetics of geoms.
#'
#' @inheritDotParams ggplot2::theme_classic base_size base_family
#'
#' @param ... Named arguments passed to
#'   \code{\link[ggplot2:ggtheme]{complete themes}} used as base.
#'
#' @return A complete 'ggplot2' theme modified.
#'
#' @details
#' \code{theme_classic_boxed()} is like \code{theme_classic()} but with a box
#'   around each panel, which is useful with facets.
#'
#' @export
#'
#' @rdname ggpptheme
#'
#' @examples
#'
#' p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' p2 <- p1 + facet_wrap(facets = vars(vs))
#'
#' p1 + theme_classic_boxed()
#' p2 + theme_classic_boxed()
#'
theme_classic_boxed <- function(...) {
  theme_classic(...) +
    theme(panel.border = element_rect(colour = "black",
                                      fill = NA,
                                      linewidth = 1,
                                      inherit.blank = TRUE))
}

#' @rdname ggpptheme
#'
theme_classic_nostrip <- function(...) {
  theme_classic(...) +
    theme(strip.background = element_rect(colour = "black",
                                          fill = NA,
                                          linewidth = 0,
                                          inherit.blank = TRUE))
}

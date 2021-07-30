#' @title Linked Text
#'
#' @description Text geoms are useful for labelling plots. `geom_text_linked()`
#'   adds text to the plot and for nudged positions links the original location
#'   to the nudged text with a segment.
#'
#' @section Under development:
#'   This is a very simple and preliminary version of a geom. I plan to add
#'   features like padding around text and points. I aim to make use of the new
#'   features of 'grid' in R >= 4.1.0 to keep the implementation as fast and
#'   simple as possible. Currently this geom does all drawing using at most two
#'   vectorized calls to 'grid' functions. As a temporary replacement of padding
#'   around text one can use 'slightly out-of-range' numeric values for
#'   justification as shown in the examples.
#'
#' @details Note that when you resize a plot, text labels stay the same size,
#'   even though the size of the plot area changes. This happens because the
#'   "width" and "height" of a text element are 0. Obviously, text labels do
#'   have height and width, but they are physical units, not data units. For the
#'   same reason, stacking and dodging text will not work by default, and axis
#'   limits are not automatically expanded to include all text.
#'
#'   By default this geom uses `position_nudge_center()` which is backwards
#'   compatible with `position_nudge()` from 'ggplot2' but provides additional
#'   control on the direction of the nudging. In contrast to `position_nudge()`,
#'   `position_nudge_center()` and `position_nudge_line()` preserve the original
#'   coordinates.
#'
#' @section Alignment: You can modify text alignment with the `vjust` and
#'   `hjust` aesthetics. These can either be a number between 0 (right/bottom)
#'   and 1 (top/left) or a character (`"left"`, `"middle"`, `"right"`,
#'   `"bottom"`, `"center"`, `"top"`). There seevral two special alignments:
#'   `"inward"` and `"outward"`. Inward always aligns text towards the center of
#'   the plotting area, and outward aligns it away from the center of the
#'   plotting area. It tagged with `_mean` or `_median` the mean or median of
#'   the data in the panel along the corresponding axis is used as center.
#'
#' @param mapping Set of aesthetic mappings created by
#'   \code{\link[ggplot2]{aes}} or \code{\link[ggplot2]{aes_}}. If specified and
#'   \code{inherit.aes = TRUE} (the default), is combined with the default
#'   mapping at the top level of the plot. You only need to supply
#'   \code{mapping} if there isn't a mapping defined for the plot.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param stat The statistical transformation to use on the data for this layer,
#'   as a string.
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param parse If \code{TRUE}, the labels will be parsed into expressions and
#'   displayed as described in \code{?plotmath}.
#' @param na.rm If \code{FALSE} (the default), removes missing values with a
#'   warning.  If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped. \code{FALSE}
#'   never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics, rather
#'   than combining with them. This is most useful for helper functions that
#'   define both data and aesthetics and shouldn't inherit behaviour from the
#'   default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param check_overlap If \code{TRUE}, text that overlaps previous text in the
#'   same layer will not be plotted. \code{check_overlap} takes place at draw
#'   time and in the order of the data, thus its action depends of the size at
#'   which the plot is drawn.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There
#'   are three types of arguments you can use here:
#'
#'   \itemize{ \item Aesthetics: to set an aesthetic to a fixed value, like
#'   \code{colour = "red"} or \code{size = 3}. \item Other arguments to the
#'   layer, for example you override the default \code{stat} associated with the
#'   layer. \item Other arguments passed on to the stat. }
#' @param nudge_x,nudge_y Horizontal and vertical adjustments to nudge the
#'   starting position of each text label. The units for \code{nudge_x} and
#'   \code{nudge_y} are the same as for the data units on the x-axis and y-axis.
#' @param arrow specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}
#'
#' @return A plot layer instance.
#'
#' @export
#'
#' @examples
#'
#' my.cars <- mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
#' my.cars$name <- rownames(my.cars)
#' p <- ggplot(my.cars, aes(wt, mpg, label = name))
#'
#' # default behavior is as for geon_text()
#' p + geom_text_linked()
#' # Avoid overlaps
#' p + geom_text_linked(check_overlap = TRUE)
#' # Change size of the label
#' p + geom_text_linked(size = 2.5)
#'
#' # Use nudging
#' p +
#'   geom_point() +
#'   geom_text_linked(hjust = -0.04, nudge_x = 0.12) +
#'   expand_limits(x = 6.2)
#' p +
#'   geom_point() +
#'   geom_text_linked(hjust = -0.04, nudge_x = 0.12,
#'   arrow = arrow(length = grid::unit(1.5, "mm"))) +
#'   expand_limits(x = 6.2)
#' p +
#'   geom_point() +
#'   geom_text_linked(vjust = -0.5, nudge_y = 0.5)
#' p +
#'   geom_point() +
#'   geom_text_linked(hjust = -0.02, nudge_x = 0.1,
#'                    vjust = -0.2, nudge_y = 0.5)
#' p +
#'   geom_point() +
#'   geom_text_linked(angle = 90,
#'                    hjust = -0.04, nudge_y = 1,
#'                    arrow = arrow(length = grid::unit(1.5, "mm"))) +
#'   expand_limits(y = 40)
#'
#' # Add aesthetic mappings
#' p +
#'   geom_point() +
#'   geom_text_linked(aes(colour = factor(cyl)),
#'                    angle = 90,
#'                    hjust = -0.04, nudge_y = 1,
#'                    arrow = arrow(length = grid::unit(1.5, "mm"))) +
#'   scale_colour_discrete(l = 40) +
#'   expand_limits(y = 40)
#'
#' p + geom_text_linked(aes(size = wt)) +
#'     expand_limits(x = c(2, 6))
#' # Scale height of text, rather than sqrt(height)
#' p +
#'   geom_text_linked(aes(size = wt)) +
#'   scale_radius(range = c(3,6)) +
#'     expand_limits(x = c(2, 6))
#'
#' # You can display expressions by setting parse = TRUE.  The
#' # details of the display are described in ?plotmath, but note that
#' # geom_text_linked uses strings, not expressions.
#' p +
#'   geom_text_linked(
#'     aes(label = paste(wt, "^(", cyl, ")", sep = "")),
#'     parse = TRUE
#'   )
#'
#' # Add a text annotation
#' p +
#'   geom_text_linked() +
#'   annotate(
#'     "text_linked", label = "plot mpg vs. wt",
#'     x = 2, y = 15, size = 3, colour = "red"
#'   )  +
#'  expand_limits(x = c(1.5, 6))
#'
#' # Justification -------------------------------------------------------------
#' df <- data.frame(
#'   x = c(1, 1, 2, 2, 1.5),
#'   y = c(1, 2, 1, 2, 1.5),
#'   text = c("bottom-left", "bottom-right", "top-left", "top-right", "center")
#' )
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text))
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), vjust = "inward", hjust = "inward")
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), hjust = "inward", angle = 33)
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), hjust = "inward", angle = 66)
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), hjust = "inward", angle = 90)
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), vjust = "inward", hjust = "inward", angle = 33)
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), vjust = "inward", hjust = "inward", angle = 66)
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), vjust = "inward", hjust = "inward", angle = 90)
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), vjust = "inward_1.5", hjust = "inward_1.5")
#' ggplot(df, aes(x - 1.5, y - 1.5)) +
#'   geom_text_linked(aes(label = text), vjust = "inward_0.0", hjust = "inward_0.0")
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), vjust = "inward_mean", hjust = "inward_mean")
#' ggplot(df, aes(x - 1.5, y - 1.5)) +
#'   geom_text_linked(aes(label = text), vjust = "inward_mean", hjust = "inward_mean")
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), vjust = "inward_median", hjust = "inward_median")
#'
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), vjust = "outward", hjust = "outward")
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), vjust = "outward_mean", hjust = "outward_mean")
#' ggplot(df, aes(x - 1.5, y - 1.5)) +
#'   geom_text_linked(aes(label = text), vjust = "outward_mean", hjust = "outward_mean")
#' ggplot(df, aes(x, y)) +
#'   geom_text_linked(aes(label = text), vjust = "outward_median", hjust = "outward_median")
#' ggplot(df, aes(x - 1.5, y - 1.5)) +
#'   geom_text_linked(aes(label = text), vjust = "outward_median", hjust = "outward_median")
#'
geom_text_linked <- function(mapping = NULL,
                             data = NULL,
                             stat = "identity",
                             position = "identity",
                             ...,
                             parse = FALSE,
                             nudge_x = 0,
                             nudge_y = 0,
                             arrow = NULL,
                             check_overlap = FALSE,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- position_nudge_center(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextLinked,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      arrow = arrow,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTextLinked <-
  ggplot2::ggproto("GeomTextLinked", ggplot2::Geom,
                    required_aes = c("x", "y", "label"),

                    default_aes = ggplot2::aes(
                      colour = "black",
                      size = 3.88,
                      angle = 0,
                      hjust = 0.5,
                      vjust = 0.5,
                      alpha = NA,
                      family = "",
                      fontface = 1,
                      lineheight = 1.2,
                      segment.linetype = 1,
                      segment.colour = NULL,
                      segment.size = 0.5,
                      segment.alpha = NULL
                    ),

                    draw_panel = function(data, panel_params, coord, #panel_scales,
                                          parse = FALSE,
                                          na.rm = FALSE, check_overlap = FALSE,
                                          arrow = NULL,
                                          nudge_x = 0,
                                          nudge_y = 0) {

                      add.links <- all(c("x_orig", "y_orig") %in% colnames(data))

                      data$label <- as.character(data$label)
                      data <- subset(data, !is.na(label) & label != "")
                      if (nrow(data) == 0L) {
                        return(nullGrob())
                      }

                      lab <- data$label
                      if (parse) {
                        lab <- parse_safe(lab)
                      }

                      data <- coord$transform(data, panel_params)
                      if (add.links) {
                        data_orig <- data.frame(x = data$x_orig, y = data$y_orig)
                        data_orig <- coord$transform(data_orig, panel_params)
                      }

                      if (is.character(data$vjust)) {
                        data$vjust <-
                          compute_just2d(data = data,
                                         coord = coord,
                                         panel_params = panel_params,
                                         just = data$vjust,
                                         a = "y", b = "x")
                      }
                      if (is.character(data$hjust)) {
                        data$hjust <-
                          compute_just2d(data = data,
                                         coord = coord,
                                         panel_params = panel_params,
                                         just = data$hjust,
                                         a = "x", b = "y")
                      }

                      if(add.links) {
                        # create the grobs
                        grid::grobTree(
                          grid::segmentsGrob(
                            x0 = data$x,
                            y0 = data$y,
                            x1 = data_orig$x,
                            y1 = data_orig$y,
                            arrow = arrow,
                            gp = grid::gpar(col =
                                              alpha(data$colour,
                                                    data$alpha))),
                          grid::textGrob(
                            lab,
                            data$x, data$y, default.units = "native",
                            hjust = data$hjust, vjust = data$vjust,
                            rot = data$angle,
                            gp = gpar(
                              col = alpha(data$colour, data$alpha),
                              fontsize = data$size * .pt,
                              fontfamily = data$family,
                              fontface = data$fontface,
                              lineheight = data$lineheight
                            ),
                            check.overlap = check_overlap
                          ))
                      } else {
                        grid::textGrob(
                          lab,
                          data$x, data$y, default.units = "native",
                          hjust = data$hjust, vjust = data$vjust,
                          rot = data$angle,
                          gp = gpar(
                            col = alpha(data$colour, data$alpha),
                            fontsize = data$size * .pt,
                            fontfamily = data$family,
                            fontface = data$fontface,
                            lineheight = data$lineheight
                          ),
                          check.overlap = check_overlap
                        )
                      }
                    },

                   draw_key = draw_key_text
  )

# heavily modified from geom-text.r from 'ggplot2' 3.1.0
# when just is "outward" or "inward" or one of its variants and the geom
# supports the angle aesthetics we need to take into account that justification
# is relative to the text, rather than the plot axes. By using compute_split()
# we add support for definitions of of "inward" and "outward" relative to
# arbitrary positions along the axis.
# this function can handle either hjust or vjust, but only one at a time.
compute_just2d <- function(data,
                           coord,
                           panel_params,
                           just,
                           a = "x",
                           b = a) {
  if (a != b) {
    angle <- data$angle
  } else {
    angle <- 0
  }
  range_a <- panel_params[[a]]$scale$range$range
  range_b <- panel_params[[b]]$scale$range$range
  if (any(grepl("outward|inward", just))) {
    # ensure all angles are in -360...+360
    angle <- angle %% 360
    # ensure correct behaviour for angles in -360...+360
    angle <- ifelse(angle > 180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)
    rotated_forward <-
      grepl("outward|inward", just) & (angle > 45 & angle < 135)
    rotated_backwards <-
      grepl("outward|inward", just) & (angle < -45 & angle > -135)

    ab <- ifelse(rotated_forward | rotated_backwards, b, a)
    swap_ab <- rotated_backwards | abs(angle) > 135

    just_used <- unique(just)
    just_special <- grep("_mean$|_median$|.*[0-9].*", just_used, value = TRUE)
    middle <- rep(0.5, length(just))
    for (j in just_special) {
      j_selector <- just == j
      if (j %in% c("outward_mean", "inward_mean")) {
        middle[j_selector & !swap_ab] <- mean(data[[a]])
        middle[j_selector & swap_ab] <- mean(data[[b]])
      } else if (j %in% c("outward_median", "inward_median")) {
        middle[j_selector & !swap_ab] <- stats::median(data[[a]])
        middle[j_selector & swap_ab] <- stats::median(data[[b]])
      } else {
        middle[j_selector & swap_ab] <- stats::median(data[[b]])
        middle_a <- as.numeric(gsub("outward_|inward_", "", unique(just)))
        if (a == "x") {
          tmp_data <- tibble::tibble(x = middle_a, y = data[[b]])
          middle[j_selector & !swap_ab] <- coord$transform(tmp_data, panel_params)$x
        } else {
          tmp_data <- tibble::tibble(y = middle_a, x = data[[b]])
          middle[j_selector & !swap_ab] <- coord$transform(tmp_data, panel_params)$y
        }
      }
    }

    just <- gsub("_.*$", "", just)

    obs <- data[[a]]
    obs[swap_ab] <- data[[b]][swap_ab]

    inward <- just == "inward"
    just[inward] <- c("left", "middle", "right")[just_dir(obs[inward],
                                                          split_at = middle[inward])]
    outward <- just == "outward"
    just[outward] <- c("right", "middle", "left")[just_dir(obs[outward],
                                                           split_at = middle[outward])]
  }

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

# modified from geom-text.r from 'ggplot2' 3.1.0 to support arbitrary split
just_dir <- function(x, tol = 0.001, split_at = 0.5) {
  out <- rep(2L, length(x))
  out[x < split_at - tol] <- 1L
  out[x > split_at + tol] <- 3L
  out
}

# as in pull request to ggplot2
compute_just <- function(just, a, b = a, angle = 0) {
  #  As justification direction is relative to the text, not the plotting area
  #  we need to swap x and y if text direction is rotated so that hjust is
  #  applied along y and vjust along x.
  if (any(grepl("outward|inward", just))) {
    # ensure all angles are in -360...+360
    angle <- angle %% 360
    # ensure correct behaviour for angles in -360...+360
    angle <- ifelse(angle > 180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)
    rotated_forward <-
      grepl("outward|inward", just) & (angle > 45 & angle < 135)
    rotated_backwards <-
      grepl("outward|inward", just) & (angle < -45 & angle > -135)

    ab <- ifelse(rotated_forward | rotated_backwards, b, a)
    just_swap <- rotated_backwards | abs(angle) > 135
    inward <-
      (just == "inward" & !just_swap | just == "outward" & just_swap)
    just[inward] <- c("left", "middle", "right")[just_dir(ab[inward])]
    outward <-
      (just == "outward" & !just_swap) | (just == "inward" & just_swap)
    just[outward] <- c("right", "middle", "left")[just_dir(ab[outward])]

  }

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

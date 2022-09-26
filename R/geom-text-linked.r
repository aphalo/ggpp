#' @title Linked Text
#'
#' @description Text geoms are most useful for labelling plots. `geom_text_s()`
#'   adds text to the plot and for nudged positions links the original location
#'   to the nudged text with a segment.
#'
#' @section Under development: This is preliminary version of the geom. I plan
#'   to add features like padding around text and points. I aim to make use of
#'   the new features of 'grid' in R >= 4.1.0 to keep the implementation as fast
#'   and simple as possible. Currently this geom does all drawing using at most
#'   two vectorized calls to 'grid' functions. As a temporary replacement of
#'   padding around text one can use 'slightly out-of-range' numeric values for
#'   justification as shown in the examples. Aesthetics `segment.colour` and
#'   `segment.alpha` are implemented, but `segment.linetype` not yet.
#'
#' @details Note that when you change the scale limits for x and/or y of a plot,
#'   text labels stay the same size, as determined by the \code{size} aesthetic.
#'   The actual size as seen in the plotted output is decided during the
#'   rendering of the plot to a graphics device. Limits are expanded only to
#'   include the anchor point of the labels because the "width" and "height" of
#'   a text element are 0 (as seen by ggplot2). For the same reason, stacking
#'   and dodging text will not work as they take place within 'ggplot2' before
#'   the rendered size of text is known. Text labels do have height and width,
#'   but in grid units, not data units.
#'
#'   By default this geom uses \code{\link{position_nudge_keep}} which is
#'   backwards compatible with \code{\link[ggplot2]{position_nudge}}. In contrast to
#'   \code{\link[ggplot2]{position_nudge}}, \code{\link{position_nudge_keep}}
#'   and all other position functions defined in packages 'ggpp' and 'ggrepel'
#'   keep the original coordinates, thus allowing the plotting of connecting
#'   segments and arrows.
#'
#' @section Alignment: You can modify text alignment with the `vjust` and
#'   `hjust` aesthetics. These can either be a number between 0 (right/bottom)
#'   and 1 (top/left) or a character (\code{"left"}, \code{"middle"},
#'   \code{"right"}, \code{"bottom"}, \code{"center"}, \code{"top"}). In
#'   addition, you can use special alignments for justification including
#'   \code{"position"}, \code{"inward"} and \code{"outward"}. Position
#'   justification is computed based on the direction of the displacement of the
#'   position of the label and is the default. Inward always aligns text towards
#'   the center of the plotting area, and outward aligns it away from the center
#'   of the plotting area. If tagged with \code{_mean} or \code{_median} (e.g.,
#'   \code{"outward_mean"}) the mean or median of the data in the panel along
#'   the corresponding axis is used as center. If the characters following the
#'   underscore represent a number (e.g., \code{"outward_10.5"}) the reference
#'   point will be this value in data units.
#'
#'   By default, unless nudging is set or one of the position functions defined
#'   in this package are used, these geometries behave like the corresponding
#'   ones from package 'ggplot2'.
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes]. If
#'   specified and \code{inherit.aes = TRUE} (the default), is combined with the
#'   default mapping at the top level of the plot. You only need to supply
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
#' @param add.segments logical Display connecting segments or arrows between
#'   original positions and displaced ones if both are available.
#' @param arrow specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}
#'
#' @return A plot layer instance.
#'
#' @note You can alternatively use \code{\link[ggrepel]{geom_label_repel}},
#'   possibly setting `max.iter = 0` to disable repulsion when needed.
#'
#' @export
#'
#' @examples
#'
#' my.cars <- mtcars[c(TRUE, FALSE, FALSE, FALSE), ]
#' my.cars$name <- rownames(my.cars)
#' p <- ggplot(my.cars, aes(wt, mpg, label = name)) +
#'        geom_point(color = "red")
#'
#' # default behavior is as for geon_text()
#' p + geom_text_s()
#' # Avoid overlaps
#' p + geom_text_s(check_overlap = TRUE)
#' # Change size of the label
#' p + geom_text_s(size = 2.5)
#'
#' # default behavior is as for geon_label()
#' p + geom_label_s()
#' # Change size of the label
#' p + geom_label_s(size = 2.5)
#'
#' # Use nudging
#' p +
#'   geom_text_s(nudge_x = 0.12) +
#'   expand_limits(x = 6.2)
#' p +
#'   geom_text_s(nudge_x = 0.12,
#'               arrow = arrow(length = grid::unit(1.5, "mm"))) +
#'   expand_limits(x = 6.2)
#' p +
#'   geom_text_s(hjust = "left", nudge_x = 0.12) +
#'   expand_limits(x = 6.2)
#' p +
#'   geom_text_s(nudge_y = 0.1, nudge_x = 0.07) +
#'   expand_limits(x = 6.2)
#' p +
#'   geom_text_s(nudge_y = 0.25, angle = 90) +
#'   expand_limits(y = 25)
#' p +
#'   geom_text_s(nudge_y = 0.22) +
#'   expand_limits(x = c(2, 6))
#' p +
#'   geom_text_s(angle = 90, nudge_y = 1,
#'               arrow = arrow(length = grid::unit(1.5, "mm")),
#'               segment.colour = "red") +
#'   expand_limits(y = 30)
#'
#' p +
#'   geom_label_s(nudge_x = 0.12) +
#'   expand_limits(x = 6.2)
#'
#' p +
#'   geom_label_s(hjust = "outward_1", nudge_x = 0.12) +
#'   expand_limits(x = 6.2)
#'
#' p +
#'   geom_label_s(hjust = "inward_3", nudge_y = 0.4)
#'
#' p +
#'   geom_label_s(nudge_x = 0.1) +
#'   expand_limits(x = 6.2)
#'
#' p +
#'   geom_label_s(nudge_x = -0.1) +
#'   expand_limits(x = 1.5)
#'
#' # Add aesthetic mappings and adjust arrows
#' p +
#'   geom_text_s(aes(colour = factor(cyl)),
#'               segment.colour = "black",
#'               angle = 90,
#'               nudge_y = 1,
#'               arrow = arrow(angle = 20,
#'                             length = grid::unit(1.5, "mm"),
#'                             ends = "first",
#'                             type = "closed"),
#'               show.legend = FALSE) +
#'   scale_colour_discrete(l = 40) + # luminance, make colours darker
#'   expand_limits(y = 25)
#'
#' # Add aesthetic mappings and adjust arrows
#' p +
#'   geom_label_s(aes(colour = factor(cyl)),
#'               nudge_x = 0.3,
#'               arrow = arrow(angle = 20,
#'                             length = grid::unit(1/3, "lines"))) +
#'   scale_colour_discrete(l = 40) + # luminance, make colours darker
#'   expand_limits(x = 7)
#'
#' # Scale height of text, rather than sqrt(height)
#' p +
#'   geom_text_s(aes(size = wt), nudge_x = -0.1) +
#'   scale_radius(range = c(3,6)) + # override scale_area()
#'     expand_limits(x = c(1.8, 5.5))
#'
geom_text_s <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = "identity",
                        ...,
                        parse = FALSE,
                        nudge_x = 0,
                        nudge_y = 0,
                        add.segments = TRUE,
                        arrow = NULL,
                        check_overlap = FALSE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position) && position != "identity") {
      rlang::abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }
    # by default we keep the original positions
    position <- position_nudge_keep(nudge_x, nudge_y)
  }


  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextS,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      add.segments = add.segments,
      arrow = arrow,
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
GeomTextS <-
  ggplot2::ggproto("GeomTextS", ggplot2::Geom,
                   required_aes = c("x", "y", "label"),

                   default_aes = ggplot2::aes(
                     colour = "black",
                     size = 3.88,
                     angle = 0,
                     hjust = "position",
                     vjust = "position",
                     alpha = NA,
                     family = "",
                     fontface = 1,
                     lineheight = 1.2,
                     segment.linetype = 1,
                     segment.colour = "grey33",
                     segment.size = 0.75,
                     segment.alpha = 1
                   ),

                   draw_panel = function(data,
                                         panel_params,
                                         coord, #panel_scales,
                                         parse = FALSE,
                                         na.rm = FALSE,
                                         check_overlap = FALSE,
                                         add.segments = TRUE,
                                         arrow = NULL) {

                     add.segments <- add.segments && any(c("x_orig", "y_orig") %in% colnames(data))

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
                     if (all(c("x_orig", "y_orig") %in% colnames(data))) {
                       data_orig <- data.frame(x = data$x_orig, y = data$y_orig)
                       data_orig <- coord$transform(data_orig, panel_params)
                       data$x_orig <- data_orig$x
                       data$y_orig <- data_orig$y
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

                     # loop needed as gpar is not vectorized
                     all.grobs <- grid::gList()

                     for (row.idx in 1:nrow(data)) {
                       row <- data[row.idx, , drop = FALSE]
                       user.grob <- grid::textGrob(
                         lab[row.idx],
                         row$x, row$y, default.units = "native",
                         hjust = row$hjust, vjust = row$vjust,
                         rot = row$angle,
                         gp = gpar(
                           col = ggplot2::alpha(row$colour, row$alpha),
                           fontsize = row$size * .pt,
                           fontfamily = row$family,
                           fontface = row$fontface,
                           lineheight = row$lineheight
                         ),
                         check.overlap = check_overlap
                       )

                       # give unique name to each grob
                       user.grob$name <- paste("text.s.grob", row$group, row.idx, sep = ".")

                       if (add.segments) {
                         segment.grob <-
                           grid::segmentsGrob(x0 = row$x,
                                              y0 = row$y,
                                              x1 = row$x_orig,
                                              y1 = row$y_orig,
                                              arrow = arrow,
                                              gp = grid::gpar(col = ggplot2::alpha(row$segment.colour,
                                                                                   row$segment.alpha)),
                                              name = paste("text.s.segment", row$group, row.idx, sep = "."))
                         all.grobs <- grid::gList(all.grobs, segment.grob, user.grob)
                       } else {
                         all.grobs <- grid::gList(all.grobs, user.grob)
                       }
                     }

                     # name needs to be unique within plot, so we would to know layer
#                     grid::grobTree(children = all.grobs, name = "geom.text.s.panel")
                     grid::grobTree(children = all.grobs)

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
  if (any(grepl("outward|inward|position", just))) {
    # ensure all angles are in -360...+360
    angle <- angle %% 360
    # ensure correct behaviour for angles in -360...+360
    angle <- ifelse(angle > 180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)
    rotated_forward <-
      grepl("outward|inward|position", just) & (angle > 45 & angle < 135)
    rotated_backwards <-
      grepl("outward|inward|position", just) & (angle < -45 & angle > -135)

    ab <- ifelse(rotated_forward | rotated_backwards, b, a)
    swap_ab <- rotated_backwards | abs(angle) > 135

    if (any(just == "position")) {
      # compute justification based on saved original position
      # works only with special position functions from 'ggpp' and 'ggrepel'
      ab_orig <- paste(ab, "_orig", sep = "")
      position <- just == "position"
      if (!all(unique(ab_orig) %in% colnames(data))) {
        just[position] <- "middle"
      } else {
        just[position] <-
          c("left", "middle", "right")[2L + 1L * sign(data[[ab_orig[1L]]] - data[[ab[1L]]])]
      }
    }
    if (any(grepl("outward|inward", just))) {
      # we allow tags for setting center/middle position for just
      just_used <- unique(just)
      just_special <- grep("_mean$|_median$|.*[0-9].*", just_used, value = TRUE)
      middle <- rep(0.5, length(just))
      for (j in just_special) { # skipped if just_special has length zero
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

      # what follows is like in ggplot2 except for split_at
      obs <- data[[a]]
      obs[swap_ab] <- data[[b]][swap_ab]

      inward <- just == "inward"
      just[inward] <- c("left", "middle", "right")[just_dir(obs[inward],
                                                            split_at = middle[inward])]
      outward <- just == "outward"
      just[outward] <- c("right", "middle", "left")[just_dir(obs[outward],
                                                             split_at = middle[outward])]
    }
  }

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}

# modified from geom-text.r from 'ggplot2' 3.1.0 to support arbitrary split
# for "inward" and "outward" justification
just_dir <- function(x, tol = 0.001, split_at = 0.5) {
  out <- rep(2L, length(x))
  out[x < split_at - tol] <- 1L
  out[x > split_at + tol] <- 3L
  out
}

# as in pull request to ggplot2
# can be used by other geoms
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

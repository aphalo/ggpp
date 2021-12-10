#' @rmd geom_grob
#'
#' @export
#'
geom_grob_linked <- function(mapping = NULL, data = NULL,
                              stat = "identity", position = "identity",
                              ...,
                              nudge_x = 0,
                              nudge_y = 0,
                              arrow = NULL,
                              na.rm = FALSE,
                              show.legend = FALSE,
                              inherit.aes = FALSE) {

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
    geom = GeomGrobLinked,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggpp-ggproto
#'
#' @format NULL
#' @usage NULL
#'
grob_linked_draw_panel_fun <-
  function(data,
           panel_params,
           coord,
           na.rm = FALSE,
           arrow = NULL) {

    if (nrow(data) == 0) {
      return(grid::nullGrob())
    }

    if (!grid::is.grob(data$label[[1]])) {
      warning("Skipping as object mapped to 'label' is not a list of \"grob\" objects.")
      return(grid::nullGrob())
    }

    add.links <- all(c("x_orig", "y_orig") %in% colnames(data))

    # should be called only once!
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

    user.grobs <- grid::gList()
    idx.shift <- 0

    # Draw segments first
    if(add.links) {
      idx.shift <- idx.shift + 1
      user.grobs[[1L]] <-
        grid::segmentsGrob(x0 = data$x,
                           y0 = data$y,
                           x1 = data_orig$x,
                           y1 = data_orig$y,
                           arrow = arrow,
                           gp = grid::gpar(col = alpha(data$segment.colour,
                                                       data$segment.alpha)),
                           name = "linking.segments.grob")
    }

    for (row.idx in 1:nrow(data)) {
      userGrob <- data$label[[row.idx]]

      userGrob$vp <-
        grid::viewport(x = grid::unit(data$x[row.idx], "native"),
                       y = grid::unit(data$y[row.idx], "native"),
                       width = grid::unit(data$vp.width[row.idx], "npc"),
                       height = grid::unit(data$vp.height[row.idx], "npc"),
                       just = c(data$hjust[row.idx], data$vjust[row.idx]),
                       angle = data$angle[row.idx],
                       name = paste("geom_grob.panel", data$PANEL[row.idx],
                                    "row", row.idx, sep = "."))

      # give unique name to each grob
      userGrob$name <- paste("inset.grob", row.idx, sep = ".")

      user.grobs[[row.idx + idx.shift]] <- userGrob
    }

    grid.name <- paste("geom_grob.panel",
                       data$PANEL[row.idx], sep = ".")
    grid.name <- c(grid.name, "geom_grob.panel.segments")


    grid::grobTree(children = user.grobs)

  }

#' @rdname ggpp-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomGrobLinked <-
  ggplot2::ggproto("GeomGrobLinked", ggplot2::Geom,
          required_aes = c("x", "y", "label"),

          default_aes = ggplot2::aes(
            colour = "black", angle = 0, hjust = 0.5,
            vjust = 0.5, alpha = NA, family = "", fontface = 1,
            vp.width = 1/5, vp.height = 1/5,
            segment.linetype = 1,
            segment.colour = "grey33",
            segment.size = 0.5,
            segment.alpha = 1
          ),

          draw_panel = grob_linked_draw_panel_fun,

          draw_key = function(...) {
            grid::nullGrob()
          }
  )

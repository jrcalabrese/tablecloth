#' Apply clear background to ggplot.
#'
#' Applies a totally-clear/transparent background to a ggplot2 object as a theme.
#' Uses `theme_bw()`.
#'
#' @param clr Desired outline color.
#' @param bg Should background be transparent? Binary.
#'
#' @importFrom ggplot2 theme_bw theme
#' @export
clear_out <- function(clr, bg) {

  element_blank <- element_rect <- element_text <- element_line <- rel <- NULL

  # If color is not specified, default to black
  if (missing(clr))
    clr <- "black"
  else
    do_nothing <- "nothing"

  # If bg is not specified, default to TRUE
  if (missing(bg))
    bg <- TRUE
  else
    do_nothing_again <- "nothing"

  if (bg == TRUE) {
    backg <- "transparent"
    backline <- "transparent"
    pg <- element_blank()
    pmn <- element_blank()
    }
  else {
    backg <- "white"
    backline <- "grey85"
    pg <- element_line(colour = "grey85")
    pmn <- element_line(size = rel(0.5))
    }

  ggplot2::theme_bw() + ggplot2::theme(
    panel.grid = pg,
    panel.grid.minor = element_blank(),
    panel.grid.major = pmn,
    panel.background = element_rect(fill = backg),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.text = element_text(color = clr),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    panel.border = element_rect(fill = NA,
                                color = clr,
                                linetype = "solid"),
    axis.ticks = element_line(color = clr),
    text = element_text(color = clr),
    axis.text = element_text(color = clr),
    strip.background = element_rect(color = clr, fill = backline),
    strip.text = element_text(color = clr), complete = TRUE)
}


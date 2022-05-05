#' Apply clear background to ggplot.
#'
#' Applies a totally-clear/transparent background to a ggplot2 object as a theme.
#'
#' @param clr Desired outline color.
#'
#' @importFrom ggplot2 theme_bw theme
#' @export
clear_out <- function(clr){

  element_blank <- element_rect <- element_text <- element_line <- NULL

  # If color is not specified, default to black
  if (missing(clr))
    clr <- "black"
  else
    do_nothing <- "nothing"

  ggplot2::theme_bw() + ggplot2::theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "transparent"),
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
    strip.background = element_rect(color = clr, fill = "transparent"),
    strip.text = element_text(color = clr))
}


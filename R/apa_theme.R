#' Format a table using APA style.
#'
#' This function can be applied to both `gtsummary` and `flextable` objects, I think.
#' Maybe it will work on other things?
#' I should update this later.
#'
#' @param ft A table like a `flextable` or `gtsummary` object. Will convert to `flextable` if not already a `flextable` object.
#' 
#' @export

apa_theme <- function (ft)  {
  
  if (is(ft) == "flextable") {
    ft
  }
  
  if (!is(ft) == "flextable") {
    ft <- gtsummary::as_flex_table(ft)
  }
  
  ft %>% 
    flextable::font(fontname = "Times New Roman", part = "all") %>% 
    flextable::fontsize(size = 12, part = "all") %>% 
    flextable::align(align = "left", part = "body") %>% 
    flextable::align(align = "center", part = "header") %>% 
    flextable::rotate(rotation = "lrtb", align = "top", part = "body") %>% 
    flextable::border_remove() %>% 
    flextable::hline_top(border = officer::fp_border(width = 2), 
                         part = "all") %>% 
    flextable::hline_bottom(border = officer::fp_border(width = 2),
                            part = "all") %>% 
    flextable::hline(i = 1, border = officer::fp_border(width = 1), part = "header") %>%
    flextable::set_table_properties(layout = "autofit")
}
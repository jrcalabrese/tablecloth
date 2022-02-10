#' Create a "menu" of corrected p-values.
#'
#' This function turns a list of p-values into correct versions using `p.adjust`.
#'
#' The output will be a `flextable` formatted in APA style.
#'
#' @param ... Numeric, enter p-values here.
#'
#' @param digits Numeric, amount of digits after decimal point when rounding values. Optional.
#'
#' @param title Character vector, table title. Optional.
#'
#' @export
pval_table <- function(..., digits, title){

  if (missing(digits))
    digits <- 3
  else
    digits <- digits

  x <- list(...)

  a <- p.adjust(x, method = "holm", length(x))
  b <- p.adjust(x, method = "hochberg", length(x))
  c <- p.adjust(x, method = "hommel", length(x))
  d <- p.adjust(x, method = "bonferroni", length(x))
  e <- p.adjust(x, method = "BH", length(x))
  f <- p.adjust(x, method = "BY", length(x))
  g <- p.adjust(x, method = "fdr", length(x))
  h <- p.adjust(x, method = "none", length(x))

  names <- c("Original p-value",
             "Holm",
             "Hochberg",
             "Hommel",
             "Bonferroni",
             "Benjamini-Hochberg",
             "Benjamini-Yekutieli",
             "False Discovery Rate")

  newvalues <- data.frame(h,a,b,c,d,e,f,g)
  colnames(newvalues) <- names

  if (missing(title))
    title <- "Table of Corrected P-values"
  else
    title <- title

  z <- newvalues %>%
    rrtable::df2flextable(
      vanilla = TRUE,
      digits = digits,
      add.rownames = FALSE,
      colorheader = FALSE,
      align_body = "left",
      NA2space = TRUE) %>%
    tablecloth::apa_theme()  %>%
    flextable::add_header_lines(values = title) %>%
    flextable::align_text_col(align = "left", header = FALSE) %>%
    align_nottext_col(align = "center", header = TRUE)

  return(z)
}

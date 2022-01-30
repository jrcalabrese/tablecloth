#' Create a descriptive statistics table from imputed data.
#'
#' This takes data from a `mids` object and turns the variables you specify
#' into table of descriptive statistics This is useful when handling imputed data.
#'
#' The output will be a `flextable` formatted in APA style.
#'
#' @param mids A `mids` object.
#'
#' @param title The title of your correlation matrix. Must be `character` class.
#'
#' @param ... Variables from your `mids` separated by commas. E.g., "bmi", "chl".
#'
#' @export

mice_df <- function(mids, title, ...){

  impdat <- mice::complete(mids, action = "long", include = FALSE)

  # https://bookdown.org/mwheymans/bookmi/data-analysis-after-multiple-imputation.html

  z <- lapply(list(...), function(x){
    x = as.name(x)
    pool_mean <- with(impdat, by(impdat, .imp, function(y) c(
      mean(y[[x]]),
      sd(y[[x]]),
      min(y[[x]]),
      max(y[[x]]),
      ( sd(y[[x]])/sqrt(length(y[[x]])) )
    )))
    Reduce("+", pool_mean)/length(pool_mean)
  }) %>% setNames(list(...)) %>%
    as.data.frame() %>%
    `rownames<-`(c("Mean", "Standard Deviation", "Minimum",
                   "Maximum", "Standard Error")) %>%
    t() %>% as.data.frame() %>%
    rownames_to_column("Variable")

  z <- z %>%
    rrtable::df2flextable(
      vanilla = TRUE,
      add.rownames = FALSE,
      colorheader = FALSE,
      align_body = "left",
      NA2space = TRUE)  %>%
    apa_theme() %>%
    flextable::add_header_lines(values = title)

  return(z)
}

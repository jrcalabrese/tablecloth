#' Create a descriptive statistics table from imputed data.
#'
#' This takes data from a `mids` object and turns the variables you specify
#' into table of descriptive statistics This is useful when handling imputed data.
#'
#' The output will be a `flextable` formatted in APA style.
#'
#' @param mids A `mids` object.
#'
#' @param vs Variables from `mids`. Must be `character` class. E.g., c("bmi", "chl").
#'
#' @param title The title of your correlation matrix. Must be `character` class. Optional.
#'
#' @param nm Preferred variable names. Must be `character` class. E.g., c("bmi", "chl"). Optional.
#'
#' @export

mice_df <- function(mids, vs, title, nm){

  impdat <- mice::complete(mids, action = "long", include = FALSE)

  # https://bookdown.org/mwheymans/bookmi/data-analysis-after-multiple-imputation.html

  z <- lapply(as.list(vs), function(x){
    x = as.name(x)
    pool_mean <- with(impdat, by(impdat, .imp, function(y) c(
      mean(y[[x]]),
      sd(y[[x]]),
      min(y[[x]]),
      max(y[[x]]),
      ( sd(y[[x]])/sqrt(length(y[[x]])) )
    )))
    Reduce("+", pool_mean)/length(pool_mean)
  }) %>%
    setNames(as.list(vs)) %>%
    as.data.frame()

  if (missing(nm))
    colnames(z) <- vs
  else
    colnames(z) <- nm

    z <- z %>% `rownames<-`(c("Mean", "Standard Deviation", "Minimum",
                   "Maximum", "Standard Error")) %>%
    t() %>% as.data.frame() %>%
    rownames_to_column("Variable")

    if (missing(title))
      title <- "Descriptive Statistics"
    else
      title <- title

  z <- z %>%
    rrtable::df2flextable(
      vanilla = TRUE,
      add.rownames = FALSE,
      colorheader = FALSE,
      align_body = "left",
      NA2space = TRUE) %>%
    apa_theme() %>%
    flextable::add_header_lines(values = title)

  return(z)
}

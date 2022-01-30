#' Create a descriptive statistics table from imputed data.
#'
#' This takes data from a `mids` object and turns the variables you specify
#' into table of descriptive statistics This is useful when handling imputed data.
#'
#' The output will be a `flextable` formatted in APA style.
#'
#' **Don't use this yet!** Still working on it.
#'
#' @param mids A `mids` object.
#'
#' @param v A variable from your `mids`.
#'
#' @param title The title of your correlation matrix. Must be `character` class.
#'
#' @export

mice_df <- function(mids, v, title) {
  v <- deparse(substitute(v))
  # from here: https://bookdown.org/mwheymans/bookmi/data-analysis-after-multiple-imputation.html
  impdat <- mice::complete(a, action = "long", include = FALSE)
  pool_mean <- with(impdat, by(impdat, .imp, function(x) c(mean(x[[b]]), sd(x[[b]]))))
  (Reduce("+", pool_mean)/length(pool_mean))
}



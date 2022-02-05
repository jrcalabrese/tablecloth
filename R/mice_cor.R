#' Create a correlation matrix from imputed data.
#'
#' This takes data from a `mids` object and turns the variables you specify
#' into a correlation matrix. This is useful when handling imputed data.
#'
#' The output will be a `flextable` formatted in APA style.
#'
#' @param imp A `mids` object.
#'
#' @param vs Variables from `imp`. Must be `character` class. E.g., `c("bmi", "chl")`.
#'
#' @param title The title of your correlation matrix. Must be `character` class. Optional.
#'
#' @export
mice_cor <- function(imp, vs, title) {

  # do the thing
  res <- miceadds::micombine.cor(mi.res = imp, variables = vs) %>%
    select(c(variable1,variable2, r, p))

  # round digits
  res$r <- round(res$r, digits = 2)
  res$r <- sub("^(-?)0.", "\\1.", sprintf("%.2f", res$r))
  res$p <- round(res$p, digits = 3)
  res$p <- sub("^(-?)0.", "\\1.", sprintf("%.2f", res$p))

  # Surround with parentheses
  res$p <- paste0("(",res$p,")")

  # Combine
  res$value <- paste0(res$r," \n ",res$p)

  # Get rid of old columns
  res$r <- NULL
  res$p <- NULL

  # Make wide
  res <- res %>%
    group_by(variable2, variable1) %>%
    pivot_wider(
      names_from = variable1,
      values_from = value
    ) %>%
    column_to_rownames("variable2")

  res[is.na(res)] <- "-"

  # Slightly change row order
  res <- res[match(colnames(res), rownames(res)),]

  # Extract lower triangle
  res[upper.tri(res, diag = TRUE)] <- NA
  res <- res[rowSums(is.na(res)) != ncol(res), ]
  res <- res[,colSums(is.na(res)) < nrow(res)]

  if (missing(title))
    title <- "Correlation Matrix"
  else
    title <- title

  res <- res %>%
    rrtable::df2flextable(
    vanilla = TRUE,
    add.rownames = TRUE,
    colorheader = FALSE,
    align_body = "left",
    NA2space = TRUE) %>%
    tablecloth::apa_theme() %>%
    compose(i = 1, j = 1, part = "header", as_paragraph(as_chunk(" "))) %>%
    flextable::add_header_lines(values = title)

  return(res)
}

#' @example

data(nhanes)
imp <- mice::mice(nhanes, m = 5, print = FALSE)
vs <- c("bmi", "chl", "age")
title <- "Title of my matrix"
mice_cor(imp = imp,
         vs = vs,
         title = title)

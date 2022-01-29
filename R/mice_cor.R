#' Make a correlation matrix from imputed data.
#'
#' This takes data from a `mira` object and turns the variables you want
#' into a correlation matrix. Make sure your variable names are pretty.
#' I'll try to add something like x/y axis varnames in the future.
#' 
#' @param mira A `mira` object.
#' 
#' @param x List of variables from your `mira` object. Must be `character` class.
#' 
#' @param title The title of your correlation matrix.
#' 
#' @export

mice_cor <- function(mira, x, title) {
  
  # do the thing
  res <- miceadds::micombine.cor(mi.res=mira, variables=c(x) ) %>%
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
  res[upper.tri(res,diag=TRUE)] <- NA
  res <- res[rowSums(is.na(res)) != ncol(res), ]
  res <- res[,colSums(is.na(res))<nrow(res)]
  
  res <- res %>% 
    rrtable::df2flextable(
    vanilla = TRUE,
    add.rownames = TRUE,
    colorheader = FALSE,
    align_body = "left",
    NA2space = TRUE)  %>%
    apa_theme() %>% 
    flextable::add_header_lines(values = title)
  
  return(res)
}
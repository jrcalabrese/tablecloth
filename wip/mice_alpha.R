#' Calculates Cronbach's alpha with multiply imputed data.
#'
#' Describe function here. Code source:
#' https://stackoverflow.com/questions/70816175/how-do-i-calculate-cronbachs-alpha-on-multiply-imputed-data
#'
#' @param imp A `mids` object.
#' @param boot Logical indicating whether a non-parametric bootstrap should be conducted.
#' @param bnum integer specifying the number of bootstrap samples to be taken.
#' @param ci Logical indicating whether a confidence interval around alpha should be estimated.
#'
#' @export

set.seed(123)

# sample survey responses
df <- data.frame(
  x1 = c(1,2,2,3,2,2,3,3,2,3,
         1,2,2,3,2,2,3,3,2,3,
         1,2,2,3,2,2,3,3,2,3),
  x2 = c(1,1,1,2,3,3,2,3,3,3,
         1,1,1,2,3,3,2,3,3,3,
         1,2,2,3,2,2,3,3,2,3),
  x3 = c(1,1,2,1,2,3,3,3,2,3,
         1,1,2,1,2,3,3,3,2,3,
         1,2,2,3,2,2,3,3,2,3)
)

# function to column-wise generate missing values (MCAR)
create_missings <- function(data, prob) {
  x <- replicate(ncol(data),rbinom(nrow(data), 1, prob))
  for(k in 1:ncol(data)) {
    data[, k] <- ifelse(x[, k] == 1, NA, data[,k])
  }
  data
}
df <- create_missings(df, prob = 0.2)

# multiple imputation ----------------------------------

library(mice)
imp <- mice(df, m = 10, maxit = 20, printFlag = FALSE)

# extract the completed data in long format

mice_alpha <- function(imp, boot, B, ci) {

  boot <- boot
  B <- B
  ci <- ci

  cronbach_fun <- function(list_compl_data
                           #boot, B = 1e4, ci = FALSE
                           ) {
    n <- nrow(list_compl_data); p <- ncol(list_compl_data)
    total_variance <- var(rowSums(list_compl_data))
    item_variance <- sum(apply(list_compl_data, 2, sd)^2)
    alpha <- (p/(p - 1)) * (1 - (item_variance/total_variance))
    out <- list(alpha = alpha)
    boot_alpha <- numeric(B)
    if (boot) {
      for (i in seq_len(B)) {
        boot_dat <- list_compl_data[sample(seq_len(n), replace = TRUE), ]
        total_variance <- var(rowSums(boot_dat))
        item_variance <- sum(apply(boot_dat, 2, sd)^2)
        boot_alpha[i] <- (p/(p - 1)) * (1 - (item_variance/total_variance))
      }
      out$var <- var(boot_alpha)
    }
    if (ci){
      out$ci <- quantile(boot_alpha, c(.025,.975))
    }
    return(out)
  }

obtain_fun <- function(imp) {

  implong <- complete(imp, 'long')
  m <- length(unique(implong$.imp))
  boot_alpha <- rep(list(NA), m)

    for (i in seq_len(m)) {
      set.seed(i) # fix random number generator
      sub <- implong[implong$.imp == i, -c(1,2)]
      boot_alpha[[i]] <- cronbach_fun(sub)

    }
  return(boot_alpha)
}

boot_alpha <- obtain_fun(imp)

# obtain Q and U (see ?pool.scalar)
Q <- sapply(boot_alpha, function(x) x$alpha)
U <- sapply(boot_alpha, function(x) x$var)

# pooled estimates
pool_estimates <- function(x) {
  out <- c(
    alpha = x$qbar,
    lwr = x$qbar - qt(0.975, x$df, na.rm = TRUE) * sqrt(x$t),
    upr = x$qbar + qt(0.975, x$df, na.rm = TRUE) * sqrt(x$t)
  )
  return(out)
}

x <- pool_estimates(pool.scalar(Q, U))
return(x)
}


mice_alpha(imp, TRUE, 10, FALSE)

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

rm(create_missings)

#implong <- mice::complete(imp, action = "long")

# specify list...
#varlist <- c("x1", "x2", "x3")

rm(df)
#bootyesno <- TRUE
#numberofboots <- 1e4
#conf <- TRUE
#seed <- 12345

#mice_alpha(imp = imp, varlist = here, boot = TRUE, B = 100, ci = TRUE, seed = 123)


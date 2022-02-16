#' Survive Dr. Pek's homewor.
#'
#' Will add later!!!!!
#'
#' @param mod `lmerMod` object.
#'
#' @importFrom tibble remove_rownames rownames_to_column
#' @importFrom dplyr %>% mutate select slice
#' @importFrom stats vcov
#' @importFrom lme4 fixef
#'
#' @export
extract_lmer <- function(mod){

  fixef <- V1 <- sprite <- vcov <- NULL

  # fixed effects
  fx <- lme4::fixef(mod) %>% as.data.frame()
  n <- length(fx[[1]])
  greek <- rep(list("gamma"), length(fx[[1]])) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame() %>%
    tibble::remove_rownames()
  greek$sprite <- sprintf("%02d", seq(0, (n-1)))
  greek <- greek %>%
    dplyr::mutate(fixed = paste0(V1, sprite)) %>%
    dplyr::select(fixed)
  fixed <- cbind(greek, fx) %>%
    `colnames<-`(c("greek", "var"))

  # random effects
  rx <- as.data.frame(lme4::VarCorr(mod), comp = "Variance") %>%
    dplyr::select(vcov) %>%
    dplyr::slice(1:1) %>%
    dplyr::mutate(greek = "t00") %>%
    dplyr::select(greek, vcov) %>%
    `colnames<-`(c("greek", "var"))

  here <- rbind(rx, fixed) %>%
    tibble::rownames_to_column(var = "varname")

  return(here)
}

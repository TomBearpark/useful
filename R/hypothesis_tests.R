#' Test for equality of coefficients across some margin of heterogeneity
#'
#' @param m : fixest object with interacted treatment variables
#' @param poly : the order of the polynomial in the treatment
#' @param lags the number of lags of the treatment variabe
#'
#' @return
#' @export
#'
#' @examples
test_het_coefs <- function(m, poly, lags){

  tt <- c()

  vars <- names(stats::coef(m))

  for(pp in 1:poly){
    for(ll in 0:lags){
      t.vars <-
        stringr::str_subset(vars, paste0("p", pp)) %>%
        stringr::str_subset(paste0("l", ll)) %>%
        sort()
      base <- t.vars[1]
      for(other in t.vars[-1]) tt <- c(tt, paste0(base,"=",other))
    }
  }
  list(string = tt,
       result = car::linearHypothesis(m, tt)
       )
}

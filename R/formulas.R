# Functions for building formula's for fixest regressions

#' Generate a string for a given variable
#'
#' @param leads
#' @param lags
#' @param treatvar
#' @param interact
#'
#' @return
#' @export
#'
#' @examples
gen_ff <- function(leads, lags, treatvar, interact = NULL){
  if(is.null(interact)){
    leads_str <- ifelse(leads > 0, paste0("f", leads:1, "_",treatvar, collapse = "+"), "")
    lags_str  <- ifelse(lags  > 0, paste0("l", 0:lags, "_",treatvar ,  collapse = "+"), "")
  }else{
    leads_str <- ifelse(leads > 0,
                        paste0("i(", interact, ", ","f", leads:1, "_",treatvar,")", collapse = "+"), "")
    lags_str  <- ifelse(lags  > 0,
                        paste0("i(", interact, ", ","l", 0:lags, "_", treatvar, ")",  collapse = "+"), "")

    if(leads == 0 & lags == 0) treatvar <- paste0("i(", interact, ", ", treatvar, ")")
  }
  if(leads >0 | lags>0) ff <- paste0(leads_str, "+", lags_str)
  else ff <- treatvar
  ff
}


#' Build a formula for fixest
#'
#' @param yvar
#' @param treat
#' @param control
#' @param poly_treat
#' @param poly_control
#' @param leads
#' @param lags
#' @param FE
#' @param interact
#' @param manual_treat
#'
#' @return
#' @export
#'
#' @examples
build_formula_poly <- function(yvar,
                               treat,
                               control,
                               poly_treat,
                               poly_control,
                               leads,
                               lags,
                               FE,
                               interact = NULL,
                               manual_treat = NULL){

  if(!is.null(manual_treat)){
    stopifnot(is.null(interact))
    ff <- paste0(manual_treat, "+")

  }else{

    ff <- ""
    for(nn in seq_along(treat)){

      for(pp in 1:poly_treat[nn]){

        if(is.null(interact[nn])){
          ff <- paste0(ff,
                       gen_ff(leads = leads, lags = lags,
                              paste0(treat[nn], pp)), "+")
        }else{
          ff <- paste0(ff,
                       gen_ff(leads = leads, lags = lags,
                              treatvar = paste0(treat[nn], pp),
                              interact = interact[nn])
                       , "+")
        }
      }
    }
  }

  for(nn in seq_along(control)){
    for(pp in 1:poly_control[nn]){
      ff <- paste0(ff, gen_ff(leads = leads, lags = lags,
                              paste0(control[nn], pp)))
      if(pp < poly_control[nn] | nn != length(control)) ff <- paste0(ff,  "+")
    }
  }

  if(is.null(FE)) {
    ff <- as.formula(paste0(yvar, "~", ff))
  }
  else{
    ff <- as.formula(paste0(yvar, "~", ff, "|", FE))
  }
  return(ff)
}

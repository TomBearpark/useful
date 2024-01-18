#' Get the response function from a polynomial FE regression, with SEs
#'
#' @param m An output of a fixest regression
#' @param var The string identifier for thevariable we want to get a RF for.
#' For this to work, you need
#' to make sure that the polynomial terms are consistently named, and include
#' this string. They need to be named "l{lag}_{var}_p{poly}" at the moment
#'
#'
#' @param min The minimum value we want to predic the RF for
#' @param max THe max value we want to predict the RF for
#' @param ref The reference value, all other
#' @param ci_level The confidence interval
#' @param step.length How far apart should we predict each value
#' @param coefs Use this if you want to manually specify the coefs, rather
#' than automatically guess them
#' @param id.col can specify something to put into an extra column: useful for
#' facetting in later plots
#' @param xvar_name what do you want the prediction variable column name to be
#' @param include_checks turn on some potentially handy name checks?
#' @param divider numeric constant to rescale outputs
#'
#' @return a tibble
#' @export
#'
#' @examples
#' library(fixest); library(dplyr)
#' data <- tibble(y = rnorm(10), tavg = rnorm(10), tavg2 = tavg^2)
#' m <- feols(fml = y ~ tavg + tavg2, data = data)
#' predict_poly(m, "tavg", 10, 30, 20)
#'

predict_poly_sum_lags <- function(m,
                                  var,
                                  min, max, ref,
                                  ivar_tag = "",
                                  ci_level = 95,
                                  step.length = 1,
                                  coefs = NULL,
                                  id.col = NULL,
                                  xvar_name = "temp",
                                  include_checks = FALSE,
                                  divider = NULL){



  # Extract the coefs as a matrix
  if(is.null(coefs)){
    coefs <- stringr::str_subset(names(stats::coef(m)), var)
    if(ivar_tag != "") coefs <- stringr::str_subset(coefs, ivar_tag)
  }

  beta  <- as.matrix(stats::coef(m)[coefs])

  # Store the number of coefficients (the polynomial order)
  K     <- length(coefs)

  # Store the number of (temperature) values we are going to output
  Nt    <- (max - min )/step.length + 1

  TT <- matrix(nrow = Nt, ncol = K)
  colnames(TT) <- coefs

  # Get auxilliary matrices for predictions
  if(!include_checks) message(paste0(coefs, sep = " "))

  for(kk in coefs) {

    var <- stringr::str_split(kk, "_", simplify = TRUE)

    # We need to check for the right naming scheme here, prevent bugs
    if(include_checks){
      stopifnot("should be only two _ in name" = (dim(var)[1] == 1 & dim(var)[2] == 3))
      stopifnot(
        (stringr::str_length(var[1]) <= 3 & stringr::str_detect(var[1], "l|f")) |
          ivar_tag != "")
      stopifnot(stringr::str_length(var[3]) == 2 & stringr::str_detect(var[3], "p"))
    }
    pp <- as.numeric(stringr::str_extract(var[3], "[[:digit:]]+"))
    TT[,kk] <- seq(min, max, step.length)^pp - ref^pp
  }

  beta <- as.matrix(beta[colnames(TT),] )
  # Get the predicted values
  xb  <- TT %*% beta
  ## Extract relevant portion of the covariance matrix
  sig <- stats::vcov(m)[colnames(TT), colnames(TT)]

  # Calculate SE at each t value using delta method
  se <- purrr::map_dbl(1:Nt,
                       function(tt){
                         Amat <- t(matrix(TT[tt,]))
                         sqrt(Amat %*% sig %*% t(Amat))
                       })

  # Calcutate normal approximation to critical values for rescaling SEs
  cv <- stats::qnorm((100-(100-ci_level)/2)/100)

  # Coefficient output formatting
  output <- dplyr::tibble(!!xvar_name := seq(min, max, step.length),
                          response = drop(xb), se = se)
  output$upper <- output$response + cv*se
  output$lower <- output$response - cv*se

  if(!is.null(divider))
     output <- output %>%
       dplyr::mutate(dplyr::across(c(upper, response, lower),
                                   ~ .x / divider))

  if(!is.null(id.col)) output$id <- id.col
  return(output)
}


#' Get a prediction df for a lagged response function with heterogeneity
#'
#' @param df.reg
#' @param m
#' @param treat.var
#' @param min
#' @param max
#' @param ref
#' @param het.var
#' @param lags
#'
#' @return
#' @export
#'
#' @examples
plotdf.het <- function(df.reg, m,
                       treat.var = "precip_p",
                       min = 0, max = 300, ref = 0,
                       het.var = "het",
                       lags = 4
                       ){
  purrr::map_dfr(
    unique(df.reg[[het.var]]),
    function(het.val){
      useful::predict_poly_sum_lags(m, treat.var, min, max, ref,
                            ivar_tag = paste0(het.var, '::', het.val,':'),
                            divider = lags + 1) %>%
        dplyr::mutate(het = het.val)
    }
  )
}

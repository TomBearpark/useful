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
                                  step.length = 1, coefs = NULL,
                                  id.col = NULL, xvar_name = "temp"){



  # Extract the coefs as a matrix
  if(is.null(coefs)){
    coefs <- stringr::str_subset(names(stats::coef(m)), var)
    coefs <- stringr::str_subset(coefs, ivar_tag)
  }

  beta  <- as.matrix(stats::coef(m)[coefs])

  # Store the number of coefficients (the polynomial order)
  K     <- length(coefs)

  # Store the number of (temperature) values we are going to output
  Nt    <- (max - min )/step.length + 1

  TT <- matrix(nrow = Nt, ncol = K)
  colnames(TT) <- coefs

  # Get auxilliary matrices for predictions
  for(kk in coefs) {

    var <- str_split(kk, "_", simplify = TRUE)

    # We need to check for the right naming scheme here, prevent bugs
    stopifnot("should be only two _ in name" = (dim(var)[1] == 1 & dim(var)[2] == 3))
    stopifnot(
      (str_length(var[1]) == 2 & str_detect(var[1], "l")) |
        ivar_tag != "")
    stopifnot(str_length(var[3]) == 2 & str_detect(var[3], "p"))

    pp <- as.numeric(str_extract(var[3], "[[:digit:]]+"))
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

  if(!is.null(id.col)) output$id <- id.col
  return(output)
}


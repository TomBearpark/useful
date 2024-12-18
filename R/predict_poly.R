#' Get the response function from a polynomial FE regression, with SEs
#'
#' @param m An output of a fixest regression
#' @param var The string identifier for thevariable we want to get a RF for.
#' For this to work, you need
#' to make sure that the polynomial terms are consistently named, and include
#' this string. For example, if you wnat to predict for tavg, you could run a
#' regression on tavg_p1 + tavg_p2. The function automatically selects these
#' variables from the fixest ouutput. If you want to mannualy specify the
#' coefficients to use in the prediciton, set this to NULL, and impose them in
#' the `coefs` argument
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

predict_poly <- function(m, var, min, max, ref, ci_level = 95,
                         step.length = 1, coefs = NULL,
                         id.col = NULL, xvar_name = "temp"){

  # Extract the coefs as a matrix
  if(is.null(coefs)){
    coefs <- stringr::str_subset(names(stats::coef(m)), var)
  }

  beta  <- as.matrix(stats::coef(m)[coefs])

  # Store the number of coefficients (the polynomial order)
  K     <- length(coefs)
  # Store the number of (temperature) values we are going to output
  Nt    <- (max - min )/step.length + 1

  # Get auxilliary matrices for predictions
  TT <- matrix(nrow = Nt, ncol = K)
  for(kk in 1:K) TT[,kk] <- seq(min, max, step.length)^kk - ref^kk

  # Get the predicted values
  xb <- TT %*% beta

  # Get the SE by the delta method
  ## Extract relevant portion of the covariance matrix
  sig <- stats::vcov(m)[coefs, coefs]

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

  # Compute z-statistics
  output$z <- output$response / output$se

  # Compute p-values for the test response = 0
  output$p_value <- 2 * (1 - stats::pnorm(abs(output$z)))

  if(!is.null(id.col)) output$id <- id.col
  return(output)
}



#' Predict poly for model where interactions were created using i()
#'
#' @param m
#' @param df
#' @param het.var
#' @param xvar_name
#' @param min
#' @param max
#' @param ref
#' @param ci_level
#' @param step.length
#' @param coefs
#' @param id.col
#' @param infer.range
#'
#' @return
#' @export
#'
#' @examples

predict_poly_het <- function(m, df, het.var,
                             xvar = "temp",
                             min = 0, max = 35, ref = 20,
                             ci_level = 95,
                             step.length = 1, coefs = NULL, id.col = NULL,
                             infer.range=FALSE,
                             minq=0, maxq=1, refq=0.5
                             ){

  if(infer.range){
    range <- df %>% dplyr::group_by(.data[[het.var]]) %>%
      dplyr::summarise(
        mint=quantile(.data[[paste0(xvar, "1")]], minq),
        maxt=quantile(.data[[paste0(xvar, "1")]], maxq),
        reft=quantile(.data[[paste0(xvar, "1")]], refq),
      )
  }
  het.list <- unique(df[[het.var]])
  het.list <- het.list[!is.na(het.list)]

  purrr::map_dfr(het.list, function(hh){

    if(infer.range){
      min <- range$mint[range[[het.var]]==hh]
      max <- range$maxt[range[[het.var]]==hh]
      ref <- range$reft[range[[het.var]]==hh]
    }
      useful::predict_poly(m, paste0(hh, ":", xvar),  min, max, ref,
                 ci_level = ci_level,
                 step.length = step.length, coefs = NULL,
                 id.col = id.col) %>%
      dplyr::mutate(!!het.var := as.factor(hh))
    }
  )

}

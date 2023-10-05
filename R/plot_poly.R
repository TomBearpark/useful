
#' Plot the output of a predict_poly call
#'
#' @param tab : the output of predict_poly()
#'
#' @return a ggplot object with some nice color bars
#' @export
#'
plot_rf_poly <- function(tab, xvar = "temp",
                         fill.var = 'blue',
                         facet.var = NULL,
                         scales = NULL,
                         alpha = .3){

  # Set up plot
  pp <- ggplot2::ggplot(tab, aes(x = .data[[xvar]]))  +
    ggplot2::geom_hline(yintercept = 0, color = "grey", alpha = 0.9)


  if(fill.var == 'blue'){
    pp <- pp +
      ggplot2::geom_line(ggplot2::aes(y = response)) +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data[[xvar]],
                                        ymax = upper, ymin = lower),
                           alpha = 0.5, fill = fill)
  }else{
    pp <- pp +
      ggplot2::geom_line(ggplot2::aes(y = response,
                                      color = .data[[fill.var]])) +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data[[xvar]],
                                        ymax = upper, ymin = lower,
                                        fill = .data[[fill.var]]),
                           alpha = alpha)
  }

  if(!is.null(facet.var)) pp <- pp +
      ggplot2::facet_wrap(as.formula(paste("~", facet.var)),
                          scales = scales)

  pp
}



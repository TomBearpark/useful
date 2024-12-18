
#' Plot the output of a predict_poly call
#'
#' @param tab : the output of predict_poly()
#'
#' @return a ggplot object with some nice color bars
#' @export
#'
plot_rf_poly <- function(tab,
                         xvar = "temp",
                         fill.var = 'blue',
                         fill.color = 'blue',
                         line.color = 'black',
                         facet.var = NULL,
                         alpha = .5,
                         scales = NULL,
                         nrow = NULL,
                         add_theme = FALSE,
                         nolegend = TRUE){

  # Set up plot
  pp <- ggplot2::ggplot(tab, ggplot2::aes(x = .data[[xvar]]))  +
    ggplot2::geom_hline(yintercept = 0, color = "grey", alpha = 0.9)


  if(fill.var == 'blue'){
    pp <- pp +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data[[xvar]],
                                        ymax = upper, ymin = lower),
                           alpha = alpha, fill = fill.color) +
      ggplot2::geom_line(ggplot2::aes(y = response),
                         color = line.color)
  }else{
    pp <- pp +
      ggplot2::geom_ribbon(ggplot2::aes(x = .data[[xvar]],
                                        ymax = upper, ymin = lower,
                                        fill = .data[[fill.var]]),
                           alpha = alpha)
    if(line.color == "ribbon"){
      pp <- pp +
        ggplot2::geom_line(ggplot2::aes(y = response,
                                        color = .data[[fill.var]],
                                        group = .data[[fill.var]]))
    }else{
      pp <- pp +
        ggplot2::geom_line(ggplot2::aes(y = response,
                                        group = .data[[fill.var]]),
                           color = line.color)
    }
  }

  if(!is.null(facet.var)) pp <- pp +
      ggplot2::facet_wrap(as.formula(paste("~", facet.var)),
                          scales = scales, nrow = nrow)

  if(add_theme){
    pp <- pp + ggplot2::theme(strip.background = element_blank())
  }
  if(nolegend){
    pp <- pp + ggplot2::theme(legend.position="none")
  }
  pp
}




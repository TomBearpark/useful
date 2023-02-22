
#' Plot the output of a predict_poly call
#'
#' @param tab : the output of predict_poly()
#'
#' @return a ggplot object with some nice color bars
#' @export
#'
plot_rf_poly <- function(tab, xvar = "temp"){
  ggplot2::ggplot(tab)  +
    ggplot2::geom_hline(yintercept = 0, color = "grey", alpha = 0.9) +
    ggplot2::geom_line(ggplot2::aes(x = .data[[xvar]] ,y = response)) +
    ggplot2::geom_ribbon(ggplot2::aes(x = .data[[xvar]], ymax = upper, ymin = lower), 
                         alpha=0.5, fill = "blue")
}
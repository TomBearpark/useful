
plot_rf_poly <- function(tab){
  ggplot2::ggplot(tab)  +
    ggplot2::geom_hline(yintercept = 0, color = "grey", alpha = 0.9) +
    ggplot2::geom_line(aes(x = temp ,y = response)) +
    ggplot2::geom_ribbon(aes(x = temp, ymax = upper, ymin = lower), alpha=0.5,
                fill = "blue")
}
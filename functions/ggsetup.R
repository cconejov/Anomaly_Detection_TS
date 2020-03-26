# Ploting function for anomaly observations
#Taken from: https://business-science.github.io/anomalize/articles/anomalize_methods.html


ggsetup <- function(data) {
  
  #Scale y axis
  y_axis <- max(abs(data$value)) + mean(abs(data$value))
  
  data %>%
    ggplot(aes(rank, value, color = outlier)) +
    geom_point() +
    geom_line(aes(y = limit_upper), color = "red", linetype = 2) +
    geom_line(aes(y = limit_lower), color = "red", linetype = 2) +
    geom_text(aes(label = index), vjust = -1.25) +
    theme_bw() +
    scale_color_manual(values = c("No" = "#2c3e50", "Yes" = "#e31a1c")) +
    ylim(- y_axis, y_axis) +
    theme(legend.position = "bottom")
}
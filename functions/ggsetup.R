# ploting function for anomaly plots
# Taken from: file:///C:/Users/CCONE/Documents/R/win-library/3.6/anomalize/doc/anomalize_methods.html



ggsetup <- function(data) {
  data %>%
    ggplot(aes(rank, value, color = outlier)) +
    geom_point() +
    geom_line(aes(y = limit_upper), color = "red", linetype = 2) +
    geom_line(aes(y = limit_lower), color = "red", linetype = 2) +
    geom_text(aes(label = index), vjust = -1.25) +
    theme_bw() +
    scale_color_manual(values = c("No" = "#2c3e50", "Yes" = "#e31a1c")) +
    expand_limits(y = 13) +
    theme(legend.position = "bottom")
}
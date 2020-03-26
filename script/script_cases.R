#libraries

library("tidyverse")
library("tibbletime")
library("anomalize")
library("lubridate")
library("quantmod")  #getsymbols
library("tbl2xts")

################
## 1 Financial series. Tesla shares prices

getSymbols("TSLA", from = "2019-01-01", src='yahoo')
tbl_TSLA_Adj <- TSLA$TSLA.Adjusted %>% 
                  xts_tbl()

tbl_TSLA_Adj$index <- as.numeric(rownames(tbl_TSLA_Adj))
  
## Time serie
ggplot(data = tbl_TSLA_Adj, aes(x = date, y = TSLA.Adjusted)) +
  geom_line(color = "indianred3", 
            size=1 ) +
  geom_smooth(method = "loess" ) +
  scale_x_date(date_labels = "%b/%Y") +
  labs(title = "shares prices Tesla",
       subtitle = "2019 to 2020",
       x = "",
       y = "Price shares USD") +
  theme_minimal()

# This serie display considerable variation, especially since 2020. Applying anomalize
#  model does not seem to be reasonable, because all the ending section will be considered
# as outlier:


## Decomposition Method

# STL
tbl_TSLA_Adj %>%
  time_decompose(TSLA.Adjusted, 
                 method    = "stl",
                 frequency = "1 week") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  labs(title = "Anomalies Tesla shares prices",
       subtitle = "Method STL Decomposition")
#> frequency = 5 days
#> trend = 63 days


acf(tbl_TSLA_Adj$TSLA.Adjusted)
# All values shown are “significantly far from zero,” 
# and the only pattern is perhaps a linear decrease with increasing lag

# we can try with the difference of the serie


## DIFFERENCE
tbl_TSLA_Adj <- tbl_TSLA_Adj %>%
  mutate( TSLA.Adjusted.Diff = c(0,diff(TSLA.Adjusted)))


tbl_TSLA_Adj %>%
  time_decompose(TSLA.Adjusted.Diff, 
                 method    = "stl",
                 frequency = "1 week") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  labs(title =  "Anomalies Tesla shares for the Difference prices",
       subtitle = "Method: STL Decomposition")

# Difference of logarithms
tbl_TSLA_Adj <- tbl_TSLA_Adj %>%
  mutate( TSLA.Adjusted.Diff.Log = c(0,diff(log(TSLA.Adjusted))))


tbl_TSLA_Adj %>%
  time_decompose(TSLA.Adjusted.Diff.Log, 
                 method    = "stl",
                 frequency = "1 week") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  labs(title =  "Anomalies Tesla shares for the Difference of the log prices",
       subtitle = "Method: STL Decomposition")


# Twitter
tbl_TSLA_Adj %>%
  time_decompose(TSLA.Adjusted.Diff.Log, 
                 method    = "twitter",
                 frequency = "1 week") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  labs(title =  "Anomalies Tesla shares for the Difference of the log prices",
       subtitle = "Method: Twitter Decomposition")
  
#> frequency = 5 days
#> median_span = 61 days




TSLA_iqr_outliers <- iqr(tbl_TSLA_Adj$TSLA.Adjusted.Diff.Log, 
                         alpha = 0.05, 
                         max_anoms = 0.2, 
                         verbose = TRUE)$outlier_report

source("functions/ggsetup.R")


TSLA_iqr_outliers %>% 
  ggsetup() +
  ggtitle("IQR: Top outliers sorted by rank") 



report_outlier <-  tbl_TSLA_Adj %>% inner_join(TSLA_iqr_outliers)


report_outlier %>%
  select(date,index, TSLA.Adjusted, TSLA.Adjusted.Diff.Log, rank, outlier) %>% 
  filter(outlier == "Yes") %>% 
  arrange(rank)



TSLA_gesd_outliers <- gesd(tbl_TSLA_Adj$TSLA.Adjusted, 
                           alpha = 0.05, 
                           max_anoms = 0.2, 
                           verbose = TRUE)$outlier_report


TSLA_gesd_outliers %>% 
  ggsetup() +
  ggtitle("GESD: Top outliers sorted by rank") 


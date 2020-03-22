# Examples

#libraries

library("tidyverse")
library("tibbletime")
library("anomalize")
library("lubridate")
library("quantmod")
library("tbl2xts")
#library("TSA")

##############
## 1 Temperature TSA

data(tempdub)
tbl_tempdub <- ts_tbl(tempdub)

tbl_tempdub %>%
  time_decompose(value, 
                 method    = "STL") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("STL Decomposition")

################
## 2 Financial series. Tesla stock prices

getSymbols("TSLA", from = "2019-01-01", src='yahoo')
tbl_TSLA_Adj <- TSLA$TSLA.Adjusted %>% 
                  xts_tbl()


## Time serie
ggplot(data = tbl_TSLA_Adj, aes(x = date, y = TSLA.Adjusted)) +
  geom_line(color = "indianred3", 
            size=1 ) +
  geom_smooth(method = "loess" ) +
  scale_x_date(date_labels = "%b/%Y") +
  labs(title = "Stock prices Tesla",
       subtitle = "2019 to 2020",
       x = "",
       y = "Price stock USD") +
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
  ggtitle("Anomalies Tesla Stock prices using STL Decomposition")
#> frequency = 5 days
#> trend = 63 days


acf(tbl_TSLA_Adj$TSLA.Adjusted)
# All values shown are “significantly far from zero,” 
# and the only pattern is perhaps a linear decrease with increasing lag

# we can try with the difference of the serie

tbl_TSLA_Adj <- tbl_TSLA_Adj %>%
  mutate( TSLA.Adjusted.Diff = c(0,diff(TSLA.Adjusted)))


tbl_TSLA_Adj %>%
  time_decompose(TSLA.Adjusted.Diff, 
                 method    = "stl",
                 frequency = "1 week") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Anomalies Tesla Stock prices using STL Decomposition")


tbl_TSLA_Adj <- tbl_TSLA_Adj %>%
  mutate( TSLA.Adjusted.Diff.Log = c(0,diff(log(TSLA.Adjusted))))


tbl_TSLA_Adj %>%
  time_decompose(TSLA.Adjusted.Diff.Log, 
                 method    = "stl",
                 frequency = "1 week") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Anomalies Tesla Stock prices using STL Decomposition")


# Twitter
p2 <- tbl_TSLA_Adj %>%
  time_decompose(TSLA.Adjusted, 
                 method    = "twitter") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Twitter Decomposition")
#> frequency = 5 days
#> median_span = 61 days

# Show plots
p1
p2


TSLA_iqr_outliers <- iqr(tbl_TSLA_Adj$TSLA.Adjusted, alpha = 0.05, max_anoms = 0.2, verbose = TRUE)$outlier_report

source("functions/ggsetup.R")


p3 <- TSLA_iqr_outliers %>% 
  ggsetup() +
  ggtitle("IQR: Top outliers sorted by rank") 
p3



TSLA_gesd_outliers <- gesd(tbl_TSLA_Adj$TSLA.Adjusted, alpha = 0.05, max_anoms = 0.2, verbose = TRUE)$outlier_report


p4 <- TSLA_gesd_outliers %>% 
  ggsetup() +
  ggtitle("GESD: Top outliers sorted by rank") 
p4
#####r walk#####

data(rwalk)

tbl_rwalk <- ts_tbl(rwalk)

tbl_rwalk %>%
  time_decompose(value, 
                 method    = "STL") %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("Twitter Decomposition")

library("tidyverse")
library("anomalize")
library("tibbletime")

ATM101 <- read.csv("data/Cajero101.csv", header = F, dec = ".", sep = ";")
n <- length(ATM101)

#Original Serie

ATM101 <- t(ATM101)/560
plot(ATM101, type = 'l', main = "Money retired from ATM", ylab = "Money (USD)")

acf(ATM101)

## No pattenrs/ Smooth the serie

ATM101_Smooth <- diff(log(ATM101))

plot(ATM101_Smooth, type="l", main = "Difference of Logarithms for money retired from ATM", 
     ylab = "Difference of log(Money (USD))")


## tbl difference
acf(ATM101_Smooth)


End_Date <- as.Date("2008-01-01") + n - 2
dates <- create_series('2008-01-01' ~ End_Date, 'daily')

tbl_ATM101_Smooth <- cbind(dates, ATM101_Smooth )
colnames(tbl_ATM101_Smooth) <- c("date", "Dif_Log_Money")

colnames(tbl_ATM101_Smooth)

tbl_ATM101_Smooth <- as_tbl_time(tbl_ATM101_Smooth, index = date )

tbl_ATM101_Smooth %>%
  time_decompose(Dif_Log_Money,
                 frequency = "1 year" ) %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("STL Decomposition")

## applyng over the original data set


End_Date_o <- as.Date("2008-01-01") + n - 1
dates_o <- create_series('2008-01-01' ~ End_Date_o, 'daily')

ATM101_index <- cbind(dates_o, ATM101  )
colnames(ATM101_index) <- c("date", "Money")
ATM101_tbl_time <- as_tbl_time(ATM101_index, index = date )

ATM101_tbl_time %>%
  time_decompose(Money,
                 frequency = "1 year" ) %>%
  anomalize(remainder) %>%
  plot_anomaly_decomposition() +
  ggtitle("STL Decomposition")



library(usethis)
library(tidyverse)
library(riingo)
library(tidyquant)
library(PerformanceAnalytics)
library(quantmod)
library(PortfolioAnalytics)
library(ggthemes)
library(lubridate)

riingo_set_token(Sys.getenv("RIINGO_TOKEN"))
riingo_get_token()
from <- "2018-10-01"


symbols <- c("batusd", "ethusd", "btcusd", "trxusd")

crypto_data <- riingo_crypto_prices(symbols,
                                    start_date = from,
                                    resample_frequency = "1day") %>%
  group_by(ticker)

crypto_graphs <- crypto_data %>%
  ggplot(aes(x = date, y = close)) +
  geom_line() +
  geom_bbands(aes(high = high, low = low, close = close), ma_fun = SMA, n = 30, show.legend = TRUE) +
  theme_economist() +
  scale_color_hc()+facet_wrap(~ ticker, ncol = 2, scales = "free") +
  labs(title = "SMA + BBands", y = "Price in USD", x = "Date")
crypto_graphs

crypto_returns <- crypto_data %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "Returns")  %>%
  select(ticker, date, Returns) %>%
  spread(key = ticker, value = Returns)

crypto_returns2 <- crypto_data %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = "daily",
               col_rename = "Returns")  %>%
  select(ticker, date, Returns)
crypto_returns2 <- na.omit(crypto_returns2)
crypto_return_graph <- ggplot(data = crypto_returns2, aes(x = date, y = Returns)) +
  geom_line() +
  geom_hline(yintercept = 0, colour = "red") +
  theme_economist() +
  facet_wrap(~ ticker, ncol = 2, scales = "free") +
  labs(title = "Daily Returns", x = "Date")
crypto_return_graph

crypto_returns <- na.omit(crypto_returns)
crypto_returns_total <- crypto_returns %>%
  transmute(total_returns = rowSums(crypto_returns[,2:length(crypto_returns)]))

crypto_histogram <- ggplot(data = crypto_returns_total, aes(x = total_returns)) +
  geom_histogram() +
  theme_economist() +
  geom_vline(xintercept = mean(crypto_returns_total$total_returns))+
  labs(title = "Daily Returns of Equal Weight Portfolio From October 2018 - Present", x = "Daily Return")
crypto_histogram


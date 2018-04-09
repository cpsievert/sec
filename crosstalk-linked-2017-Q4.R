library(readr)
library(dplyr)
library(lubridate)
library(crosstalk)
library(tidyr)
library(plotly)

dec17 <- read_csv("individual_security_exchange_2017_q4/dec17.csv")
dec17$Date <- ymd(dec17$Date)

# IDEA: dropdowns to choose exhange (e.g. NYSE, etc) & security (e.g. stock, etf)
# type, then direct/indirect manipulation to query tickers
# TODO: table to translate tickers?
d <- dec17 %>%
  filter(Exchange == "NYSE", Security == "Stock") %>%
  gather(variable, value, McapRank:`TradeVolForHidden('000)`) %>%
  mutate(txt = paste(Ticker, "had", value, variable, "on", Date))

p <- ggplot(SharedData$new(d, ~Ticker, "Select a ticker")) +
  geom_line(aes(Date, value, group = Ticker, text = txt)) +
  facet_wrap(~variable, scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_bw()

ggplotly(p, height = 600, width = 1100, dynamicTicks = "x", tooltip = "text") %>%
  toWebGL() %>%
  highlight(dynamic = TRUE, persistent = TRUE, selectize = TRUE) %>%
  layout(title = "NYSE stocks (Dec 2017)", margin = list(t = 75))


# skimr::skim(dec17)

#dec17 %>%
#  filter(Exchange == "NYSE", Security == "Stock") %>%
#  group_by(Ticker) %>%
#  plot_ly(x = ~Date, y = ~McapRank) %>%
#  add_lines() %>%
#  toWebGL() %>%
#  rangeslider()

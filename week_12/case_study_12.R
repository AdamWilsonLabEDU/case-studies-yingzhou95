#install.packages("xts")
#install.packages("dygraphs")
#install.packages("openmeteo")
#install.packages("htmlwidgets")
#install.packages("widgetframe")
library(xts)
library(dygraphs)
library(openmeteo)
library(tidyverse)
library(htmlwidgets)
library(widgetframe)
library(dplyr)

# download recent daily weather data for UB
d<- weather_history(c(43.00923265935055, -78.78494250958327),start = "2023-01-01",end=today(),
                    daily=list("temperature_2m_max","temperature_2m_min","precipitation_sum")) %>% 
  mutate(daily_temperature_2m_mean=(daily_temperature_2m_max+daily_temperature_2m_min)/2)

# convert d into xts time series object
d_xts_temp <- xts(select(d,
                  daily_temperature_2m_max,
                  daily_temperature_2m_min,
                  daily_temperature_2m_mean),
                  order.by=d$date)

d_xts_precipitation <- xts(select(d,
                           daily_precipitation_sum),
                           order.by=d$date)

# plot

temp_graph <- dygraph(d_xts_temp,
                      main = "Daily Maximum Temperature in Buffalo, NY") %>%
  dySeries("daily_temperature_2m_max", label = "Max Temperature") %>%
  dySeries("daily_temperature_2m_min", label = "Min Temperature") %>%
  dySeries("daily_temperature_2m_mean", label = "Mean Temperature") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", "2024-10-31"))
temp_graph

percipitation_graph <- dygraph(d_xts_precipitation,
                               main = "Daily Precipitation in Buffalo, NY") %>%
  dyRangeSelector(dateWindow = c("2023-01-01", "2024-10-31"))
percipitation_graph


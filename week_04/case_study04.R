# load packages
#install.packages("nycflights13")
library(tidyverse)
library(nycflights13)
library(maps)

# look at contents of each table in the dataset
#view(airlines)
view(airports)
view(flights)
#view(planes)
#view(weather)

# find the destination airport farthest from any of the NYC airports
farthest_airport <- flights %>% 
  filter(dest %in% c("JFK", "LGA", "EWR", "SWF", "ISP")) %>%
  arrange(desc(distance)) %>%
  left_join(airports, by = c("dest" = "faa" )) %>%
  slice(1) %>%
  select(name) %>%
  mutate(destName = as.character(name))

view(farthest_airport)

# calculate mean value of delay time by destinantion
delay_mean_bydest <- flights %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  group_by(name, lat, lon) %>%
  summarize(mean_delays = mean(arr_delay, na.rm = TRUE))

view(delay_mean_bydest)

# create the map
ggplot(delay_mean_bydest, aes(lon, lat)) +
  borders("state") +
  geom_point(aes(color = mean_delays)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0,
                        name = "Mean Delay (minutes)") +
  coord_quickmap() +
  labs(x = "lon", y = "lat")


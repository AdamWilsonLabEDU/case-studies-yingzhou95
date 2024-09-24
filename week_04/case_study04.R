# load packages
#install.packages("nycflights13")
library(tidyverse)
library(nycflights13)

# look at contents of each table in the dataset
view(airlines)
view(airports)
view(flights)
view(planes)
view(weather)

# find the destination airport farthest from any of the NYC airports
farthest_airport <- flights %>% 
  filter(dest %in% c("JFK", "LGA", "EWR", "SWF", "ISP")) %>%
  arrange(desc(distance)) %>%
  left_join(airports, by = c("dest" = "faa" )) %>%
  slice(1) %>%
  select(name)

view(farthest_airport)








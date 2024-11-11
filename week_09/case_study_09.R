#install.packages("ggmap")
#install.packages("lubridate")

library(sf)
library(tidyverse)
library(ggmap)
library(spData)
library(lubridate)
data(world)
data(us_states)

# Download a csv from noaa with storm track information
dataurl="https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r01/access/csv/ibtracs.NA.list.v04r01.csv"

storm_data <- read_csv(dataurl)

# Wrangle the data
storm_data <- storm_data %>%
  mutate(year = year(ISO_TIME)) %>% #extract the number of year from the column of ISO_TIME
  filter( year >= 1950) %>%
  mutate_if(is.numeric, function(x) ifelse(x==-999.0,NA,x)) %>%
  mutate(decade = floor(year / 10) * 10)  #get the decade number for each year 
  
storms <- storm_data %>%
  st_as_sf(coords=c("LON","LAT"),crs=4326)

region <- st_bbox(storms) #identify the bounding box of the storm data

# Quantify how many storms have hit each of the United States.
ggplot() +
  geom_sf(data = world, fill = "lightgray") +
  stat_bin2d(data=storms, #count observation under resolution of 10*10
             aes(y=st_coordinates(storms)[,2], x=st_coordinates(storms)[,1]),
             bins=100) +
  scale_fill_distiller(palette="YlOrRd",  #set the color ramp
                      trans="log", direction=-1, #high value - yellow; low value -red
                      breaks = c(1,10,100,1000)) +
  coord_sf(ylim = region[c(2,4)], xlim = region[c(1,3)]) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~decade)

# Calculate table of the five states with most storms.
states <- us_states %>%
  st_transform(st_crs(storms)) %>%
  select(state = NAME)

# Perform a spatial join between the storm database and the states object
storm_states <- st_join(storms, states, join = st_intersects,left = F) %>%
  group_by(state) %>%
  summarize(storms=length(unique(NAME))) %>%
  arrange(desc(storms)) %>%
  slice(1:5)
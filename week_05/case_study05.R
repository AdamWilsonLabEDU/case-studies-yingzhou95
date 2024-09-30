# load package
# install.packages("spData")
# install.packages("sf")
#install.packages("units")
library(spData)
library(sf)
library(tidyverse)
library(units)

#load 'world' data from spData package
data(world)  
# load 'states' boundaries from spData package
data(us_states)
# plot(world[1])  #plot if desired
# plot(us_states[1]) #plot if desired

# Define Albers Equal Area projection string
albers="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Generate Canada areas
# Filter the world ara to only include Canada
Canada <- world %>%  
  filter(name_long == "Canada")

# Project to Albers Equal Area projection
Canada_albers <- st_transform(Canada, crs = albers)

# Set 10,000 meters (10km) buffer area
Canada_Buffer <- st_buffer(Canada_albers, dist = 10000)

# Generate New York area
NY <- us_states %>%
  filter(NAME == "New York") %>%
  st_transform(crs = albers)

#  Create border area and calculate
border_area <- st_intersection(Canada_Buffer, NY) 
border_area_size <- st_area(border_area) %>%
  set_units(km^2)
border_area <- border_area %>%
  mutate(Area_KM2 = border_area_size) 

# Plot the border area and NY
ggplot() +
  geom_sf(data = NY, fill = "lightgrey") +
  geom_sf(data = border_area, fill = "red") +
  labs(title = "New York Land within 10km",
       xlab("Latitude"),
       ylab("Longtitude"),
       caption = "Red Area: Cananda Buffer Area; Light Grey Area: New York State")
    
# Build a leaflet map
install.packages("leaflet")
library(leaflet)





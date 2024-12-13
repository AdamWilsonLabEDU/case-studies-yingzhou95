# install.packages("terra")
# install.packages("spData")
# install.packages("ncdf4")

library(terra)
library(spData)
library(tidyverse)
library(sf)

library(ncdf4)
download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc","crudata.nc", method = "curl")

# read in the data using the rast() function from the terra package
tmean=rast("crudata.nc")

# inspect the object
tmean
view(tmean)
plot(tmean)

# Calculate the maximum temperature of pixels
max_tmean <- max(tmean, na.rm = TRUE)
plot(max_tmean)

# identify the maximum temperature observed in each country 
data(world)
extract_max_tmean <- terra::extract(max_tmean, world, fun=max, na.rm=T, small=T)
world_clim <- bind_cols(world, extract_max_tmean)

# plot the maxium temperature
ggplot() +
  geom_sf(data = world_clim, aes(fill = max)) +
  scale_fill_viridis_c(name = "Maximum\nTemperature (C)") +
  theme(legend.position = 'bottom')

# summary
hottest_continents <- world_clim %>%
  group_by(continent) %>%
  top_n(1) %>%
  select(name_long, continent, max) %>%
  arrange(desc(max)) %>%
  st_set_geometry(NULL)




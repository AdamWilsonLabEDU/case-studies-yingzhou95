# install.packages("mapview")
# install.packages("foreach")
# install.packages("doParallel")
library(tidyverse)
library(spData)
library(sf)
library(mapview) 
library(foreach)
library(doParallel)
library(dplyr)

registerDoParallel(4)
getDoParWorkers()

# install.packages("tidycensus")
library(tidycensus)
census_api_key("0f564f96122378fc727f225363a3b1ecfa11db4f",  overwrite=TRUE)

# Defining race variables
race_vars <- c(
  "Total Population" = "P1_001N",
  "White alone" = "P1_003N",
  "Black or African American alone" = "P1_004N",
  "American Indian and Alaska Native alone" = "P1_005N",
  "Asian alone" = "P1_006N",
  "Native Hawaiian and Other Pacific Islander alone" = "P1_007N",
  "Some Other Race alone" = "P1_008N",
  "Two or More Races" = "P1_009N"
)

# Download data 
options(tigris_use_cache = TRUE)
erie <- get_decennial(geography = "block", variables = race_vars, year=2020,
                      state = "NY", county = "Erie County", geometry = TRUE,
                      sumfile = "pl", cache_table=T) 

# crop the county-level
erie_crop <- st_crop(erie, 
  xmin = -78.9, xmax = -78.85, 
  ymin = 42.888, ymax = 42.92)

erie_crop$variable <- factor(erie_crop$variable)

# Run a foreach loop to create random points
racialGroup_points <- foreach(race = levels(erie_crop$variable), 
                              .combine = rbind,
                              .packages = c("dplyr", "sf")) %dopar% {
                                erie_crop %>%
                                  filter(variable == race) %>%
                                  st_sample(size = .$value, exact = TRUE) %>%
                                  st_as_sf() %>%
                                  mutate(variable = race)
                              }

# Visualization
mapview(racialGroup_points, 
        zcol = "variable", 
        cex = 1,
        alpha = 0.7,
        stroke = FALSE,
        legend = TRUE )
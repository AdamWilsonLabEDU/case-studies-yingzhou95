# install.packages("reprex")
# install.packages("spData")
# install.packages("world")

library(tidyverse)
library(reprex)
library(sf)

library(spData)
data(world)

ggplot(world,aes(x=gdpPercap, y=continent, color=continent))+
   geom_density(alpha=0.5,color=F)

reprex::reprex(venue = "gh", {
  library(tidyverse)
  library(sf)
  library(spData)
  
  data(world)
  
  ggplot(world, aes(x = gdpPercap, y = continent, fill = continent)) +
    geom_density_ridges(alpha = 0.5) })
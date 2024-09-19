
# load packages
# install.packages("gapminder")
library(ggplot2)
library(gapminder)
library(dplyr)

# remove "Kuwait" from the gapminder
gapminder_without_Kuwait <- gapminder %>% 
  filter(country != "Kuwait" )

# plot #1 scatter plot of wealth (gdpPercap on y) and life expectancy (lifeExp on X) through time
ggplot(gapminder_without_Kuwait, aes(lifeExp, gdpPercap, 
                                     color = continent, size = pop/100000)) +
  geom_point() +
  scale_y_continuous(trans =  "sqrt") + #square root
  facet_wrap(~year,nrow = 1) +
  theme_bw() +
  labs(title = "Wealth and life expectancy through time", 
       x = "Life Expatency",
       y = "GDP Per Capital")

# save image
ggsave("Wealth and life expectancy through time.png", 
       width = 15, height = 6, units = "in")


# group by continent and year, then summarize weighter gdpPercap and sum of the population
gapminder_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop), #larger populations have more influence on the average
            pop = sum(as.numeric(pop)))


# plot #2 line graph of wealth (gdpPercap on y) through time (year on x)
ggplot() +
  # create graph for gapminder and group the points by country
  geom_point(data = gapminder, aes(year, gdpPercap, color = continent, group = country,
                            size = pop/100000)) +
  geom_line(data = gapminder, aes(year, gdpPercap, color = continent, group = country)) +
  # create graph for gapminder_continent (to explore the weighted mean values over years)
  geom_point(data = gapminder_continent, aes(year, gdpPercapweighted,
                                             size = pop/100000)) +
  geom_line(data = gapminder_continent, aes(year, gdpPercapweighted)) +
  facet_wrap(~continent, nrow = 1) +
  scale_y_continuous(trans =  "sqrt") + #square root
  theme_bw() +
  labs(title = "Wealth in diffrent continents through time", 
       x = "Year",
       y = "GDP Per Capital")

# save image
ggsave("Wealth in diffrent continents through time.png", 
       width = 15, height = 6, units = "in")
  

 


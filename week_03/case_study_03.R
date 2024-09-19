# load packages
# install.packages("gapminder")
library(ggplot2)
library(gapminder)
library(dplyr)

# remove "Kuwait" from the gapminder
gapminder_without_Kuwait <- gapminder %>% 
  filter(country != "Kuwait" )

# plot #1
ggplot(gapminder_without_Kuwait, aes(lifeExp, gdpPercap, 
                                     color = continent, size = pop/100000)) +
  geom_point() +
  scale_y_continuous(trans =  "sqrt") + #square root
  facet_wrap(~year,nrow = 1) +
  theme_bw() +
  labs(title = "Wealth and life expectancy through time", 
       x = "Year",
       y = "GDP Per Capital")



# group by continent and year, then summarize weighter gdpPercap and sum of the population
gapminder_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop), 
            pop = sum(as.numeric(pop)))

# plot #2
ggplot() +
  geom_point(data = gapminder, aes(year, gdpPercap, color = continent, group = country,
                            size = pop/100000)) +
  geom_line(data = gapminder, aes(year, gdpPercap, color = continent, group = country)) +
  geom_point(data = gapminder_continent, aes(year, gdpPercapweighted,
                                             size = pop/100000)) +
  geom_line(data = gapminder_continent, aes(year, gdpPercapweighted)) +
  scale_y_continuous(trans =  "sqrt") + #square root
  facet_wrap(~continent, nrow = 1) +
  theme_bw() +
  labs(title = "Wealth and life expectancy through time", 
       x = "Year",
       y = "GDP Per Capital")
  

 


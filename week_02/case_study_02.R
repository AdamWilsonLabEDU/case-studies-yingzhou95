# install packages
#install.packages("tidyverse")
library(tidyverse)

# define the link
dataurl <- "https://data.giss.nasa.gov/tmp/gistemp/STATIONS_v4/tmp_USW00014733_14_0_1/station.csv"

# give date with column names and define it as temp
# the next lines download the data
temp=read_csv(dataurl, 
              na="999.90", # tell R that 999.90 means missing in this dataset
              skip=1, # we will use our own column names as below so we'll skip the first row
              col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                            "APR","MAY","JUN","JUL",  
                            "AUG","SEP","OCT","NOV",  
                            "DEC","DJF","MAM","JJA",  
                            "SON","metANN"))

# Graph the annual mean temperature in June, July and August (JJA) using ggplot
ggplot(temp, aes(YEAR, JJA)) +
  geom_line()+
  geom_smooth()+
  xlab("year") + ylab("mean temprature(c)") +
  ggtitle("Mean Summer Temperature in Buffalo, New York",
          subtitle = "Summer include June, July and August")

# Save the graphic
ggsave("Mean Summer Temperature in Buffalo.png",
       width = 8, height = 5)








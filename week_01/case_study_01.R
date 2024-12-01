# load the iris dataset
data(iris)

# calculate the mean
petal_length_mean <- mean(iris$Petal.Length)

# plot the distribution of the Petal.Length
hist(iris$Petal.Length,
     main = "The distributiopn of the petal length",
     xlab = "Petal Lenth",
     ylab = "Frequency",
     col = 'blue', border = "white",
     ylim = c(0,50),
     breaks = 30)


# install ggplot2
install.packages("ggplot2")
library(ggplot2)


# use ggplot2 to create distribution
ggplot(data = iris, aes(x = Petal.Length)) + 
  geom_histogram(color = "white", fill = "grey")


#  generate a summary table of each column
summary(iris)

# summary by using str()
str(iris)




# Libraries
library(dplyr)
library(ggplot2)
# Read the data
data <- read.csv(file = "heart_statlog_cleveland_hungary_final.csv")
# Check the structure of the data
glimpse(data)
# Are there NA's?
data[!complete.cases(data),]

# EDA
ggplot(data = data, aes(x = age, color = sex)) +
  geom_histogram(binwidth = 5, color = 'black', fill = 'lightblue', position = 'dodge') +
  ggtitle("Histogram of variable age via sex (0 - female, 1 - male)") +
  facet_grid(~ sex)

ggplot(data = data, aes(x = cholesterol, color = sex)) +
  geom_histogram(color = 'black', fill = 'lightblue', position = 'dodge') +
  ggtitle("Histogram of variable cholesterol via sex (0 - female, 1 - male)") +
  facet_grid(~ sex)

ggplot(data = data, aes(x = chest.pain.type, color = sex)) +
  geom_bar(color = 'black', fill = 'lightblue') +
  ggtitle("Number of observations for every chest pain type")

ggplot(data = data, aes(x = max.heart.rate, color = sex)) +
  geom_histogram(binwidth = 10, color = 'black', fill = 'lightblue', position = 'dodge') +
  ggtitle("Histogram of variable max.heart.rate via sex (0 - female, 1 - male)") +
  facet_grid(~ sex)

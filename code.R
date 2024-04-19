# Libraries
library(dplyr)
library(ggplot2)
library(glmtoolbox)
library(generalhoslem)
library(gofcat)
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

str(data)

# There is only one observation with ST.slope 0
barplot(table(data$ST.slope))

data <- data %>%
  filter(ST.slope > 0)

barplot(table(data$resting.ecg))
# Convert nominal variables into dummies

# Variable "chest.pain.type" - base level "typical angina"
data$chest.pain.type.atypical.angina <- ifelse(data$chest.pain.type == 2, 1, 0)
data$chest.pain.type.non.anginal <- ifelse(data$chest.pain.type == 3, 1, 0)
data$chest.pain.type.asymptomatic <- ifelse(data$chest.pain.type == 4, 1, 0)

# Variable "resting.ecg" - base level "normal"
data$resting.ecg.1 <- ifelse(data$resting.ecg == 1, 1, 0)
data$resting.ecg.2 <- ifelse(data$resting.ecg == 2, 1, 0)

# Variable "St.slope" - base level "upsloping"
data$ST.slope.flat <- ifelse(data$ST.slope == 2, 1, 0)
data$ST.slope.downsloping <- ifelse(data$ST.slope == 3, 1, 0)

# Finally prepared dataset
data <- data[, -c(3, 7, 11)]

# Model

# Initial model with all variables
logit_0 <- glm(target ~ . + I(age^2) + age:max.heart.rate, data = data, family = binomial('logit'))
summary(logit_0)

# Linktest
source("linktest.R")
summary(linktest(logit_0))

# RESULT: The specification of our model is correct

# H0: Specification of the model is correct
# H1: Specification of the model is incorrect

# yhat is statistically significant - p-value < 5% (significance level alpha), 
# therefore we reject H0 in favor of H1. yhat2 is statistically
# insignificant - p-value 89% > 5% (significance level alpha), we fail to 
# reject H0.

# Hosmer-Lemeshow test
hltest(logit_0)

# RESULT: The specification of our model is correct

# H0: Specification of the model is correct
# H1: Specification of the model is incorrect

# p-value = 0.28% > 5% (significance level alpha), we fail to reject H0

# Genaral to specific approach (backward selection)

# Check if variable resting.ecg is statistically significant
# H0: resting.ecg = 0
logit_1 <- glm(target ~ age + sex + chest.pain.type + resting.bp.s, + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + ST.slope + I(age^2) + age:max.heart.rate, 
               data = data, family = binomial('logit'))
summary(logit_1)

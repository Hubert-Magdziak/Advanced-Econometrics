# Libraries
library(dplyr)
library(ggplot2)
library(glmtoolbox)
library(generalhoslem)
library(gofcat)
library(car)
library(stargazer)
library(mfx)
library(sandwich)
library(lmtest)
library(zoo)
library(MASS)
library(betareg)
# Read the data
setwd("/Users/hubertmagdziak/Desktop/Github/Advanced-Econometrics")
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

# p-value = 28% > 5% (significance level alpha), we fail to reject H0

# Genaral to specific approach (backward selection)

# Check if variable "resting.ecg.1" is statistically insignificant

# H0: resting.ecg.1 = 0
linearHypothesis(logit_0, "resting.ecg.1 = 0")
# New_model
logit_1 <- glm(target ~ age + sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.atypical.angina + chest.pain.type.non.anginal
               + chest.pain.type.asymptomatic + resting.ecg.2 
               + ST.slope.flat + ST.slope.downsloping + I(age^2) + age:max.heart.rate, 
               data = data, family = binomial('logit'))
# P-value = 95% > 5% (significance level alpha), we fail to reject H0
summary(logit_1)
# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0
linearHypothesis(logit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0"))
# Model
logit_2 <- glm(target ~ age + sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.non.anginal
               + chest.pain.type.asymptomatic + resting.ecg.2 
               + ST.slope.flat + ST.slope.downsloping + I(age^2) + age:max.heart.rate, 
               data = data, family = binomial('logit'))

# P-value = 95% > 5% (significance level alpha), we fail to reject H0
summary(logit_2)

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal" are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0
linearHypothesis(logit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0"))
# Model
logit_3 <- glm(target ~ age + sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.asymptomatic + resting.ecg.2 
               + ST.slope.flat + ST.slope.downsloping + I(age^2) + age:max.heart.rate, 
               data = data, family = binomial('logit'))

# P-value = 98% > 5% (significance level alpha), we fail to reject H0
summary(logit_3)                 

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal", "age^2" are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0 & age^2 = 0
linearHypothesis(logit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0",
                            "I(age^2) = 0"))
# Model
logit_4 <- glm(target ~ age + sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.asymptomatic + resting.ecg.2 
               + ST.slope.flat + ST.slope.downsloping + age:max.heart.rate, 
               data = data, family = binomial('logit'))

# P-value = 82% > 5% (significance level alpha), we fail to reject H0
summary(logit_4)         

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal", "age^2", "age" are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0 & age^2 = 0 & age = 0
linearHypothesis(logit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0",
                            "I(age^2) = 0",
                            "age = 0"))
# Model
logit_5 <- glm(target ~ sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.asymptomatic + resting.ecg.2 
               + ST.slope.flat + ST.slope.downsloping + age:max.heart.rate, 
               data = data, family = binomial('logit'))

# P-value = 75% > 5% (significance level alpha), we fail to reject H0
summary(logit_5)   

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal", "age^2", "age" , "resting.ecg.2"
# are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0 & age^2 = 0 & age = 0 & resting.ecg.2 = 0
linearHypothesis(logit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0",
                            "I(age^2) = 0",
                            "age = 0",
                            "resting.ecg.2 = 0"))
# Model
logit_6 <- glm(target ~ sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.asymptomatic + ST.slope.flat 
               + ST.slope.downsloping + age:max.heart.rate, 
               data = data, family = binomial('logit'))

# P-value = 65% > 5% (significance level alpha), we fail to reject H0
summary(logit_6)   

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal", "age^2", "age" , "resting.ecg.2"
# "resting.bp.s" are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0 & age^2 = 0 & age = 0 & resting.ecg.2 = 0
# resting.bp.s = 0
linearHypothesis(logit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0",
                            "I(age^2) = 0",
                            "age = 0",
                            "resting.ecg.2 = 0",
                            "resting.bp.s = 0"))
# Model
logit_7 <- glm(target ~ sex + cholesterol + fasting.blood.sugar + max.heart.rate 
               + exercise.angina + oldpeak + chest.pain.type.asymptomatic 
               + ST.slope.flat + ST.slope.downsloping + age:max.heart.rate, 
               data = data, family = binomial('logit'))

# P-value = 55% > 5% (significance level alpha), we fail to reject H0
summary(logit_7)  


# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal", "age^2", "age" , "resting.ecg.2"
# "resting.bp.s" , "ST.slope.downsloping" are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0 & age^2 = 0 & age = 0 & resting.ecg.2 = 0
# resting.bp.s = 0 & ST.slope.downsloping = 0
linearHypothesis(logit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0",
                            "I(age^2) = 0",
                            "age = 0",
                            "resting.ecg.2 = 0",
                            "resting.bp.s = 0",
                            "ST.slope.downsloping = 0"))
# Final model with all variables statistically significant

final_model <- glm(target ~ sex + cholesterol + fasting.blood.sugar + max.heart.rate 
               + exercise.angina + oldpeak + chest.pain.type.asymptomatic 
               + ST.slope.flat + age:max.heart.rate, 
               data = data, family = binomial('logit'))

# P-value = 30% > 5% (significance level alpha), we fail to reject H0
summary(final_model) 

# Check the specification

# Linktest
summary(linktest(final_model))

# RESULT: The specification of our model is correct

# H0: Specification of the model is correct
# H1: Specification of the model is incorrect

# yhat is statistically significant - p-value < 5% (significance level alpha), 
# therefore we reject H0 in favor of H1. yhat2 is statistically
# insignificant - p-value 77% > 5% (significance level alpha), we fail to 
# reject H0.


# Hosmer-Lemeshow test
hltest(final_model)

# RESULT: The specification of our model is correct

# H0: Specification of the model is correct
# H1: Specification of the model is incorrect

# p-value = 76% > 5% (significance level alpha), we fail to reject H0

# Print some of the models (including initial and final models)
stargazer(logit_0,
          logit_1,
          logit_2,
          logit_3,
          logit_4,
          logit_7,
          final_model,
          type = "text")

# Marginal effects for mean observation for the final model
logitmfx(formula = target ~ sex + cholesterol + fasting.blood.sugar + max.heart.rate 
         + exercise.angina + oldpeak + chest.pain.type.asymptomatic 
         + ST.slope.flat + age:max.heart.rate, 
         data = data, atmean = T)

#. INTERPRETATION OF MARGINAL EFFECTS

# The marginal effects are crucial for analysing results of the model.
# In the logit model we cannot interpret the coefficients quantitatively. The only
# way is the qualitative approach based on sign of the coefficient.
# Positive sign for the coefficients impose that increase of that variable 
# by 1 unit has positive influence on probability of observing success in 
# the dependent variable. However we are always keen on quantitative analysis.
# This might be reached by calculating marginal effects. Then the coefficients
# might be interpreted quantitatively.

#
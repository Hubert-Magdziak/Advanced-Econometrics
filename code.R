# --------------------------  Libraries --------------------------
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
library(DescTools)
#-----------------------------------------------------------------
# Read the data
data <- read.csv(file = "heart_statlog_cleveland_hungary_final.csv")

# Check the structure of the data
glimpse(data)

# Are there NA's?
data[!complete.cases(data),]

# ----------------------------- EDA --------------------------------
ggplot(data = data, aes(x = age, color = sex)) +
  geom_histogram(binwidth = 5, 
                 color = 'black', 
                 fill = 'lightblue', 
                 position = 'dodge') +
  ggtitle("Histogram of variable age via sex (0 - female, 1 - male)") +
  facet_grid(~ sex)

ggplot(data = data, aes(x = cholesterol, color = sex)) +
  geom_histogram(color = 'black', 
                 fill = 'lightblue', 
                 position = 'dodge') +
  ggtitle("Histogram of variable cholesterol via sex (0 - female, 1 - male)") +
  facet_grid(~ sex)

ggplot(data = data, aes(x = chest.pain.type, color = sex)) +
  geom_bar(color = 'black', 
           fill = 'lightblue') +
  ggtitle("Number of observations for every chest pain type")

ggplot(data = data, aes(x = max.heart.rate, color = sex)) +
  geom_histogram(binwidth = 10, 
                 color = 'black', 
                 fill = 'lightblue', 
                 position = 'dodge') +
  ggtitle("Histogram of variable max.heart.rate via sex (0 - female, 1 - male)") +
  facet_grid(~ sex)

# Create boxplots

df <- data[,c(2, 3, 4, 5, 8)]
df$sex <- as.factor(ifelse(df$sex == 0, "F", "M"))
df$chest.pain.type <- as.factor(df$chest.pain.type)
ggplot(data = df, aes(x = chest.pain.type, y = cholesterol, fill = sex)) +
  geom_boxplot()


# Create age pyramid for the dataset
d <- data %>%
  group_by(sex, age) %>%
  count() %>%
  ungroup()
d$sex <- as.factor(ifelse(d$sex == 0, "F", "M"))
glimpse(d)


d %>%
  mutate(population = ifelse(sex == "F", n * -1, n)) %>%
  ggplot(aes(x = age, y = population, fill = sex)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_brewer(type = "seq", palette = 7) +
  labs(y = "Population", x = "Age", title = "Age Pyramid") +
  theme_minimal()


# What is the median of variables age, max.heart.rate, cholesterol for different
# sex and status of heart disease?

ggplot(data = data, aes(x = age)) +
  geom_histogram()

data %>%
  group_by(sex, target) %>%
  summarize(medianAge = median(age),
            medianMaxHeartRate = median(max.heart.rate),
            medianCholesterol = median(cholesterol),
            Obs = n())
# Check structure of the data
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

# ----------------------------- MODEL --------------------------------
                          # Logit vs Probit


#                               LOGIT


# Initial model with all variables
logit_0 <- glm(target ~ . + I(age^2) + age:max.heart.rate, 
               data = data, 
               family = binomial('logit'))
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


#                             PROBIT


# Initial model with all variables
probit_0 <- glm(target ~ . + I(age^2) + age:max.heart.rate, 
               data = data, 
               family = binomial('probit'))
summary(probit_0)


# Linktest
summary(linktest(probit_0))

# RESULT: The specification of our model is correct

# H0: Specification of the model is correct
# H1: Specification of the model is incorrect

# yhat is statistically significant - p-value < 5% (significance level alpha), 
# therefore we reject H0 in favor of H1. yhat2 is statistically
# insignificant - p-value 58% > 5% (significance level alpha), we fail to 
# reject H0.

# Hosmer-Lemeshow test
hltest(probit_0)

# RESULT: The specification of our model is correct

# H0: Specification of the model is correct
# H1: Specification of the model is incorrect

# p-value = 65% > 5% (significance level alpha), we fail to reject H0


#                 LOGIT vs PROBIT - comparison
stargazer(logit_0, probit_0, type = "text")

# AIC & BIC for LOGIT & PROBIT

PseudoR2(logit_0, which = c("AIC", "BIC"))
PseudoR2(probit_0, which = c("AIC", "BIC"))

# The measures which we use for comparing the models are AIC and BIC 
# information criterias. They both base on errors of the model and
# penalization for number of variables used for modelliing. The probit model
# obtains AIC 867.82 and BIC 959.28, whereas for logit it is  respectively
# 869.72, 961.17. Therefore we go for probit as it has lower value of both
# AIC, BIC information criterias. Further analysis will be conducted based
# on probit.

#----------- Joint significance of the variables in the model ---------------

# Check if all the variables are jointly statistically significant
null_model <- glm(target ~ 1, data = data, family = binomial('probit'))

# Likelihood ratio test
lrtest(probit_0, null_model)

# H0: The restricted model is better
# H1: The unrestricted model is better

# Likelihood ratio test p-value < 5% (significance level alpha), we reject
# null hypothesis in favor of alternative hypothesis. The unrestricted model
# is better, therefore the variables in initial model are jointly statistically
# significant

# ----------- Genaral to specific approach (backward selection) ------------

# Check if variable "resting.ecg.1" is statistically insignificant

# H0: resting.ecg.1 = 0

linearHypothesis(probit_0, "resting.ecg.1 = 0")
# P-value = 85% > 5% (significance level alpha), we fail to reject H0

# New_model
probit_1 <- glm(target ~ age + sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.atypical.angina + chest.pain.type.non.anginal
               + chest.pain.type.asymptomatic + resting.ecg.2 
               + ST.slope.flat + ST.slope.downsloping + I(age^2) + age:max.heart.rate, 
               data = data, family = binomial('probit'))

summary(probit_1)

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0

linearHypothesis(probit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0"))
# P-value = 94% > 5% (significance level alpha), we fail to reject H0

# Model
probit_2 <- glm(target ~ age + sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.non.anginal
               + chest.pain.type.asymptomatic + resting.ecg.2 
               + ST.slope.flat + ST.slope.downsloping + I(age^2) + age:max.heart.rate, 
               data = data, family = binomial('probit'))

summary(probit_2)

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal" are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0

linearHypothesis(probit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0"))
# P-value = 98% > 5% (significance level alpha), we fail to reject H0

# Model
probit_3 <- glm(target ~ age + sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.asymptomatic + resting.ecg.2 
               + ST.slope.flat + ST.slope.downsloping + I(age^2) + age:max.heart.rate, 
               data = data, family = binomial('probit'))

summary(probit_3)                 

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal", "age^2" are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0 & age^2 = 0

linearHypothesis(probit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0",
                            "I(age^2) = 0"))
# P-value = 87% > 5% (significance level alpha), we fail to reject H0

# Model
probit_4 <- glm(target ~ age + sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.asymptomatic + resting.ecg.2 
               + ST.slope.flat + ST.slope.downsloping + age:max.heart.rate, 
               data = data, family = binomial('probit'))

summary(probit_4)         

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal", "age^2", "age" are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0 & age^2 = 0 & age = 0

linearHypothesis(probit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0",
                            "I(age^2) = 0",
                            "age = 0"))
# P-value = 71% > 5% (significance level alpha), we fail to reject H0

# Model
probit_5 <- glm(target ~ sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.asymptomatic + resting.ecg.2 
               + ST.slope.flat + ST.slope.downsloping + age:max.heart.rate, 
               data = data, family = binomial('probit'))


summary(probit_5)   

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal", "age^2", "age" , "resting.ecg.2"
# are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0 & age^2 = 0 & age = 0 & resting.ecg.2 = 0

linearHypothesis(probit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0",
                            "I(age^2) = 0",
                            "age = 0",
                            "resting.ecg.2 = 0"))
# P-value = 54% > 5% (significance level alpha), we fail to reject H0

# Model
probit_6 <- glm(target ~ sex + resting.bp.s + cholesterol
               + fasting.blood.sugar + max.heart.rate + exercise.angina
               + oldpeak + chest.pain.type.asymptomatic + ST.slope.flat 
               + ST.slope.downsloping + age:max.heart.rate, 
               data = data, family = binomial('probit'))


summary(probit_6)   

# Check if variables "resting.ecg.1", "chest.pain.type.atypical.angina" 
# "chest.pain.type.non.anginal", "age^2", "age" , "resting.ecg.2"
# "resting.bp.s" are jointly statistically insignificant

# H0: resting.ecg.1 = 0 & chest.pain.type.atypical.angina = 0 &
# chest.pain.type.non.anginal = 0 & age^2 = 0 & age = 0 & resting.ecg.2 = 0
# resting.bp.s = 0

linearHypothesis(probit_0, c("resting.ecg.1 = 0", 
                            "chest.pain.type.atypical.angina = 0",
                            "chest.pain.type.non.anginal = 0",
                            "I(age^2) = 0",
                            "age = 0",
                            "resting.ecg.2 = 0",
                            "resting.bp.s = 0"))
# P-value = 36% > 5% (significance level alpha), we fail to reject H0

# Final model with all variables statistically significant

final_model <- glm(target ~ sex + cholesterol + fasting.blood.sugar + max.heart.rate 
               + exercise.angina + oldpeak + chest.pain.type.asymptomatic 
               + ST.slope.flat + ST.slope.downsloping + age:max.heart.rate, 
               data = data, family = binomial('probit'))

summary(final_model)  


# Check the specification

# Linktest
summary(linktest(final_model))

# RESULT: The specification of our model is correct

# H0: Specification of the model is correct
# H1: Specification of the model is incorrect

# yhat is statistically significant - p-value < 5% (significance level alpha), 
# therefore we reject H0 in favor of H1. yhat2 is statistically
# insignificant - p-value 56% > 5% (significance level alpha), we fail to 
# reject H0.


# Hosmer-Lemeshow test
hltest(final_model)

# RESULT: The specification of our model is correct

# H0: Specification of the model is correct
# H1: Specification of the model is incorrect

# p-value = 16% > 5% (significance level alpha), we fail to reject H0

# Print some of the models (including initial and final models)
stargazer(probit_0,
          probit_1,
          probit_3,
          probit_4,
          probit_6,
          final_model,
          type = "text")

# Marginal effects for mean observation for the final model
probitmfx(formula = target ~ sex + cholesterol + fasting.blood.sugar + max.heart.rate 
         + exercise.angina + oldpeak + chest.pain.type.asymptomatic 
         + ST.slope.flat + age:max.heart.rate, 
         data = data, atmean = T)

#. INTERPRETATION OF MARGINAL EFFECTS FOR MEAN

# The marginal effects are crucial for analyzing results of the model.
# In the logit model we cannot interpret the coefficients quantitatively. The only
# way is the qualitative approach based on sign of the coefficient.
# Positive sign for the coefficients impose that increase of that variable 
# by 1 unit has positive influence on probability of observing success in 
# the dependent variable. However we are always keen on quantitative analysis.
# This might be reached by calculating marginal effects. Then the coefficients
# might be interpreted quantitatively.

# Variable "sex"

# A man with average characteristics have about 32.5 p.p higher probability
# of having heart disease than woman ceteris paribus.


# Average marginal effects for the final model
probitmfx(formula = target ~ sex + cholesterol + fasting.blood.sugar + max.heart.rate 
         + exercise.angina + oldpeak + chest.pain.type.asymptomatic 
         + ST.slope.flat + age:max.heart.rate, 
         data = data, atmean = F)

# INTERPRETATION OF AVERAGE MARGINAL EFFECTS

# Variable "sex"

# On average men have about 18 p.p higher probability of having heart disease
# than women ceteris paribus.

#---------------------------- Performance --------------------------------

# Measures to assess performance of the model - McKelveyZavoina
PseudoR2(final_model, which = "all")

# McKelveyZavoina = 67.2%
# If a hidden variable would be observed, the model would explain about 67.2% of 
# its total variation.

# Count R2

# Define function countR2
countR2<-function(m) mean(m$y==round(m$fitted.values))

# Use the function countR2 on the final model
countR2(final_model)

# About 85.2% of observations were predicted correctly by the model

# Adjusted Count R2

# Define function adj.countR2
adj.countR2<-function(m) {
  n<-length(m$y)
  k<-max(table(m$y))
  correct<-table(m$y==round(m$fitted.values))[["TRUE"]]
  (correct-k)/(n-k)
}

# Use the function adj.countR2 on the final model
adj.countR2(final_model)

# About 68.6% of the observations were predicted based only on the
# variables used for modelling


# VIF
vif(final_model)

# For all variables in our final model Variance Inflation Factor (VIF) is 
# not higher than 1.77. Therefore the assumption about lack of multicollinearity
# in the model is fulfilled.



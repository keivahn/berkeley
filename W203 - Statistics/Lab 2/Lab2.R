# Alex Smith
# W203 - Exploring and Analyzing Data
# Lab 2

# load packages, ggplot2 and car, install if don't have
library(ggplot2)
library(car)

# set working directory to make it easier to pull appropriate files
setwd("~/Documents/MIDS/Spring14/W203/Lab2")

# load the General Social Survey data in a data frame
load("GSS.Rdata")

# get a look at the data as a whole
head(GSS)

# examine the agewed variable (the age at which couples have married)
summary(GSS$agewed)
table(GSS$agewed)

# recode values of 0 and 99 in agewed variable to "NA"
GSS$agewed[GSS$agewed==0] <- NA
GSS$agewed[GSS$agewed==99] <- NA

# calculate the mean agewed value, excluding NA's
mean_agewed = mean(GSS$agewed, na.rm = TRUE)
mean_agewed

# create a QQ plot for the agewed variable
qq_agewed <- qplot(sample = GSS$agewed, stat = "qq") + ggtitle("QQ Plot of agewed Variable")
qq_agewed

# run a Shapiro-Wilk test to test for normality
shapiro.test(GSS$agewed)

# run a Shapiro-Wilk test, separating out men and women
# by(GSS$agewed,GSS$sex,shapiro.test)
# commented out because unnecessary for prompt

# calculate the variance of agewed by sex
by(GSS$agewed,GSS$sex,var,na.rm=TRUE)

# perform a Levene's test on the agewed variable by gender
leveneTest(GSS$agewed,GSS$sex)

# perform a z-test
# assume that the population standard deviation is 5
# null hypothesis: mean of agewed = 23
# alternative hypothesis: mean of agewed != 23

# calculate a z-score using the formula z = (theorized mean - calculated mean)/(sigma/sqrt(sample size))
theorized_mean = 23
calculated_mean = mean(GSS$agewed,na.rm=TRUE)
z_numerator = theorized_mean - calculated_mean
sigma = 5
sample_size = sum(GSS$agewed != "NA", na.rm = TRUE)
z_denominator = sigma/(sqrt(sample_size))

z_score = z_numerator/z_denominatorz

# use the z-score to calculate the probability of calculated average given the null hypothesis
p_value = (1-pnorm(z_score))*2
p_value


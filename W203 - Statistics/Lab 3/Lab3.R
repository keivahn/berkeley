# Alex Smith
# W203 - Lab 3

# set the working directory
setwd("~/Documents/MIDS/Spring14/W203/Lab3")

# load the dataset & examine the first few rows
load("GSS.Rdata")
head(GSS)

# Task 1 (q.14): perform a chi-square test of independence for
# marital status and political orientation

# determine if there are any weird values that we should turn to NAs
summary(GSS$marital)
summary(GSS$politics)

levels(GSS$marital)
levels(GSS$politics)

# remove the NA in marital
GSS$marital[GSS$marital=="NA"]= NA
GSS$marital <- factor(GSS$marital)

# load the gmodels package
library("gmodels")

# perform chi-square test
CrossTable(GSS$marital, GSS$politics, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

# determine the effect size by using a cramer's v function
marital_politics = chisq.test(GSS$marital, GSS$politics)
marital_politics

# source of crammers's v function: W203 Async material
crammers_v = function(chisquare)
{
	crammersv = sqrt(chisquare$statistic / (sum(chisquare$observed) * (min(dim(chisquare$observed))-1)))
	print.noquote("Crammer's V: ")
	return(as.numeric(crammersv))
}

crammers_v(marital_politics)

# Task 2 (q.15) conduct a Pearson's correlation analysis to examine the association between 
# age when married (agewed) and hours of tv watched (tvhours)

# load the Hmisc package so we can test p-values
library("Hmisc")

# conduct the Pearson's correlation analysis, remembering to turn the data into a matrix format
rcorr(as.matrix(GSS[,c("agewed","tvhours")]))


# Task 3 (q.16), Conduct a Wilcox rank-sum test to determine whether 
# your new “married” variable is associated with the number of children (childs) 
# for respondents who are 23 years old.

# create a dummy variable, married, to note if a person is currently married
GSS$married <- ifelse(GSS$marital == "married",1,0)

# mean of married variable for those aged 23 (i.e. portion of sample that is married and 23)
mean(subset(GSS, age == 23, select=married)$married, na.rm = TRUE)

# wilcox rank sum test
married_23 <- wilcox.test(childs ~ married, data = subset(GSS, age == 23), paired = FALSE, na.action = na.exclude)
married_23

# calculate effect size
# source of r function from textbook, p.656
rFromWilcox <- function(wilcoxModel, N)
{
	z <- qnorm(wilcoxModel$p.value/2)
	r <- z / sqrt(N)
	cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

GSS$twenty3 <- ifelse(GSS$age == 23, 1, 0)
TwentyThreeSampleSize = sum(GSS$twenty3)

rFromWilcox(married_23,TwentyThreeSampleSize)

# Task 4 (q.17), Conduct an analysis of variance to determine if 
# there is an association between religious affiliation (relig) and 
# age when married (agewed).

# check variables for anything weird
table(GSS$agewed)
summary(GSS$agewed)

table(GSS$relig)
summary(GSS$relig)

# recode values of 0 and 99 in agewed variable to "NA"
GSS$agewed[GSS$agewed==0] <- NA
GSS$agewed[GSS$agewed==99] <- NA

# recode values of DK and NA in relig variable to "NA"
GSS$relig[GSS$relig=="DK"] <- NA
GSS$relig[GSS$relig=="NA"] <- NA
GSS$relig <- factor(GSS$relig)

# we use Levene's test to test homogeneity of variance
library("car")
leveneTest(GSS$agewed,GSS$relig)

# because Levene's test is not significant, we continue with our ANOVA
# load packages for ANOVA
library("compute.es")
library("multcomp")
library("pastecs")

# create ANOVA model
ReligAgeWed <- aov(agewed ~ relig, data = GSS, na.action=na.omit)
summary(ReligAgeWed)

# test individual pairs using a Bonferroni correction
pairwise.t.test(GSS$agewed, GSS$relig, p.adjust.method = "bonferroni")

# calculate the Omega squared effect size
# (SSm - (dfm)MSr)/(SSt + MSr)
# formula is modified version from textbook, p.455 and 
# site: http://stats.stackexchange.com/questions/2962/omega-squared-for-measure-of-effect-in-r

omega_squared <- function(anova_model)
{
	sum_stats <- summary(anova_model)[[1]]
	SSm <- sum_stats[["Sum Sq"]][1]
	SSr <- sum_stats[["Sum Sq"]][2]
	SSt <- SSm + SSr
	MSr <- sum_stats[["Mean Sq"]][2]
	DFm <- sum_stats[["Df"]][1]
	omega_sq = (SSm - DFm * MSr)/(SSt + MSr)
	print.noquote("Omega squared: ")
	return(as.numeric(omega_sq))
}

omega_squared(ReligAgeWed)
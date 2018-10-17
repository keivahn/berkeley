# import the data and set it to a data frame
WB_gdp <- read.csv("GDP_World_Bank.csv",header = TRUE)

# add a column for gdp growth between 2012 and 2011
WB_gdp$gdp_growth <- WB_gdp$gdp2012 - WB_gdp$gdp2011

# find the mean growth, while avoiding "NA" values
mean_growth <- mean(WB_gdp$gdp_growth, na.rm = TRUE)
mean_growth

# plot a histogram of the growths
library("Rcmdr")
with(WB_gdp, Hist(gdp_growth, scale="frequency", breaks=50, col="darkgray", xlab="gdp_growth", ylab="Number of countries", main="Number of Countries by GDP Growth"))

# create a high_growth column that yields true for countries with above average growth and false 
# for countries with below average growth
WB_gdp$high_growth = WB_gdp$gdp_growth > mean_growth

# find the number of countries with above and below average growths
num_true <- length(WB_gdp$high_growth[WB_gdp$high_growth==TRUE])
num_true
num_false <- length(WB_gdp$high_growth[WB_gdp$high_growth==FALSE])
num_false

# alternative method (correct method)
table(WB_gdp$high_growth)

# load TB incidence rates, source: World Bank (http://data.worldbank.org/indicator/SH.TBS.INCD)
# IMPORTANT: must delete frist two rows of World Bank data so that R can properly read in headers, 
# change header for "Country Name" to "Country" to merge with existing data frame
WB_TB <- read.csv("TB_data.csv", header = TRUE)

# create new data frame for both GDP and TB data
WB_gdp_tb = merge(WB_gdp,WB_TB, by="Country", all=TRUE)

# create new variable, TB_decline, that measures how TB incidence dropped from 2011 to 2012
WB_gdp_tb$TB_decline <- WB_gdp_tb$X2011 - WB_gdp_tb$X2012

# plot GDP growth against TB incidence, make sure you have GGPlot package installed
library(ggplot2)
graph_gdp_tb = ggplot(WB_gdp_tb, aes(TB_decline,gdp_growth)) + ggtitle("2011-2012: Change in GDP by Change in Incidence of TB")
graph_gdp_tb + geom_point()

# because we find that points are largely clustered around the zero values on the x- and y-axises, we choose to scale
# both variables by log10, recognizing that we're losing those values that are negative 
# (source: Async lecture 5.5)
graph_gdp_tb + geom_point() + scale_y_log10() + scale_x_log10() + labs(x="Decline in TB Incidence", y="GDP Growth")
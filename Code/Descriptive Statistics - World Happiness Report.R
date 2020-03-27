# Descriptive Statistics

# https://towardsdatascience.com/descriptive-statistics-in-r-8e1cad20bf3a

library(ggplot2)

# Clear all variables
rm(list = ls())

# Read Happiness Data
happ2019 = read.csv("C:/Users/afisher/Documents/R Code/Resources/Data/Happiness/2019.csv")

# First 6 observations
head(happ2019)

# Structure of dataset
str(happ2019)

# Min and Max
min(happ2019$Score)
max(happ2019$Score)

# Range function
range(happ2019$Score)
rng = range(happ2019$Score)
rng[2]

# Mean
mean(happ2019$Score)

# Important Note: if there is at least one missing value in your dataset, 
# use mean(happ2019$Score, na.rm = TRUE) to compute the mean with 
# the NA excluded. This argument can be used for most functions presented

# Median
median(happ2019$Score)

# Quantile
quantile(happ2019$Score)
quantile(happ2019$Score) #95th percentile

# Standard Deviation and Variance

sd(happ2019$Score)
var(happ2019$Score)

# Tip: to compute the standard deviation (or variance) 
# of multiple variables at the same time, use lapply() 
# with the appropriate statistics as second argument:

lapply(happ2019[,3:7], mean)

# Using Summary
summary(happ2019)

# If you need more descriptive statistics, 
# use stat.desc() from the package {pastecs}:

library(pastecs)

stat.desc(happ2019)

# Create categorical variables
happ2019$Happy = ifelse(happ2019$Score > mean(happ2019$Score), 
                        "Happy", "Not Happy")
happ2019$Rich = ifelse(happ2019$GDP.per.capita > quantile(happ2019$GDP.per.capita, 0.75),
                       "Rich Country", "Other")

# Table function
table(happ2019$Happy)
table(happ2019$Rich)

table(happ2019$Happy, happ2019$Rich)

# Instead of having the frequencies (i.e.. the number of cases) 
# you can also have the relative frequencies in each subgroup 
# by adding the table() function inside the prop.table() function:

prop.table(table(happ2019$Happy, happ2019$Rich))

#################
# Visualization #
#################

# Barplot - base R graphics
barplot(table(happ2019$Happy))
barplot(prop.table(table(happ2019$Happy)))


# ggplot - get in the habit of using
library(ggplot2)

ggplot(happ2019) +
       aes(x=Happy) +
       geom_bar()

# Histogram
hist(happ2019$Score)
ggplot(happ2019) + aes(x=Score) + geom_histogram()

# Boxplot

# A boxplot graphically represents the distribution of a 
# quantitative variable by visually displaying five common 
# location summary (minimum, median, first and third quartiles and maximum) 
# and any observation that was classified as a suspected outlier using the
# interquartile range (IQR) criterion.

boxplot(happ2019$Score)
boxplot(happ2019$Score ~ happ2019$Rich)
ggplot(happ2019) + aes(x=Rich, y=Score) + geom_boxplot()

# Scatterplot
plot(happ2019$GDP.per.capita, happ2019$Score)
ggplot(happ2019) + aes(x = GDP.per.capita, y=Score) + geom_point()

# Scatterplot by Group
happ2019$Nordic = ifelse(happ2019$Country.or.region %in% c('Denmark', 'Finland', 'Iceland', 'Norway', 'Sweden'),
                         "Nordic", "Other")

# Color Nordic
ggplot(happ2019) + 
  aes(x=GDP.per.capita, y=Score, colour=Nordic) + 
  geom_point()

# Size by Generosity
ggplot(happ2019) + 
  aes(x=GDP.per.capita, y=Score, size = Generosity) + 
  geom_point()

# Shape by Rich
ggplot(happ2019) + 
  aes(x=GDP.per.capita, y=Score, shape = Rich) + 
  geom_point()

# Facet by Rich
ggplot(happ2019) + 
  aes(x=GDP.per.capita, y=Score) + 
  geom_point() +
  facet_wrap( ~ Rich, nrow=2)

# Smooth Plots
ggplot(happ2019) +
  aes(y=Score, x=GDP.per.capita)+
  geom_smooth()

ggplot(happ2019) +
  aes(y=Score, x=GDP.per.capita, linetype = Rich)+
  geom_smooth()

ggplot(happ2019) +
  aes(y=Score, x=GDP.per.capita, color = Rich)+
  geom_smooth()

#Combining scatter and smoothing plot
# Smooth Plots
ggplot(happ2019) +
  aes(y=Score, x=GDP.per.capita)+
  geom_smooth() +
  geom_point()

ggplot(happ2019) +
  aes(y=Score, x=GDP.per.capita)+
  geom_smooth() +
  geom_point(aes(color=Rich))
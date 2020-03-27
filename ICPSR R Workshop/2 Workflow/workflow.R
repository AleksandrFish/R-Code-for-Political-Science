##--------------------------------------------------------------##
##                  Script for Lecture 2:                       ##
##                      Workflow in R                           ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                          ICPSR                               ##
##                          2018                                ##
##--------------------------------------------------------------##

# An Illustrative Data Analysis: Duncan's Occupational Prestige Regression

library("car")      # load car package (for functions and data in carData package)
head(Duncan, n=10)  # first 10 cases
brief(Duncan)       # abbreviated output
dim(Duncan)         # rows and columns
View(Duncan)        # in the RStudio data viewer
summary(Duncan)     # invoking the summary() generic function
help("Duncan")      # codebook for the data set

# Examining the Data

with(Duncan, hist(prestige))

scatterplotMatrix( ~ prestige + education + income, 
                   id=list(n=3), data=Duncan)

# Duncan's regression

(duncan.model <- lm(prestige ~ education + income, data=Duncan))

summary(duncan.model)  # more detailed report

# some regression diagnostics

    # distribution of the (studentized) residuals

densityPlot(rstudent(duncan.model))
qqPlot(duncan.model, id=list(n=3))
outlierTest(duncan.model)

    # influence diagnostics

influencePlot(duncan.model, id=list(n=3))

influenceIndexPlot(duncan.model, id=list(n=3))

avPlots(duncan.model, id=list(n=3, method="mahal"))

    # nonlinearity diagnostic

crPlots(duncan.model)

# nonconstant-spread diagnostics

spreadLevelPlot(duncan.model)
ncvTest(duncan.model)
ncvTest(duncan.model, var.formula= ~ income + education)

# refit without ministers and conductors

whichNames(c("minister", "conductor"), Duncan)
duncan.model.2 <- update(duncan.model, subset=-c(6, 16))
summary(duncan.model.2)
compareCoefs(duncan.model, duncan.model.2)

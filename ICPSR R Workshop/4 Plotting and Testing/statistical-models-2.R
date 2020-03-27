##--------------------------------------------------------------##
##                  Script for Lecture 4:                       ##
##                 Statistical Models in R                      ##
##        Part 2: Testing, Plotting, and Checking Models        ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                          ICPSR                               ##
##                          2018                                ##
##--------------------------------------------------------------##

library("car")

# Confidence intervals for coefficients

Prestige.2 <- na.omit(Prestige)

prestige.mod.1 <- lm(prestige ~ education + log2(income) + women,
                     data=Prestige.2)  
S(prestige.mod.1)
confint(prestige.mod.1)
Confint(prestige.mod.1)

mod.cowles <- glm(volunteer ~ sex + extraversion*neuroticism, 
                  family=binomial, data=Cowles)
S(mod.cowles)

# Hypothesis tests

    # Linear models

        # ANOVA tables

Prestige.2$type <- factor(Prestige.2$type, levels=c("bc", "wc", "prof"))

prestige.mod.2 <- lm(prestige ~ log2(income) + education + type, 
                     data=Prestige.2)
S(prestige.mod.2)
anova(prestige.mod.2) # sequential (Type-I) tests -- not generally useful
Anova(prestige.mod.2) # partial (Type-II) tests

prestige.mod.3 <- lm(prestige ~ (log2(income) + education)*type, data=Prestige.2)
Anova(prestige.mod.3)

        # direct computation of LR F tests for nested models

anova(prestige.mod.2, prestige.mod.3) # both sets of interactions

brief((mod.duncan.1 <- lm(prestige ~ income + education, data=Duncan)))
brief(mod.duncan.2 <- lm(prestige ~ I(income + education), data=Duncan)) # equal slopes
anova(mod.duncan.1, mod.duncan.2) # test of equal slopes

        # Wald tests of linear hypotheses

matchCoefs(prestige.mod.3, ":")
linearHypothesis(prestige.mod.3, matchCoefs(prestige.mod.3, ":")) # no interactions

linearHypothesis(mod.duncan.1, "income = education") # test of equal slopes

    # Analysis of deviance for a GLM

Anova(mod.cowles)

    # Wald tests for fixed-effect terms in a mixed-effects model

library("nlme")
Blackmore$log.exercise <- log(Blackmore$exercise + 5/60, 2)
bm.mod <- lme(log.exercise ~ I(age - 8)*group,
              random = ~ 1 | subject,
              correlation = corCAR1(form = ~ age |subject),
              data=Blackmore)
S(bm.mod)
Anova(bm.mod)

# Visualizing fitted models: Effect displays

library("effects")
plot(predictorEffects(prestige.mod.3, c("income", "education")))

plot(Effect(c("extraversion", "neuroticism"), mod.cowles))
plot(Effect(c("extraversion", "neuroticism"), mod.cowles), multiline=TRUE)
plot(Effect(c("extraversion", "neuroticism"), mod.cowles), 
     lines=list(multiline=TRUE), 
     confint=list(style="bands"))

plot(Effect(c("age", "group"), bm.mod))
plot(Effect(c("age", "group"), bm.mod, 
        transformation=list(link=function(x) log2(x + 5/60), 
                        inverse=function(x) 2^x - 5/60)), 
     type="response",
     lines=list(multiline=TRUE), 
     confint=list(style="bars"), 
     axes=list(x=list(age=list(lab="Age (years)"), rug=FALSE), 
               y=list(lab="Exercise (hours/week)")), 
     lattice=list(key.args=list(x = 0.20, y = 0.75, corner = c(0, 0), 
                   padding.text = 1.25)), 
     main="")

# Some regression (model) diagnostics

    # added-variable ("partial regression") plots

avPlots(mod.duncan.1, id=list(n=3, method="mahal"))
whichNames(c("minister", "conductor"), Duncan)
mod.duncan.3 <- update(mod.duncan.1, subset = - c(6, 16))
compareCoefs(mod.duncan.1, mod.duncan.3)

    # component + residual ("partial residual") plots

brief(prestige.mod.4 <- lm(prestige ~ income + education, 
                           data=Prestige))
crPlots(prestige.mod.4)

brief(prestige.mod.5 <- lm(prestige ~ log2(income) + education, 
  data=Prestige)) # original analysis
crPlots(prestige.mod.5)

    # adding partial residuals to effect plots

brief(prestige.mod.2)
plot(Effect(c("income", "type"), prestige.mod.2, residuals=TRUE), 
     partial.residual=list(span=1))
plot(Effect(c("education", "type"), prestige.mod.2, residuals=TRUE), 
     partial.residual=list(span=1))

brief(prestige.mod.6 <- lm(prestige ~ type*(income + education), data=Prestige.2))
Anova(prestige.mod.6)
plot(allEffects(prestige.mod.6, partial.residuals=TRUE), span=1)

    # these diagnostics also work with GLMs

plot(Effect(c("extraversion", "neuroticism"), mod.cowles,residuals=TRUE),
     partial.residuals=list(span=0.9))

brief(mod.ornstein.1 <- glm(interlocks ~ assets + nation + sector,
                      family=quasipoisson, data=Ornstein))
crPlot(mod.ornstein.1, "assets")
brief(mod.ornstein.2 <- glm(interlocks ~ log2(assets) + nation + sector,
                      family=quasipoisson, data=Ornstein))
crPlot(mod.ornstein.2, "log2(assets)")
avPlot(mod.ornstein.2, "log2(assets)", id=list(n=2, method="mahal"))

#Acceptance

library(tidyverse)
library(ggplot2)

grad = read.csv("C:/Users/afisher/Documents/Python Code/Projects/College/binary1.csv")

summary(grad)

hist(grad$gre)
hist(grad$admit)
hist(grad$gpa)
hist(grad$rank)


m1 = glm(admit ~ gre + gpa, data=grad, family=binomial)
summary(m1)

library(tibble)
library(broom)
library(margins)
library(Ecdat)

tidy(m1)
effects = margins(m1)
summary(effects)
plot(effects)

effects_graph = summary(effects)

ggplot(data = effects_graph) +
  geom_point(aes(factor, AME)) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

cplot(m1, x = "gre", se.type = "shade")
cplot(m1, x = "gpa", se.type = "shade")

library(ggeffects)

m1 = glm(admit ~ gre, data=grad, family=binomial)
plot(ggpredict(m1, terms="gre [all]"))
plot(ggpredict(m1, terms="gpa [all]")) +
  labs(
    x = "GPA", 
    y = "& Chance of Admission", 
    title = "Relationship between GPA and Admission"
  )


# https://towardsdatascience.com/visualizing-marginal-effects-using-ggeffects-in-r-4e7fb0569040
# https://strengejacke.wordpress.com/2018/07/03/marginal-effects-for-regression-models-in-r-rstats-dataviz/
# https://strengejacke.github.io/ggeffects/articles/plotcustomize.html
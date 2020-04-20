library(tidyverse)
library(ggplot2)

votes = read.csv("C:/Users/afisher/Documents/R Code/Resources/Data/2016 Election/votes.csv")
votes = votes %>%
  select(Trump, state_abbr, county_name, population_change, White, Edu_batchelors) %>%
  rename(educ_bach = Edu_batchelors) 

## Change scale
votes$Trump = votes$Trump*100
votes$White = votes$White*100
votes$Black = votes$Black*100


## Scatterplots
plot(votes$White, votes$Trump, xlab = "% White Population",
     ylab = "Trump Vote")

ggplot(votes) +
  aes(x=White, y=Trump) +
  geom_point(color = 'dodgerblue', alpha=0.3) +
  geom_smooth(method='lm', color='black')+
  theme_gray() +
  labs(x="% White Population", y = "Trump Vote")

## Linear Regression
m1 = lm(Trump ~ White, data=votes)
summary(m1)

plot(votes$White, votes$Trump, xlab = "% White Population",
     ylab = "Trump Vote")
abline(m1, col='red')

## What is the predicted Trump vote for a county thats 30% white

coef(m1)
a.hat <- coef(m1)[1] ## estimated intercept
b.hat <- coef(m1)[2] ## estimated slope

pred30 = a.hat + b.hat * 0.3
pred30

## What is the predicted Trump vote for a county thats 30% white

pred80 = a.hat + b.hat * 0.8
pred80

plot(votes$White, votes$Trump, xlab = "% White Population",
     ylab = "Trump Vote")
abline(m1, col='red')
abline(v=0.3, col='blue')


## Looking at States

penn = lm(Trump ~ White, data=votes, subset = state_abbr == 'PA')
coef(penn)
florida = lm(Trump ~ White, data=votes, subset = state_abbr == 'FL')
coef(florida)

plot(votes$White, votes$Trump, xlab = "% White Population",
     ylab = "Trump Vote")
abline(m1, col='green')
abline(penn, col='red')
abline(florida, col='blue')

library(gridExtra)
library(grid)

p1<-ggplot(votes) +
  aes(x=White, y=Trump) +
  geom_point(color = 'dodgerblue', alpha=0.3) +
  geom_smooth(method='lm', color='black')+
  theme_gray() +
  labs(x="% White Population", y = "Trump Vote")

p2<-ggplot(votes) +
  aes(x=educ_bach, y=Trump) +
  geom_point(color = 'dodgerblue', alpha=0.3) +
  geom_smooth(method='lm', color='black')+
  theme_gray() +
  labs(x="% with Bachelor degrees", y = "Trump Vote")

grid.arrange(p1, p2, ncol = 2)

## Multiple Regression in R

race.fit = lm(Trump ~ White, data=votes)
educ.fit = lm(Trump ~ educ_bach, data=votes)


mult.fit = lm(Trump ~ White + educ_bach, data=votes)
summary(mult.fit)

## Interaction
interaction.fit = lm(Trump ~ White * educ_bach, data=votes)
summary(interaction.fit)

library(stargazer)

stargazer(race.fit, educ.fit, type = 'html')

stargazer(race.fit, educ.fit, header=FALSE, align=TRUE,
          title = "Regression Results",
          covariate.labels = c("% White", "% With Bachelor"))

library(Hmisc)


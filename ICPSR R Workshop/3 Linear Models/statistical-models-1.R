##--------------------------------------------------------------##
##                  Script for Lecture 3:                       ##
##                 Statistical Models in R                      ##
##  Part 1: Linear, Generalized Linear and Mixed-Effects Models ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                          ICPSR                               ##
##                          2018                                ##
##--------------------------------------------------------------##

# Linear models in R

# multiple regression (Prestige data)

library("car")
names(Prestige) # from carData package
some(Prestige)  # 10 randomly sampling observations
brief(Prestige)
View(Prestige)
?Prestige       #help on data set

prestige.mod.1 <- lm(prestige ~ education + log2(income) + women,
    data=Prestige)  
summary(prestige.mod.1)
S(prestige.mod.1)
brief(prestige.mod.1)

# dummy regression

Prestige$type # a factor
class(Prestige$type) 
str(Prestige$type) # structure: also see RStudio Environment tab

Prestige.2 <- na.omit(Prestige) # filter out missing data
nrow(Prestige)
nrow(Prestige.2)
levels(Prestige.2$type)
Prestige.2$type <- with(Prestige.2, 
    factor(type, levels=c("bc", "wc", "prof"))) # reorder levels
Prestige.2$type

# generating contrasts from factors

getOption("contrasts")
contrasts(Prestige.2$type)
model.matrix(~ type, data=Prestige.2)

contrasts(Prestige.2$type) <- contr.treatment(levels(Prestige.2$type), base=2)
                                                # changing baseline level
contrasts(Prestige.2$type)

contrasts(Prestige.2$type) <- "contr.helmert"  # Helmert contrasts
contrasts(Prestige.2$type)

contrasts(Prestige.2$type) <- "contr.sum"  # "deviation" contrasts
contrasts(Prestige.2$type)

contrasts(Prestige.2$type) <- NULL  # back to default

Prestige.2$type.ord <- ordered(Prestige.2$type, 
    levels=c("bc", "wc", "prof")) # ordered factor
Prestige.2$type.ord
round(contrasts(Prestige.2$type.ord), 3)   # orthogonal polynomial contrasts
crossprod(contrasts(Prestige.2$type.ord))  # floating-point arithmetic 
                                           #   is not exact!
round(crossprod(contrasts(Prestige.2$type.ord)), 10)
zapsmall(crossprod(contrasts(Prestige.2$type.ord)))

prestige.mod.2 <- lm(prestige ~ log2(income) + education + type, 
    data=Prestige.2)
S(prestige.mod.2)

scatter3d(prestige ~ log2(income) + education | type, 
          parallel=TRUE, data=Prestige)

prestige.mod.3 <- update(prestige.mod.2, 
    . ~ . + log2(income):type + education:type)  # adding interactions
S(prestige.mod.3)
scatter3d(prestige ~ log2(income) + education | type, 
          parallel=FALSE, data=Prestige)

# equivalent specifications:
brief(lm(prestige ~ log2(income)*type + education*type, 
         data=Prestige.2))
brief(lm(prestige ~ (log2(income) + education)*type, 
         data=Prestige.2))

# more on lm() (time permitting)

args(lm)

brief(Davis)
?Davis
brief(lm(weight ~ repwt, data=Davis, subset=sex == "F"))  # observation selection 
                                                          #   (women only)
brief(lm(weight ~ repwt, data=Davis, subset=1:100))  # first 100 cases

brief(lm(prestige ~ income + education, data=Duncan, subset=-c(6, 16))) 
                                            # omit cases 6 & 16

brief(lm(conformity ~ partner.status*fcategory,  # specifying contrasts
    contrasts=list(partner.status=contr.sum, fcategory=contr.poly),
    data=Moore))

brief(lm(100*conformity/40 ~ partner.status*fcategory, data=Moore))
        # note computation of y

brief(lm(prestige ~ I(income + education), data=Duncan))  # "protecting" expresssion 
                                                          #   on RHS of the model


# Generalized linear models

# binary logit model

?Cowles

mod.cowles <- glm(volunteer ~ sex + extraversion*neuroticism, 
                  family=binomial, data=Cowles)
S(mod.cowles)

# Poisson and quasi-Poisson regression

some(Ornstein)
nrow(Ornstein)
?Ornstein
(tab <- xtabs(~interlocks, data=Ornstein))

x <- as.numeric(names(tab)) # the names are the distinct values of interlocks
plot(x, as.vector(tab), type="h", 
    xlab="Number of Interlocks", ylab="Frequency")
points(x, tab, pch=16)

mod.ornstein <- glm(interlocks ~ log2(assets) + nation + sector,
    family=poisson, data=Ornstein)
S(mod.ornstein)

    # quasi-Poisson model, allowing for overdispersion

mod.ornstein.q <- update(mod.ornstein, family=quasipoisson)
S(mod.ornstein.q)


# A mixed-effects models for longitudinal data

# Blackmore data

library("nlme")
library("car") # if not already loaded

brief(Blackmore, c(10, 10))  # first and last 10 obs.
dim(Blackmore)
sum(Blackmore$exercise == 0)

Blackmore$log.exercise <- log(Blackmore$exercise + 5/60, 2)
#logs, base 2 (add 5 minutes to avoid 0s)

par(mfrow=c(1, 2), mar=c(5, 4, 1, 4))
boxplot(exercise ~ group, ylab="Exercise (hours/week)", data=Blackmore)
boxplot(log.exercise ~ group, ylab=expression(log[2]*(Exercise + 5/60)),
        data=Blackmore)
axis(4, at=seq(-2, 4, by=2), labels=round(2^seq(-2, 4, by=2) - 5/60, 1))
mtext("Exercise (hours/week)", 4, line=3)

set.seed(12345) # for reproducibility

pat <- with(Blackmore,
            sample(unique(subject[group=='patient']), 20) # 20 patients
)
Pat.20 <- with(Blackmore, 
               groupedData(log.exercise ~ age | subject,
                           data=Blackmore[is.element(subject, pat),])
)

con <- with(Blackmore,
            sample(unique(subject[group=='control']), 20) # 20 controls
)
Con.20 <- with(Blackmore, 
               groupedData(log.exercise ~ age | subject,
                           data=Blackmore[is.element(subject, con),])
)

print(plot(Con.20, main='Control Subjects', cex.main=1,
           xlab='Age', ylab=expression(log[2]*Exercise),
           ylim=1.2*range(Con.20$log.exercise, Pat.20$log.exercise),
           layout=c(10, 2), aspect=1.0),
      position=c(0, 0, 1, .5), more=TRUE)
print(plot(Pat.20, main='Patients', cex.main=1,
           xlab='Age', ylab=expression(log[2]*Exercise),
           ylim=1.2*range(Con.20$log.exercise, Pat.20$log.exercise),
           layout=c(10, 2), aspect=1.0),
      position=c(0, .5, 1, 1))

pat.list <- lmList(log.exercise ~ age | subject, subset= group == "patient",
                   data=Blackmore)

con.list <- lmList(log.exercise ~ age | subject, subset= group == "control",
                   data=Blackmore)

pat.coef <- coef(pat.list)
con.coef <- coef(con.list)

par(mfrow=c(1, 2))
boxplot(pat.coef[,1], con.coef[,1], main='Intercepts', cex.main=1,
        names=c('Patients', 'Controls'))
boxplot(pat.coef[,2], con.coef[,2], main='Slopes', cex.main=1,
        names=c('Patients', 'Controls'))

bm.mod.1 <- lme(log.exercise ~ I(age - 8)*group,
                random = ~ I(age - 8) | subject,
                data=Blackmore)
S(bm.mod.1)

bm.mod.2 <- update(bm.mod.1, random = ~ 1 | subject) 
# no random slopes
anova(bm.mod.1, bm.mod.2)

bm.mod.3 <- update(bm.mod.1, random = ~ I(age - 8) - 1 | subject)
# no random intercepts
anova(bm.mod.1, bm.mod.3)

bm.mod.4 <- update(bm.mod.1, correlation = corCAR1(form = ~ age |subject))
# CAR1 errors; this model doesn't converge

bm.mod.5 <- update(bm.mod.1, random = ~ 1 | subject,
                   correlation = corCAR1(form = ~ age |subject))
# CAR1 errors, no random slopes

bm.mod.6 <- update(bm.mod.1, random = ~ I(age - 8) - 1 | subject,
                   correlation = corCAR1(form = ~ age |subject))
# CAR1 errors, no random intercepts

logLik(bm.mod.1)
logLik(bm.mod.5)
logLik(bm.mod.6)

BIC(bm.mod.1)
BIC(bm.mod.5)
BIC(bm.mod.6)

AIC(bm.mod.1)
AIC(bm.mod.5)
AIC(bm.mod.6)

compareCoefs(bm.mod.1, bm.mod.5, bm.mod.6)

S(bm.mod.5)

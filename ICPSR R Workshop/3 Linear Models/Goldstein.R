# Goldstein's math-achievement data

# preparing the data

Goldstein <- read.table(file.choose(), header=TRUE)
attach(Goldstein)

table(school) # counts in each school

meanmath.8 <- tapply(math.8, school, mean) # mean math-1 score by school

Goldstein$meanmath.8 <- meanmath.8[as.character(school)] # add mean math-1 score to level-1 data
Goldstein$math.8.center <- math.8 - Goldstein$meanmath.8 # school-centered math-1 scores

# examining the data

library(lattice)

trellis.device(theme=col.whitebg())

xyplot(math.11 ~ math.8.center | as.character(school), data=Goldstein, 
    panel=function(x, y){
        panel.xyplot(x, y)
        panel.lmline(x, y, lty=2)
        }
    )

dotplot(math.11 ~ factor(female, labels=c("male", "female")) 
    | as.character(school), data=Goldstein)

dotplot(math.11 ~ factor(manual, labels=c("non.man", "manual")) 
    | as.character(school), data=Goldstein)

# plot within-school regression coefficients vs. school mean math-1 scores
# identifying interesting schools with the mouse

library(nlme)

mod.list <- lmList(math.11 ~ math.8.center + manual + female | school, data=Goldstein)
mod.coef <- coef(mod.list)
mod.coef # note some missing

windows()
mod.coef$mean.math.8 <- meanmath.8
mod.coef <- na.omit(mod.coef)
par(mfrow=c(2,2))
for (i in 1:4){
    plot(mod.coef[,5], mod.coef[,i], ylab=colnames(mod.coef)[i], xlab="mean gr. 8 math")
    abline(lm(mod.coef[,i] ~ mod.coef[,5]))
    lines(lowess(mod.coef[,5], mod.coef[,i]), lty=2)
    identify(mod.coef[,5], mod.coef[,i], rownames(mod.coef))
    }

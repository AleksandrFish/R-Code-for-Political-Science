##--------------------------------------------------------------##
##                  Script for Lecture 7:                       ##
##           R Programming 2: Beyond the Basics                 ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                          ICPSR                               ##
##                          2018                                ##
##--------------------------------------------------------------##

# Non-trivial programming example
# estimation of the ZIP (zero inflated Poisson) regression model by optimization

zipmod <- function(X, y, Z=X, intercept.X=TRUE, 
                   intercept.Z=TRUE, ...) {
    # ZIP model
    # X: model matrix for Poisson model
    # y: response vector
    # Z: model model for logit model
    # intercept.X: add column of 1s for intercept to X
    # intercept.Z: add column of 1s for intercept to Z
    # ...: arguments to be passed to optim()
    if (!is.matrix(X) || !is.numeric(X)) 
        stop("X must be a numeric matrix")
    if (!is.matrix(Z) || !is.numeric(Z)) 
        stop("Z must be a numeric matrix")
    if (!is.vector(y) || !is.numeric(y) || !all(y >= 0) 
        || !all(y == round(y)))
        stop("y must be a vector of counts")
    if (nrow(X) != length(y)) 
        stop("number of rows in X must be the same as length of y")
    if (nrow(Z) != length(y)) 
        stop("number of rows in Z must be the same as length of y")
    if (intercept.X) {
        X <- cbind(1, X)
        colnames(X)[1] <- "intercept"
    }
    if (intercept.Z) {
        Z <- cbind(1, Z)
        colnames(Z)[1] <- "intercept"
    }
    n.x <- ncol(X)
    negLogL <- function(beta.gamma) {
        beta <- beta.gamma[1:n.x]
        gamma <- beta.gamma[-(1:n.x)]
 #       pi <- 1/(1 + exp(- Z %*% gamma))
        pi <- plogis(Z %*% gamma)
        mu <- exp(X %*% beta)
        L1 <- sum(log(pi + (1 - pi)*dpois(y, mu))[y == 0])
        L2 <- sum((log((1 - pi)*dpois(y, mu)))[y > 0])
        -(L1 + L2)
    }
    initial.beta <- coef(glm(y ~ X - 1, family=poisson))
    initial.gamma <- coef(glm(y == 0 ~ Z - 1, family=binomial))
    result <- optim(c(initial.beta, initial.gamma), negLogL,
                    hessian=TRUE, method="BFGS", ...)
    beta.gamma <- result$par
    vcov <- solve(result$hessian)
    par.names <- c(paste0("beta.", colnames(X)), paste0("gamma.", colnames(Z)))
    names(beta.gamma) <- par.names
    rownames(vcov) <- colnames(vcov) <- par.names
    list(coefficients=beta.gamma,  vcov=vcov,
         deviance=2*result$value, converged=result$convergence == 0)
}

    # example (Ornstein's interlocking-directorate data)

library("carData") # for data

X <- model.matrix(~ log2(assets) + nation + sector, 
                  data=Ornstein)[, -1]  # removing the constant column 1
Z <- model.matrix(~ log2(assets) + nation, data=Ornstein)[, -1]
head(X)
head(Z) 

ornstein.zip <- zipmod(X, Ornstein$interlocks, Z)
coefs <- cbind(ornstein.zip$coefficients,
    sqrt(diag(ornstein.zip$vcov))) # standard errors
colnames(coefs) <- c("estimate", "std. error")
round(coefs, 3)

ornstein.zip$converged

        # an effect plot for assets

beta <- ornstein.zip$coefficients[1:14]
gamma <- ornstein.zip$coefficients[-(1:14)]
x.beta.fixed <- sum(beta[c(1, 3:14)]*c(1, colMeans(X[, 2:13])))
x.gamma.fixed <- sum(gamma[c(1, 3:5)]*c(1, colMeans(Z[, 2:4])))
assets <- with(Ornstein, seq(min(assets), max(assets), length=100))
pi <- 1/(1 + exp(-(log2(assets)*gamma[2] + x.gamma.fixed)))
e.interlocks <- (1 - pi)*exp(log2(assets)*beta[2] + x.beta.fixed)
plot(assets, e.interlocks, xlab="assets ($millions)",
     ylab="estimated expected interlocks", type="l", lwd=2)

    # the example fails due to ill-conditioning for Z=X 

ornstein.zip.2 <- zipmod(X, Ornstein$interlocks) # error!

    # trying again using an analytic gradient (time permitting)

zipmodg <- function(X, y, Z=X, intercept.X=TRUE, 
                   intercept.Z=TRUE, ...) {
    # ZIP model
    # X: model matrix for Poisson model
    # y: response vector
    # Z: model model for logit model
    # intercept.X: add column of 1s for intercept to X
    # intercept.Z: add column of 1s for intercept to Z
    # ...: arguments to be passed to optim()
    if (!is.matrix(X) || !is.numeric(X)) 
        stop("X must be a numeric matrix")
    if (!is.matrix(Z) || !is.numeric(Z)) 
        stop("Z must be a numeric matrix")
    if (!is.vector(y) || !is.numeric(y) || !all(y >= 0) 
        || !all(y == round(y)))
        stop("y must be a vector of counts")
    if (nrow(X) != length(y)) 
        stop("number of rows in X must be the same as length of y")
    if (nrow(Z) != length(y)) 
        stop("number of rows in Z must be the same as length of y")
    if (intercept.X) {
        X <- cbind(1, X)
        colnames(X)[1] <- "intercept"
    }
    if (intercept.Z) {
        Z <- cbind(1, Z)
        colnames(Z)[1] <- "intercept"
    }
    n.x <- ncol(X)
    negLogL <- function(beta.gamma) {
        beta <- beta.gamma[1:n.x]
        gamma <- beta.gamma[-(1:n.x)]
        pi <- 1/(1 + exp(- Z %*% gamma))
        mu <- exp(X %*% beta)
        L1 <- sum(log(pi + (1 - pi)*dpois(y, mu))[y == 0])
        L2 <- sum((log((1 - pi)*dpois(y, mu)))[y > 0])
        -(L1 + L2)
    }
    grad <- function(beta.gamma){
        beta <- beta.gamma[1:n.x]
        gamma <- beta.gamma[-(1:n.x)]
        exp.xbeta <- as.vector(exp(X %*% beta))
        exp.zgamma <- as.vector(exp(Z %*% gamma))
        denom <- exp.zgamma + exp(-exp.xbeta)
        b1 <- (-(exp(-exp.xbeta)*exp.xbeta*X)/denom)[y == 0, ]
        b2 <- ((y - exp.xbeta)*X)[y > 0, ]
        b <- colSums(rbind(b1, b2))
        g1 <- (exp.zgamma*Z/denom)[y == 0, ]
        g2 <- -exp.zgamma*Z/(1 + exp.zgamma)
        g <- colSums(rbind(g1, g2))
        -c(b, g)
    }
    initial.beta <- coef(glm(y ~ X - 1, family=poisson))
    initial.gamma <- coef(glm(y == 0 ~ Z - 1, family=binomial))
    result <- optim(c(initial.beta, initial.gamma), negLogL, gr=grad,
                    hessian=TRUE, method="BFGS", ...)
    beta.gamma <- result$par
    vcov <- solve(result$hessian)
    par.names <- c(paste0("beta.", colnames(X)), paste0("gamma.", colnames(Z)))
    names(beta.gamma) <- par.names
    rownames(vcov) <- colnames(vcov) <- par.names
    list(coefficients=beta.gamma,  vcov=vcov,
         deviance=2*result$value, converged=result$convergence == 0)
}

ornstein.zip.g <- zipmodg(X, Ornstein$interlocks, Z)
ornstein.zip$coefficients/ornstein.zip.g$coefficients  # should be (close to 1) 1
sqrt(diag(ornstein.zip$vcov))/sqrt(diag(ornstein.zip.g$vcov))

        # try Z=X again

ornstein.zip.2g <- zipmodg(X, Ornstein$interlocks) # works!
(lambda <- eigen(ornstein.zip.2g$vcov)$values)  
lambda[1]/lambda[length(lambda)]  # ill-conditioned!

# Simulation example: (time permitting)
# efficiency of LS and robust regression when errors are normal, non-normal
#    and contaminated by outliers

library("MASS") # for rlm()

mod.prestige <- lm(prestige ~ education + log2(income), data=Prestige)
beta <- coef(mod.prestige)  # taken as "true" parameters
sigma <- summary(mod.prestige)$sigma  # taken as true scale of errors
Ey <- fitted(mod.prestige)  # taken as expectation of response

    # scale factors for t[2] and Cauchy (t[1]) errors
(scale.factor.t <- 
        diff(qt(c(0.25, 0.75), df=2))/diff(qnorm(c(0.25, 0.75))))
(scale.factor.c <- 
        diff(qt(c(0.25, 0.75), df=1))/diff(qnorm(c(0.25, 0.75))))
(sigma.t <- sigma/scale.factor.t)
(sigma.c <- sigma/scale.factor.c)
x <- seq(-5, 5, length=500)
dc <- dcauchy(x)
dt <- dt(x, df=2)
dn <- dnorm(x)
plot(sigma.c*x, dc, ylim=c(0, max(dn)), type="l", 
     ylab=expression("Density"~~p(epsilon)), 
     xlab=expression("Error"~~epsilon), lwd=2,
     main="Error Distributions\nwith IQRs equated")
lines(sigma.t*x, dt, lty=2, lwd=2)
lines(sigma*x, dn, lty=3, lwd=2)
legend("topright", lty=1:3, lwd=2, 
       legend=c("Cauchy", "t(2)", "Normal"), 
       bty="n", inset=0.02)

B <- 1000  # number of simulated samples

    # initialize:
robust.c <- robust.t <- robust.normal <- 
    LS.c  <- LS.t <- LS.normal <- matrix(0, B, 3)
colnames(robust.c) <- colnames(robust.t) <-  
    colnames(robust.normal) <- colnames(LS.c) <- 
    colnames(LS.t) <- colnames(LS.normal) <- 
    names(coef(mod.prestige))
X <- with(Prestige, cbind(education, log2(income)))
colnames(X)[2] <- "log2(income)"
head(X)

(seed <- sample(1e6, 1))  # randomly generate a seed
set.seed(seed) # for reproducibility

system.time(
    for (i in 1:B){
        # sample errors:
        normal.errors <- sigma*rnorm(102) 
        t.errors <- sigma.t*rt(102, 2)
        contam.errors <- normal.errors
        contam.errors[2] <- rcauchy(1, scale=sigma.c)  # case 2
        # construct observed responses:
        y.normal <- Ey + normal.errors 
        y.t <- Ey + t.errors
        y.c <- Ey + contam.errors
        # LS estimation errors:
        LS.normal[i, ] <- coef(lm(y.normal ~ X)) - beta 
        LS.t[i, ] <- coef(lm(y.t ~ X)) - beta
        LS.c[i, ] <- coef(lm(y.c ~ X)) - beta
        # robust estimation errors:
        robust.normal[i, ] <- 
            coef(rlm(y.normal ~ X, method="MM", maxit=100)) - beta
        robust.t[i, ] <- 
            coef(rlm(y.t ~ X, method="MM", maxit=100)) - beta
        robust.c[i, ] <- 
            coef(rlm(y.c ~ X, method="MM", maxit=100)) - beta
    }
)
head(LS.normal)  # e.g., LS errors in estimation from first 6 samples

    # estimated relative bias (should be close to 0)

colMeans(LS.normal)/abs(beta)
colMeans(LS.t)/abs(beta)
colMeans(LS.c)/abs(beta)
colMeans(robust.normal)/abs(beta)
colMeans(robust.t)/abs(beta)
colMeans(robust.c)/abs(beta)

    # estimated relative root-mean-square error

RMSE <- c(
    sqrt(colMeans(LS.normal^2))/abs(beta),
    sqrt(colMeans(robust.normal^2))/abs(beta),
    sqrt(colMeans(LS.t^2))/abs(beta),
    sqrt(colMeans(robust.t^2))/abs(beta),
    sqrt(colMeans(LS.c^2))/abs(beta),
    sqrt(colMeans(robust.c^2))/abs(beta)
)
Coefficient <- factor(
    rep(c("Intercept", "Education", "log Income"), 6),
    levels=c("Intercept", "Education", "log Income")
)
Estimator <- rep(rep(c("LS", "MM"), each=3), 3)
Error <- factor(rep(c("normal", "t(2)", "contaminated"), each=6), 
                levels=c("normal", "t(2)", "contaminated"))
Results <- data.frame(RMSE, Coefficient, Estimator, Error)
head(Results)

library("lattice")

xyplot(RMSE ~ Coefficient | Error, group=Estimator, data=Results,
       type="b", auto.key=list(lines=TRUE, points=TRUE, 
                               title="Estimator"),
       ylab="Relative RMSE", 
       scale=list(y=list(log=2, equispaced.log=FALSE)), 
       layout=c(3, 1))

# The S3 object system 

    # S3 classes

mod.prestige <- lm(prestige ~ income + education + women,
    data=Prestige)
attributes(mod.prestige)
class(mod.prestige)


v <- 1:10
v
attributes(v)
class(v)

class(v) <- "character"
attributes(v)
v
class(v)

    # S3 generic functions and methods

print # the print generic

stats:::print.lm # print method for "lm" objects (from the stats package namespace)

mod.prestige
print(mod.prestige) # equivalent
stats:::print.lm(mod.prestige)  # equivalent, but bad form
print.lm(mod.prestige)  # doesn't work, because method not "exported"

methods("print") # print methods
methods(class="lm") # methods for objects of class "lm"

    # S3 "inheritance"

mod.mroz <- glm(lfp ~ ., family=binomial, data=Mroz)
class(mod.mroz)

    # Example: ZIP model

        # generic function

zipmod <- function(X, ...){
    UseMethod("zipmod")
}

        # default method (uses version with gradient)

zipmod.default <- function(X, y, Z=X, intercept.X=TRUE,  intercept.Z=TRUE, ...) {
    if (!is.matrix(X) || !is.numeric(X)) 
        stop("X must be a numeric matrix")
    if (!is.matrix(Z) || !is.numeric(Z)) 
        stop("Z must be a numeric matrix")
    if (!is.vector(y) || !is.numeric(y) || !all(y >= 0) 
        || !all(y == round(y)))
        stop("y must be a vector of counts")
    if (nrow(X) != length(y)) 
        stop("number of rows in X must be the same as length of y")
    if (nrow(Z) != length(y)) 
        stop("number of rows in Z must be the same as length of y")
    if (intercept.X) {
        X <- cbind(1, X)
        colnames(X)[1] <- "intercept"
    }
    if (intercept.Z) {
        Z <- cbind(1, Z)
        colnames(Z)[1] <- "intercept"
    }
    n.x <- ncol(X)
    negLogL <- function(beta.gamma) {
        beta <- beta.gamma[1:n.x]
        gamma <- beta.gamma[-(1:n.x)]
        pi <- 1/(1 + exp(- Z %*% gamma))
        mu <- exp(X %*% beta)
        L1 <- sum(log(pi + (1 - pi)*dpois(y, mu))[y == 0])
        L2 <- sum((log((1 - pi)*dpois(y, mu)))[y > 0])
        -(L1 + L2)
    }
    grad <- function(beta.gamma){
        beta <- beta.gamma[1:n.x]
        gamma <- beta.gamma[-(1:n.x)]
        exp.xbeta <- as.vector(exp(X %*% beta))
        exp.zgamma <- as.vector(exp(Z %*% gamma))
        denom <- exp.zgamma + exp(-exp.xbeta)
        b1 <- (-(exp(-exp.xbeta)*exp.xbeta*X)/denom)[y == 0, ]
        b2 <- ((y - exp.xbeta)*X)[y > 0, ]
        b <- colSums(rbind(b1, b2))
        g1 <- (exp.zgamma*Z/denom)[y == 0, ]
        g2 <- -exp.zgamma*Z/(1 + exp.zgamma)
        g <- colSums(rbind(g1, g2))
        -c(b, g)
    }
    initial.beta <- coef(glm(y ~ X - 1, family=poisson))
    initial.gamma <- coef(glm(y == 0 ~ Z - 1, family=binomial))
    result <- optim(c(initial.beta, initial.gamma), negLogL, gr=grad,
                    hessian=TRUE, method="BFGS", ...)
    beta.gamma <- result$par
    vcov <- solve(result$hessian)
    par.names <- c(paste0("beta.", colnames(X)), 
                   paste0("gamma.", colnames(Z))) 
    names(beta.gamma) <- par.names
    rownames(vcov) <- colnames(vcov) <- par.names
    result <- list(coefficients=beta.gamma,  vcov=vcov, 
       npar=c(beta=ncol(X), gamma=ncol(Z)),
       deviance=2*result$value, converged=result$convergence == 0)
    class(result) <- "zipmod"  # create new class
    result
}

X <- model.matrix(~ log2(assets) + nation + sector, 
                  data=Ornstein)[, -1]  # removing the constant column 1
Z <- model.matrix(~ log2(assets) + nation, data=Ornstein)[, -1]
head(X)
head(Z) 

ornstein.zip.3 <- zipmod(X, Ornstein$interlocks, Z)
ornstein.zip.3  # whoops, dumps whole object to console!

        # print and summary methods for class "zipmod"

print.zipmod <- function(x, ...) {
    coef <- coef(x)
    npar <- x$npar
    beta <- coef[1:npar["beta"]]
    gamma <- coef[-(1:npar["beta"])]
    names.beta <- names(beta)
    names.gamma <- names(gamma)
    names(beta) <- sub("^beta\\.", "", names.beta)
    names(gamma) <- sub("^gamma\\.", "", names.gamma)
    cat("beta coeffients:\n")
    print(beta)
    cat("\ngamma coeffients:\n")
    print(gamma)
    if (!x$converged) warning("estimates did not converge")
    invisible(x)
}

vcov.zipmod <- function(object, separate=FALSE, ...){
    if (!separate) return(object$vcov)
    else{
        vcov <- object$vcov
        npar <- object$npar
        index <- 1:npar["beta"]
        vcov.beta <- vcov[index, index]
        vcov.gamma <- vcov[-index, -index]
        names.beta <- rownames(vcov.beta)
        names.gamma <- rownames(vcov.gamma)
        rownames(vcov.beta) <- colnames(vcov.beta) <-
            sub("^beta\\.", "", names.beta)
        rownames(vcov.gamma) <- colnames(vcov.gamma) <-
            sub("^gamma\\.", "", names.gamma)
        return(list(beta=vcov.beta, gamma=vcov.gamma))
    }
}

summary.zipmod <- function(object, ...) {
    coef <- coef(object)
    npar <- object$npar
    beta <- coef[1:npar["beta"]]
    gamma <- coef[-(1:npar["beta"])]
    names.beta <- names(beta)
    names.gamma <- names(gamma)
    names(beta) <- sub("^beta\\.", "", names.beta)
    names(gamma) <- sub("^gamma\\.", "", names.gamma)
    vcov <- vcov(object, separate=TRUE)
    se.beta <- sqrt(diag(vcov[["beta"]]))
    z.beta <- beta/se.beta
    table.beta <- cbind(beta, se.beta, z.beta, 
                        2*(pnorm(abs(z.beta), lower.tail=FALSE)))
    colnames(table.beta) <- c("Estimate", "Std.Err", 
                              "z value", "Pr(>|z|)")
    rownames(table.beta) <- names(beta)
    se.gamma <- sqrt(diag(vcov[["gamma"]]))
    z.gamma <- gamma/se.gamma
    table.gamma <- cbind(gamma, se.gamma, z.gamma, 
                         2*(pnorm(abs(z.gamma), lower.tail=FALSE)))
    colnames(table.gamma) <- c("Estimate", "Std.Err", 
                               "z value", "Pr(>|z|)")
    rownames(table.gamma) <- names(gamma)
    result <- list(coef.beta=table.beta, 
                   coef.gamma=table.gamma, deviance=object$deviance, 
                   converged=object$converged)
    class(result) <- "summary.zipmod"
    result
}

print.summary.zipmod <- function(x, ...) {
    cat("beta coefficients:\n")
    printCoefmat(x$coef.beta, signif.legend=FALSE, ...)
    cat("\ngamma coefficients:\n")
    printCoefmat(x$coef.gamma, ...)
    cat("\nDeviance =", x$deviance,"\n")
    if (!x$converged) warning("estimates did not converge")
    invisible(x)
}

ornstein.zip.3

lapply(vcov(ornstein.zip.3, separate=TRUE), round, digits=4)

summary(ornstein.zip.3)

    # should also write methods for other standard generics:
    #       residuals(), fitted(), etc.

coef(ornstein.zip.3)  # default coef() method already works!



# Writing a statistical modeling function (time permitting)

    # a formula method for zipmod() (using the Formula package for 2-part formulas)

zipmod.formula <- function(formula, data, subset, 
                           na.action, model = TRUE, contrasts = NULL, ...) {
    if (!require("Formula")) stop("Formula package missing")
    formula <- Formula(formula)
    call <- match.call()  # returns the function call
    mf <- match.call(expand.dots = FALSE)  # the function call w/o ...
    args <- match(c("formula", "data", "subset", "na.action"),
                  names(mf), 0)  # which arguments are present?
    mf <- mf[c(1, args)]
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf$formula <- formula
    mf <- eval.parent(mf)  # create a model frame
    terms <- attr(mf, "terms")  # terms object for the model
    y <- model.part(formula, mf, lhs=1)[, 1]  # response variable
    X <- model.matrix(formula, mf, rhs=1, contrasts=contrasts)
    Z <- if (length(formula)[2] == 1) X
    else model.matrix(formula, mf, rhs=2, contrasts=contrasts)
    mod <- zipmod(X, y, Z, intercept.X=FALSE, intercept.Z=FALSE, ...)
    mod$na.action <- attr(mf, "na.action")
    mod$contrasts <- attr(X, "contrasts")
    mod$formula <- formula
    if (model)  {
        mod$model <- mf
        mod$X <- X
        mod$Z <- Z
        mod$y <- y
    }
    mod
}

zipmod(interlocks ~ log(assets) + nation + sector | log(assets) + nation,
    data=Ornstein)  # response ~ Poisson formula | logit formula


zipmod(interlocks ~ log(assets) + nation + sector | 
           log(assets) + nation + sector,
       data=Ornstein)

zipmod(interlocks ~ log(assets) + nation + sector, 
       data=Ornstein) # equivalent

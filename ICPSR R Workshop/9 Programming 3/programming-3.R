##--------------------------------------------------------------##
##                  Script for Lecture 9:                       ##
##        R Programming 3: Debugging and Improving R Code       ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                          ICPSR                               ##
##                          2018                                ##
##--------------------------------------------------------------##

# Debugging

    # covert factor in Mroz data to dummy variables

library("carData") # for data
head(Mroz, 10)
Mroz$lfp <- with(Mroz, ifelse(lfp == "yes", 1, 0))
Mroz$wc <- with(Mroz, ifelse(wc == "yes", 1, 0))
Mroz$hc <- with(Mroz, ifelse(hc == "yes", 1, 0))
head(Mroz, 10)

    # Implementation of Newton-Rapson algorithm for logistic regression
    #  (answer to R Programming 2 Exercise 3)

    # bugged!

lreg <- function(X, y, max.iter=10, tol=1E-6, verbose=FALSE) {
    X <- cbind(1, X)
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        if (verbose) cat("\niteration = ", it, ": ", b)
        p <- 1/(1 + exp(-X %*% b))
        V <- diag(p * (1 - p))
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
    }
    if (it > max.iter) warning("maximum iterations exceeded")
    if (verbose) cat("\n")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

mod.mroz <- with(Mroz,
    lreg(cbind(k5, k618, age, wc, hc, lwg, inc), lfp))

    # call stack to error: traceback

traceback()

    # interrupt execution, examine environment of function: browser

?browser

        # still bugged!
lreg <- function(X, y, max.iter=10, tol=1E-6, verbose=FALSE) {
    X <- cbind(1, X)
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        if (verbose) cat("\niteration = ", it, ": ", b)
        p <- 1/(1 + exp(-X %*% b))
        V <- diag(p * (1 - p))
      browser()  # pause here
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
    }
    if (it > max.iter) warning("maximum iterations exceeded")
    if (verbose) cat("\n")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

mod.mroz <- with(Mroz,
    lreg(cbind(k5, k618, age, wc, hc, lwg, inc), lfp))

        # imagined sequence of commands in browser mode: (Note RStudio debugging features!)   
t(X) %*% V %*% X
str(X)
head(X)
str(V)
V
str(p)
head(p)
p <- as.vector(p)
str(diag(p * (1 - p)))
V <- diag(p * (1 - p))
V[1:10, 1:10]

Q

    # step-through debugger: debug

?debug

        # bugged! (original bugged function)
lreg <- function(X, y, max.iter=10, tol=1E-6, verbose=FALSE) {
    X <- cbind(1, X)
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        if (verbose) cat("\niteration = ", it, ": ", b)
        p <- 1/(1 + exp(-X %*% b))
        V <- diag(p * (1 - p))
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
    }
    if (it > max.iter) warning("maximum iterations exceeded")
    if (verbose) cat("\n")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

debug(lreg) # set debug flag; use undebug(lreg4) to unset flag
    # also see ?debugonce

mod.mroz <- with(Mroz,  # again note debugging support in RStudio
    lreg(cbind(k5, k618, age, wc, hc, lwg, inc), lfp))


# Measuring time and memory allocation

    # logistic regression by Newton-Raphson (corrected):

lreg <- function(X, y, max.iter=10, tol=1E-6, verbose=FALSE){
    # X is the model matrix
    # y is the response vector of 0s and 1s
    # max.iter is the maximum number of iterations
    # tol is a convergence criterion
    # verbose: show iteration history?
    X <- cbind(1, X)  # add constant
    b <- b.last <- rep(0, ncol(X))  # initialize coefficients
    it <- 1  # initialize iteration counter
    while (it <= max.iter){
        if (verbose) cat("\niteration = ", it, ": ", b)
        p <- as.vector(1/(1 + exp(-X %*% b)))
        V <- diag(p * (1 - p))
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p)  # update coefficients
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b  # update previous coefficients
        it <- it + 1  # increment counter
    }
    if (verbose) cat("\n")  # newline
    if (it > max.iter) warning("maximum iterations exceeded")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

    # by optimization (answer to R Programming 2 Exercise 2):

lreg2 <- function(X, y, method="BFGS"){
    X <- cbind(1, X)
    negLogL <- function(b, X, y){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        - sum(y*log(p) + (1 - y)*log(1 - p))
    }
    grad <- function(b, X, y){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        - colSums((y - p)*X)
    }
    result <- optim(rep(0, ncol(X)), negLogL, gr=grad,
        hessian=TRUE, method=method, X=X, y=y)
    list(coefficients=result$par, var=solve(result$hessian),
        deviance=2*result$value, converged=result$convergence == 0)
}

    # a larger (but not large!) problem

set.seed(12345)  # for reproducibility
X <- matrix(rnorm(5000*10), 5000, 10)
y <- rbinom(5000, 1, prob=1/(1 + exp(-cbind(1, X) %*% rep(1, 11))))

    # measuring time: system.time (also see microbenchmark package)

system.time(mod.1 <- lreg(X, y))
coef(mod.1)

system.time(mod.2 <- lreg2(X, y))
coef(mod.2)

system.time(mod.glm <- glm(y ~ X, family=binomial))
coef(mod.glm)

    # profiling time and memory use: Rprof

(tmp <- tempfile()) # create temporary file
Rprof(tmp, memory.profiling=TRUE) # turn on profiling
mod.1 <- lreg(X, y)
Rprof() # turn off profiling
summaryRprof(tmp, memory="both") # summarize results
unlink(tmp) # delete temporary file

tmp <- tempfile() # create temporary file
Rprof(tmp, memory.profiling=TRUE) # turn on profiling
mod.2 <- lreg2(X, y)
Rprof() # turn off profiling
summaryRprof(tmp, memory="both") # summarize results
unlink(tmp) # delete temporary file

        # new version of Newton-Raphson function

diagprod <- function(d, X){
    if (!is.matrix(X) || !nrow(X) == length(d)) stop("X and d not conformable")
    d*X
}

        # explanation:

XX <- matrix(1, 5, 3) # don't want to overwrite X
XX
V <- diag(1:5)
V %*% XX
1:5 * XX # same thing!

diagprod(1:5, XX) # same thing


lreg3 <- function(X, y, max.iter=10, tol=1E-6) {
    X <- cbind(1, X)
    b <- b.last <- rep(0, ncol(X))
    it <- 1
    while (it <= max.iter){
        p <- as.vector(1/(1 + exp(-X %*% b)))
        var.b <- solve(crossprod(X, diagprod(p*(1 - p), X)))
        b <- b + var.b %*% crossprod(X, y - p)
        if (max(abs(b - b.last)/(abs(b.last) + 0.01*tol)) < tol) break
        b.last <- b
        it <- it + 1
    }
    if (it > max.iter) warning("maximum iterations exceeded")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

system.time(mod.3 <- lreg3(X, y))
coef(mod.3)/coef(mod.1) # check

tmp <- tempfile() # create temporary file
Rprof(tmp, memory.profiling=TRUE, interval=0.002) # profiling with smaller sampling interval
mod.3 <- lreg3(X, y)
Rprof() # turn off profiling
summaryRprof(tmp, memory="both") # summarize results
unlink(tmp) # delete temporary file

    # a much larger problem

set.seed(12345)  # for reproducibility
X <- matrix(rnorm(100000*10), 100000, 10)
y <- rbinom(100000, 1, prob=1/(1 + exp(-cbind(1, X) %*% rep(1, 11))))

system.time(mod.1 <- lreg(X, y))  # original Newton-Raphson, fails!

system.time(mod.3 <- lreg3(X, y))  # improved Newton-Raphson

system.time(mod.2 <- lreg2(X, y))  # by optimization

system.time(mod.glm <- glm(y ~ X, family=binomial))

max(abs(coef(mod.3)/coef(mod.glm) - 1))

# "debugging" other functions to examine how they work

debugonce(lm)
mod <- lm(prestige ~ income + education + type, data=Duncan)

debugonce(print.lm)  # fails
debugonce(stats:::print.lm)  # from namespace of stats package
mod

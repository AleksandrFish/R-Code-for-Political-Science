##--------------------------------------------------------------##
##                  Script for Lecture 6:                       ##
##                 R Programming 1: Basics                      ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                          ICPSR                               ##
##                          2018                                ##
##--------------------------------------------------------------##

# Preliminary programming examples

    # Example: creating lagged variables in a regression

(x <- 1:10)
c(NA, x)
c(NA, x)[-11]

        # simple version:

Lag <- function(x, lag=1){ 
    length.x <- length(x)
    c(rep(NA, lag), x[-((length.x - lag + 1):length.x)])
}

Lag(1:10)
Lag(1:10, 2)
Lag(letters)

set.seed(123) # for reproducibility
(fac <- factor(sample(c("a", "b", "c"), 20, replace=TRUE)))
Lag(fac, 2) # incorrect!
Lag(1:10, -1) # fails!

        # enhanced version (handles factors, "leads", does error checking):

Lag <- function(x, lag=1){
    # lag a variable, padding with NAs
    # x: variable to lag
    # lag: number of observations to lag (negative = lead)
    if (!(is.vector(x) || is.factor(x))) 
        stop ("x must be a vector or a factor")
    if (!(length(lag) == 1 && floor(lag) == lag)) 
        stop ("lag must be an integer")
    length.x <- length(x)
    if (abs(lag) > length.x) stop("|lag| > length of x")
    result <- if (lag == 0) x
        else if (lag > 0) c(rep(NA, lag), x[1:(length.x - lag)])
        else {
            lag <- abs(lag)
            c(x[(lag + 1):length.x], rep(NA, lag))
        }
    if (is.factor(x)) result <- factor(levels(x)[result])
    result
}

Lag(1:10)
Lag(1:10, 2)
Lag(letters)
Lag(fac, 2) # works
Lag(1:10, -1) # "lead"
Lag(1:10, 1.5) # informative error

        # Example

library("carData") # for data

?Bfox

Bfox$debt.1 <- Lag(Bfox$debt)
Bfox$debt.2 <- Lag(Bfox$debt, 2)
Bfox$debt.3 <- Lag(Bfox$debt, 3)
Bfox

summary(lm(partic ~ debt, data=Bfox))
summary(lm(partic ~ Lag(debt), data=Bfox))
summary(lm(partic ~ Lag(debt, 2), data=Bfox))

    # Example (time permitting): "influence plot"

mod.duncan <- lm(prestige ~ income + education, data=Duncan)
summary(mod.duncan)

cook <- sqrt(cooks.distance(mod.duncan))  # Cook's distances 
                                          # (influence measure)
plot(hatvalues(mod.duncan), rstudent(mod.duncan), cex=(10/max(cook))*cook)  
    # hatvalues (leverage) and studentized residuals (outlyingness)
abline(h=c(-2, 0, 2), lty=2)  # lines on rstudent scale
abline(v=c(2, 3)*length(coef(mod.duncan))/length(rstudent(mod.duncan)), lty=1)
    # 2 and 3 times average hatvalue
identify(hatvalues(mod.duncan), rstudent(mod.duncan), rownames(Duncan))  
            # remember to exit from identify!

        # encapsulate in a function:

            # simple version

inflPlot0 <- function (model, case.names) {
    cook <- sqrt(cooks.distance(model))
    plot(hatvalues(model), rstudent(model), cex=(10/max(cook))*cook)  
    abline(h=c(-2, 0, 2), lty=2)
    abline(v=c(2, 3)*length(coef(model))/length(rstudent(model)), lty=1)
    identify(hatvalues(model), rstudent(model), case.names)  
}

inflPlot0(mod.duncan, rownames(Duncan)) # remember to exit identify!

            # more sophisticated version, illustrating various aspects
            #   of function definition

inflPlot <- function(model, scale=10, col=c(1, 2),
    identify=TRUE, labels=names(rstud), ... ) {
    # Plot hatvalues, Studentized residuals, and Cook's distances
    #  for a linear or generalized linear model
    # Arguments:
    #  model: an lm or glm model object
    #  scale: a scaling factor for the circles representing Cook's D
    #  col: colors for non-noteworthy and noteworthy points
    #  identify points: label noteworthy points (TRUE or FALSE)
    #  labels: for identified points
    hatval <- hatvalues(model)
    rstud <- rstudent(model)
    cook <- sqrt(cooks.distance(model))
    scale <- scale/max(cook, na.rm = TRUE)
    p <- length(coef(model))
    n <- length(rstud)
    cutoff <- sqrt(4/(n - p))
    plot(hatval, rstud, xlab = "Hat-Values", ylab = "Studentized Residuals",
        cex=scale*cook, col=ifelse(cook > cutoff, col[2], col[1]), ...)  
                                                       # note use of ...
    abline(v = c(2, 3)*p/n, lty = "dashed")
    bonf <- qt(.025/n, df = n - p - 1, lower.tail=FALSE)
    abline(h=c(-bonf, -2, 0, 2, bonf), lty="dashed")
    if (identify){
        noteworthy <- cook > cutoff | abs(rstud) > bonf | hatval > 2*p/n
        pos <- ifelse(hatval - mean(range(hatval, na.rm=TRUE)) <= 0, 4, 2)
        text(hatval[noteworthy], rstud[noteworthy], labels[noteworthy],
            pos = pos[noteworthy])
        return(which(noteworthy))
    }
    else return(invisible(NULL))
}

inflPlot(mod.duncan)

inflPlot(mod.duncan, ylim=c(-3, 4), las=1, col=gray(c(0.5, 0)))

# Basic matrix operations

(A <- matrix(c(1, 2, -4, 3, 5, 0), nrow=2, ncol=3))
(B <- matrix(1:6, 2, 3))
(C <- matrix(c(2, -2, 0, 1, -1, 1, 4 ,4, -4), 3, 3, byrow=TRUE))

A + B  # addition
A - B  # subtraction
2*A    # product of a scalar and a matrix
-A     # negation

A %*% C  # matrix product

A %*% B  # not conformable for matrix multiplication

    # vectors are treated as row or column vectors as needed

a <- rep(1, 3)
b <- c(1, 5, 3)
C %*% a

a %*% C

a %*% b  # inner product
outer(a, b)  # outer product
a %o% b  # equivalent
outer(a, b, `==`)  # generalized outer product

t(B)  # matrix transpose

solve(C)  # matrix inverse
solve(C) %*% C  # check

library(MASS)  # for fractions()
fractions(solve(C))

fractions(solve(C, b))  # solve matrix equation Cx = b

fractions(solve(C) %*% b)  # equivalent

    # illustration: naive computation of LS regression

X <- cbind(1, as.matrix(Prestige[,1:3]))  # attach the constant
y <- Prestige[, 4]
head(X)  # first 6 rows
head(y)

head(Prestige[, 4, drop=FALSE]) # as a one-column matrix

solve(t(X) %*% X) %*% t(X) %*% y  # LS coefficients

lm(prestige ~ education + income + women, data=Prestige) # check?

as.vector(solve(t(X) %*% X) %*% t(X) %*% y) # coerce to vector, loses names

    # eigenvalues and eigenvectors

R <- with(Prestige, cor(cbind(education, income, women)))
R  # correlation matrix

eigen(R)  # principal-components analysis (eigenvalues and vectors)

det(R)  # determinant

diag(R)  # extract diagonal

diag(R) <- NA  # set diagonal
R
mean(abs(R), na.rm=TRUE) # mean of absolute off-diagonal elements

diag(1:3)  # make diagonal matrix

diag(3)  # order-3 identity matrix

    # some other matrix-algebra functions: qr(), svd(), 
    #               chol() (Choleski factorization)

# Control structures

    # conditionals

        # if ... else

abs1 <- function(x) if (x < 0) -x else x
abs1(-5)
abs1(5)

abs1(-3:3)  # wrong! the first element, -3, controls the result

        # ifelse (vectorized)

abs2 <- function(x) ifelse(x < 0, -x, x)
abs2(-3:3)

        # cascading conditionals

sign1 <- function(x){
    if (x < 0) -1
        else if (x > 0) 1
            else 0
}

sign1(-5)

sign2 <- function(x){
    ifelse (x < 0, -1,
        ifelse(x > 0, 1, 0))
}

sign2(c(-5, 0, 10))

        # switch (time permitting)

convert2meters <- function(x, 
    units=c("inches", "feet", "yards", "miles")) {
    units <- match.arg(units)
    switch(units,
        inches = x * 0.0254,
        feet = x * 0.3048,
        yards = x * 0.9144,
        miles = x * 1609.344)
}

convert2meters(10, "inches")
convert2meters(3, "feet")
convert2meters(100, "yards")
convert2meters(5, "miles")
convert2meters(7, "fathoms")

    # iteration

        # for loops

prod(1:5)    # factorial, ok for small numbers
gamma(5 + 1) # better
factorial(5) # best
factorial

fact1 <- function(x){
    if (x <= 1) return(1)
    f <- 1  # initialize
    for (i in 1:x) f <- f * i  # accumulate product
    f  # return result
}

fact1(5)

fact1(-5)  # wrong!

fact2 <- function(x) {  # with input checks
    if ((!is.numeric(x)) || (x != floor(x))
        || (x < 0) || (length(x) > 1))
        stop("x must be a non-negative integer")
    f <- 1  # initialize
    for (i in 1:x) f <- f * i  # accumulate product
    f  # return result
}

fact2(5)
fact2(-5)
fact2(1.5)

        # while loops

fact3 <- function(x){
    if ((!is.numeric(x)) || (x != floor(x))
        || (x < 0) || (length(x) > 1))
        stop("x must be a non-negative integer")
    i <- f <- 1  # initialize
    while (i <= x) {
        f <- f * i  # accumulate product
        i <- i + 1  # increment counter
    }
    f  # return result
}

fact3(5)

        # repeat loops (time permitting)

fact4 <- function(x) {
    if ((!is.numeric(x)) || (x != floor(x))
        || (x < 0) || (length(x) > 1))
        stop("x must be a non-negative integer")
    i <- 1  # initialize
    f <- 1
    repeat {
        f <- f * i  # accumulate product
        i <- i + 1  # increment counter
        if (i > x) break  # termination test
    }
    f  # return result
}

fact4(5)

    # recursion

fact5 <- function(x){
    if (x <= 1) 1  # termination condition
    else x * fact5(x - 1)  # recursive call
}

fact5(5)

trace(fact5)
fact5(5)
untrace(fact5)

      # recursion using Recall() (time permitting)

fact6 <- function(x, verbose=FALSE){
    if (verbose) cat("x = ", x, "\n")
    if (x <= 1) 1  # termination condition
    else x * Recall(x - 1, verbose=verbose)  # recursive call, 
                                             #  not using function name
}

fact6(5)
fact6(5, verbose=TRUE)

# Avoiding loops (as time permits)

    # the "apply" family

        # apply (arrays)

head(DavisThin, 10)  # first 10 rows
dim(DavisThin)
?DavisThin

DavisThin$thin.drive <- apply(DavisThin, 1, sum) # row sums
head(DavisThin$thin.drive, 10)

apply(DavisThin, 2, mean) # column means

colMeans(DavisThin) # more efficient (rowMeans, colSums, rowSums too)

DavisThin$thin.drive <- NULL  # remove thin.drive
DavisThin[1, 2] <- DavisThin[2, 4] <- 
    DavisThin[10, 3] <- NA  # some missing data
head(DavisThin, 10)

head(apply(DavisThin, 1, sum), 10)

head(apply(DavisThin, 1, function(x) 7*mean(x, na.rm=TRUE)), 10)  
                            # "anonymous" function

DavisThin[1, 2:5] <- NA  # create some more missing data
head(DavisThin, 10)

make.scale <- function(items) {
    if (sum(is.na(items)) >= 4) NA
    else 7*mean(items, na.rm=TRUE)
}

head(apply(DavisThin, 1, make.scale), 10)


        # lapply, sapply (lists)

thin.list <- as.list(DavisThin)
str(thin.list)  # structure of the result

lapply(thin.list, mean, na.rm=TRUE) # returns list (na.rm passed to mean)

lapply(thin.list, function(x) mean(x, na.rm=TRUE)) # equivalent

sapply(thin.list, mean, na.rm=TRUE) # simplifies result

        # mapply (multiple arguments)

integrate(dnorm, lower=-1.96, upper=1.96) # integrate normal density

(low <- c(-Inf, -3:3))
(high <- c(-3:3, Inf))
(P <- mapply(function(lo, hi) integrate(dnorm, lo, hi)$value, 
    lo=low, hi=high))
sum(P)

pnorm(high) - pnorm(low) # doing it the easy way!
P - (pnorm(high) - pnorm(low)) # check, should be close to 0

        # Vectorize

Integrate <- Vectorize(
    function(fn, lower, upper) integrate(fn, lower, upper)$value,
    vectorize.args=c("lower", "upper")
)

Integrate(dnorm, lower=low, upper=high)

        # tapply (tables of statistics)

View(Moore)  # randomly sample 10 observations

with(Moore, tapply(conformity,
    list(Status=partner.status, Authoritarianism=fcategory), mean))

Moore$fcategory <- factor(Moore$fcategory, # reorder levels
    levels=c("low", "medium", "high"))
Moore$partner.status <- factor(Moore$partner.status,
    levels=c("low", "high"))

with(Moore, tapply(conformity,
    list(Status=partner.status, Authoritarianism=fcategory), mean))

library("car")

Tapply(conformity ~ partner.status*fcategory, mean, data=Moore) # Tapply() from car

        # others: apropos("apply")

    # to loop or not to loop? (as time permits)

time1 <- function(n){  # inefficient!
    a <- NULL
    for(i in 1:n) a <- c(a, i^2)
    a
}

system.time(time1(1e5))

time2 <- function(n){  # better
    a <- numeric(n) # initialize to final length
    for(i in 1:n) a[i] <- i^2
    a
}

system.time(time2(1e6)) # 10 times larger

time3 <- function(n){  # best
    a <- (1:n)^2 # vectorize
    a
}

system.time(time3(1e6))


time4 <- function(n){  # (slightly) inefficient!
    a <- numeric(n)
    for(i in 1:n) a[i] <- 2 * pi * sin(i)
    a
}

system.time(time4(1e6))

time5 <- function(n){  # slightly better
    a <- numeric(n)
    for(i in 1:n)
        a[i] <- sin(i)
    2 * pi * a  # move outside loop
}
system.time(time5(1e6))

time6 <- function(n){  # best
    2 * pi * sin(1:n) # fully vectorized
}
system.time(time6(1e6))

    # don't vectorize for its own sake

matrices <- vector(mode="list", length=10000)  # allocate 
for (i in seq_along(matrices)) # takes awhile
    matrices[[i]] <- matrix(rnorm(10000), 100, 100)

        # simple, using loop
system.time({ 
    S1 <- matrix(0, 100, 100)  # initialize
    for (M in matrices) S1 <- S1 + M  # accumulate
})

        # clever, avoiding loop ;)
system.time(S2 <- apply(array(unlist(matrices), 
    dim = c(100, 100, 10000)), 1:2, sum))

        # a smaller problem (in case the larger problem fails on your machine

system.time({ 
    S1 <- matrix(0, 100, 100)  # initialize
    for (M in matrices[1:1000]) S1 <- S1 + M  # accumulate
})

        # clever ;)
system.time(S2 <- apply(array(unlist(matrices[1:1000]), 
    dim = c(100, 100, 1000)), 1:2, sum))

all(S1 == S2)  # are answers EXACTLY equal (bad idea!)?

max(abs(S1 - S2))  # how different?

all.equal(S1, S2)  # equal within tolerance
?all.equal

    # solution using compiled C++ code

library(Rcpp)

cppFunction('
    NumericMatrix sumMatrices(List lst) {
        NumericMatrix x = lst(0);
        int nr = x.nrow();
        int nc = x.ncol();
        NumericMatrix z(nr, nc);
        int n = lst.size();
        for (int i=0; i < n; i++){
            NumericMatrix x = lst(i);
            for (int j=0; j < nr; j++){
                for (int k=0; k < nc; k++) {
                    z(j, k) += x(j, k);
                };
            };
        };
        return(z);
    }
    ')

system.time(S3 <- sumMatrices(matrices)) # in this case, not better!
all(S1 == S3)  # but identical!

        # and much better than this:

sumMatrices2 <-  function(matrices){
    M <- matrices[[1]]
    nr <- nrow(M)
    nc <- ncol(M)
    S <- matrix(0, nr, nc)  # initialize
    it <- 0
    for (M in matrices){
        it <- it + 1
        cat(it, " ")
        for (i in 1:nr){
            for (j in 1:nc){
                S[i, j] <- S[i, j] + M[i, j]
            }
        }
    }
    cat("\n")
    S
}

system.time(S4 <- sumMatrices2(matrices[1:1000])) # only first 1000 matrices!

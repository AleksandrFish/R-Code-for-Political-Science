# Debugged functions
# John Fox ICPSR 2017-08-06

lregNR <- function(X, y, max.iter=10, tol=1E-6, verbose=FALSE){ # debugged
    # X is the model matrix
    # y is the response vector of 0s and 1s
    # max.iter is the maximum number of iterations
    # tol is a convergence criterion
    # verbose: show iteration history?
    X <- cbind(1, X) # add constant
    b <- previous.b <- rep(0, ncol(X)) # initialize coefficients
    it <- 1 # initialize iteration counter
    while (it <= max.iter){
        if (verbose) cat("\niteration = ", it, ": ", b)
        p <- as.vector(1/(1 + exp(-X %*% b)))  # fixed: coerce p to a vector
        V <- diag(p * (1 - p))
        var.b <- solve(t(X) %*% V %*% X)
        b <- b + var.b %*% t(X) %*% (y - p) # update coefficients
        if (max(abs(b - previous.b)/(abs(previous.b) + 0.01*tol)) < tol) break
        previous.b <- b # update previous coefficients
        it <- it + 1 # increment counter
    }
    if (verbose) cat("\n") # newline
    if (it > max.iter) warning("maximum iterations exceeded")
    list(coefficients=as.vector(b), var=var.b, iterations=it)
}

lregIWLS <- function(X, y, n=rep(1,length(y)), maxIter=10, tol=1E-6){ # debugged!
    # X is the model matrix
    # y is the response vector of observed proportion
    # n is the vector of binomial counts
    # maxIter is the maximum number of iterations
    # tol is a convergence criterion
    X <- cbind(1, X)  # add constant
    b <- bLast <- rep(0, ncol(X))  # initialize
    it <- 1  # iteration index
    while (it <= maxIter){
        eta <- X %*% b
        mu <- 1/(1 + exp(-eta))
        nu <- as.vector(mu*(1 - mu))
        w <- n*nu
        z <- eta + (y - mu)/nu
        b <- lsfit(X, z, w, intercept=FALSE)$coef
        if (max(abs(b - bLast)/(abs(bLast) + 0.01*tol)) < tol)
          break  # fixed: moved this test to *after* computation of new b
        bLast <- b
        it <- it + 1  # increment index
        }
    if (it > maxIter) warning('maximum iterations exceeded')
    Vb <- solve(t(X) %*% diag(w) %*% X)
    list(coefficients=b, var=Vb, iterations=it)
    }


runningMedian <- function(x, length=3){   # debugged!
#   x: a numeric vector
#   length: the number of values for each running median, defaults to 3
    n <- length(x)
    X <- matrix(x, n, length)
    for (i in 2:length) X[1:(n-i+1), i] <- x[-(1:(i-1))] # fixed: 2:length *not* 1:length
    apply(X, 1, median)[1:(n - length + 1)]
}

fib1 <- function(n){  # debugged!
    if (n <= 2) return(1)
    last.minus.1 <- 1
    last.minus.2 <- 1
    for (i in 3:n){
        save <- last.minus.1  # fixed: save last.minus.1 before updating it
        last.minus.1 <- last.minus.1 + last.minus.2
        last.minus.2 <- save  # fixed: then set last.minus.2 to saved value
    }
    last.minus.1
}

# not bugged, but VERY inefficient (see answer to Fibonacci exercise for explanation):

fib0 <- function(n){
    if (n <= 2) return(1)
    Recall(n - 1) + Recall(n - 2)
}

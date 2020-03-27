##--------------------------------------------------------------##
##                  Script for Lecture 8:                       ##
##                       R Graphics                             ##
##                        John Fox                              ##
##   Introduction to the R Statistical Computing Environment    ##
##                          ICPSR                               ##
##                          2018                                ##
##--------------------------------------------------------------##

# Graphics Basics (traditional S/R graphics)

args(plot.default)  # default plot method

?plot
?plot.default

    # points, lines, axes, frames

plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")  # coordinate system
plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="", axes=FALSE) # w/o axes

par("col")  # graphical parameters

names(par())

?par

plot(1:25, pch=1:25, xlab="Symbol Number", ylab="")  # symbols
lines(1:25, type="h", lty="dashed")

plot(26:1, xlab="letters", ylab="",
    pch=letters, axes=FALSE, frame.plot=TRUE)

plot(c(1, 7), c(0, 1), type="n", axes=FALSE,  # lines
    xlab="Line Type (lty)", ylab="")
box() # add frame
axis(1, at=1:6)  # x-axis
for (lty in 1:6) 
    lines(c(lty, lty, lty + 1), c(0, 0.5, 1), lty=lty)

plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
abline(0, 1) # intercept and slope
abline(c(1, -1), lty="dashed")
# horizontal and vertical lines: 
abline(h=seq(0, 1, by=0.1), v=seq(0, 1, by=0.1), col="gray")

    # text

par(mfrow=c(1, 2))  # array of plots

plot(c(0, 1), c(0, 1), axes=FALSE, type="n", xlab="", ylab="",
    frame.plot=TRUE, main="(a)")
text(x=c(0.2, 0.5), y=c(0.2, 0.7),
    c("example text", "another string"))

plot(c(0, 1), c(0, 1), axes=FALSE, type="n", xlab="", ylab="",
    frame.plot=TRUE, main="(b)")
text(locator(3), c("one", "two", "three"))  # left-click 3 times


locator() # returns mouse coordinates, remember to exit

    # plot layout

plot(1:10, type="n", xlab="", ylab="", axes=FALSE, frame=TRUE)
box("outer")
text(5.5, 5.5, "plotting region", cex=1.5)
text(5.5, 4.5, "standard par() settings")
for (side in 1:4){
    for (line in 0:4){
        mtext(paste("line", line), side=side, line=line, adj=0.65) 
    }
}
for (side in 1:4){
    mtext(paste("margin, side", side), side=side, line=1, adj=0.45, cex=1.25)
}

    # mult-panel plots

(oldpar <- par(mfrow=c(2, 2), oma=c(1, 1, 2, 1), mar=c(2, 2, 2, 1)))
n.lines <- par("mar") - 1
for (plot in 1:4){
    plot(1:10, type="n", xlab="", ylab="", axes=FALSE, frame=TRUE)
    box("figure")
    text(5.5, 5.5, paste("panel", plot), cex=1.5)
    if (plot == 1){
        text(5.5, 3.5, "par(mfrow=c(2, 2),\n  oma=c(1, 1, 2, 1),\n  mar=c(2, 2, 2, 1))", 
             cex=1.25)
    }
    for (side in 1:4){
        for (line in 0:n.lines[side]){
            mtext(paste("inner margin line", line), side=side, line=line, cex=0.75) 
        }
    }
}
box("inner")
box("outer")
n.lines <- par("oma") - 1
for (side in 1:4){
    for (line in 0:n.lines[side]){
        mtext(paste("outer margin line", line), side=side, line=line, outer=TRUE)
    }
}

    # arrows and line segments

par(mfrow=c(1, 2))

plot(c(1, 5), c(0, 1), axes=FALSE, type="n", xlab="", ylab="")
arrows(x0=1:5, y0=rep(0.1, 5),
    x1=1:5, y1=seq(0.3, 0.9, len=5), code=3)
title("(a) arrows")

plot(c(1, 5), c(0, 1), axes=FALSE, type="n", xlab="", ylab="")
segments(x0=1:5, y0=rep(0.1, 5),
    x1=1:5, y1=seq(0.3, 0.9, len=5))
title("(b) segments")

    # nicer arrows: p.arrows() in the sfsmisc package
    #  note different arguments, unidirectional arrows
    #  question: how can you easily get bidirectional arrows?

library(sfsmisc)

par(mfrow=c(1, 1))  # restore single panel
plot(c(1, 5), c(0, 1), axes=FALSE, type="n", xlab="", ylab="")
p.arrows(x1=1:5, y1=rep(0.1, 5),
    x2=1:5, y2=seq(0.3, 0.9, len=5), fill="black")

    # polygons

plot(c(0, 1), c(0, 1), type="n", xlab="", ylab="")
polygon(c(0.2, 0.8, 0.8), c(0.2, 0.2, 0.8), col="red")
polygon(c(0.2, 0.2, 0.8), c(0.2, 0.8, 0.8))

    # legend

plot(c(1,5), c(0,1), axes=FALSE, type="n", xlab="", ylab="",
    frame.plot=TRUE)
legend(locator(1), legend=c("group A", "group B", "group C"),
    lty=1:3, pch=1:3, col=c("blue", "green", "red"))
legend("topright", legend=c("group A", "group B", "group C"),
    lty=1:3, pch=1:3, col=c("blue", "green", "red"), inset=0.01)

    # curve

curve(x*cos(25/x), 0.01, pi, n=1000)

curve(sin, 0, 2*pi, ann=FALSE, axes=FALSE, lwd=2)
axis(1, pos=0, at=c(0, pi/2, pi, 3*pi/2, 2*pi),
    labels=c(0, expression(pi/2), expression(pi),
        expression(3*pi/2), expression(2*pi)))
axis(2, pos=0)
curve(cos, add=TRUE, lty="dashed", lwd=2)
legend(pi, 1, lty=1:2, lwd=2, legend=c("sine", "cosine"), bty="n")


    # colors

pie(rep(1, length(palette())), col=palette())

palette()

rainbow(10) # "#RRGGBBTT" (red, green, blue, transparency)
pie(rep(1, 10), col=rainbow(10))

gray(0:9/9) # "#RRGGBB"
pie(rep(1, 10), col=gray(0:9/9))

length(colors())
head(colors(), 20) # first 20 named colors
pie(rep(1, 20), labels=head(colors(), 20), col=head(colors(), 20))


        # for palettes based on psychophysical principles, see colorspace package

# Putting it together (as time permits)

    # diagrams of the standard normal density function

        # showing the area above 1.96

oldpar <- par(mar = c(5, 6, 4, 2) + 0.1)    # leave room on the left
oldpar  # old parameter saved

z <- seq(-4, 4, length=1000)
p <- dnorm(z)
plot(z, p, type="l", lwd=2,
    main=expression("The Standard Normal Density Function" ~~ phi(z)),
    ylab=expression(phi(z) ==
            frac(1, sqrt(2*pi)) * ~~ e^- ~~ frac(z^2, 2)))
abline(h=0, col="gray")
abline(v=0, col="gray")
z0 <- z[z >= 1.96]    # define region to fill
z0 <- c(z0[1], z0)
p0 <- p[z >= 1.96]
p0 <- c(0, p0)
polygon(z0, p0, col="gray")
coords <- locator(2)    # locate head and tail of arrow
arrows(coords$x[1], coords$y[1], coords$x[2], coords$y[2], code=1,
    length=0.125)
text(coords$x[2], coords$y[2], pos=3,   # text above tail of arrow
    expression(integral(phi(z)*dz, 1.96, infinity) == .025))

        # with lines at z = -3:3 (time permitting)

par(oldpar)  # restore graphics parameters

plot(z, p, type="n", xlab="", ylab="", axes=FALSE,
    main=expression("The Standard Normal Density Function" ~~ phi(z)))
axis(1, pos=0, at=-3:3)
abline(h=0)
axis(2, pos=0, at=.1*1:3)
abline(v=0)
curve(dnorm, -4, 4, n=1000, add=TRUE, lwd=2)
text(locator(2), c("z", expression(phi(z))), xpd=TRUE)
for (z0 in -3:3) lines(c(z0, z0), c(0, dnorm(z0)), lty=2)


    # explaining nearest-neighbour local linear regression (time permitting)

oldpar <- par(mfrow=c(2, 2), las=1)   # 2 x 2 array of graphs

UN <- na.omit(UN[, c("ppgdp", "infantMortality")]) # filter NAs for variables used
gdp <- UN$ppgdp/1000  # rescale to $1000s
infant <- UN$infantMortality
ord <- order(gdp)   # sort data by gdp
gdp <- gdp[ord]
infant <- infant[ord]

x0 <- gdp[150]          # focal x = x_(150)
dist <- abs(gdp - x0)   # distance from focal x
h <- sort(dist)[97]     # bandwidth for span of 0.5 (where n = 193)
pick <- dist <= h       # observations within window

plot(gdp, infant, xlab="GDP per Capita ($1000s)", 
     ylab="Infant Mortality Rate per 1000",
     type="n", main="(a) Observations Within the Window\nspan = 0.5")
points(gdp[pick], infant[pick], col="blue")
points(gdp[!pick], infant[!pick], col="gray")
points(gdp[150], infant[150], pch=16, cex=1.5, col="magenta") # focal point
abline(v=x0, col="magenta")    # at focal x
abline(v=c(x0 - h, x0 + h), lty=2, col="blue")  # window
text(x0, par("usr")[4] + 5, expression(x[(150)]), xpd=TRUE, col="magenta")

plot(range(gdp), c(0,1), xlab="GDP per Capita ($1000s)",
     ylab="Tricube Kernel Weight",
     type="n", main="(b) Tricube Weights")
abline(v=x0, col="magenta")
abline(v=c(x0 - h, x0 + h), lty=2, col="blue")

tricube <- function(x, x0, h) {
  z <- abs(x - x0)/h
  ifelse(z < 1, (1 - z^3)^3, 0)
}
tc <- function(x) tricube(x, x0, h) # to use with curve
curve(tc, min(gdp), max(gdp), n=1000, lwd=2, add=TRUE, col="blue")
points(gdp[pick], tricube(gdp, x0, h)[pick], pch=16, col="blue")
abline(h=c(0, 1), col="gray")

plot(gdp, infant, xlab="GDP per Capita ($1000s)", 
     ylab="Infant Mortality Rate per 1000",
     type="n", main="(c) Local Linear Regression")
points(gdp[pick], infant[pick], col="blue")
points(gdp[!pick], infant[!pick], col="gray")
abline(v=x0, col="magenta")
abline(v=c(x0 - h, x0 + h), lty=2, col="blue")
mod <- lm(infant ~ gdp, weights=tricube(gdp, x0, h))
new <- data.frame(gdp=c(x0 - h, x0, x0 + h))
fit <- predict(mod, newdata=new)
lines(c(x0 - h, x0 + h), fit[c(1, 3)], lwd=3, col="magenta") # local regression line
points(x0, fit[2], pch=16, cex=2, col="magenta")
text(x0, par("usr")[4] + 5, expression(x[(150)]), xpd=TRUE, col="magenta")
text(x0 + 1, fit[2] + 2.5, expression(hat(y)[(150)]), adj=c(0, 0), col="magenta")

plot(gdp, infant, xlab="GDP per Capita  ($1000s)", 
     ylab="Infant Mortality Rate (per 1000)",
     main="(d) Complete Local-Linear Estimate", col=gray(0.25))
yhat <- numeric(length(gdp))
for (i in 1:length(gdp)){   # kernel estimate at each x
  x0 <- gdp[i]
  dist <- abs(gdp - x0)
  h <- sort(dist)[97]
  mod <- update(mod, weights=tricube(gdp, x0, h))
  yhat[i] <- predict(mod, newdata=data.frame(gdp=x0))
}
lines(gdp, yhat, lwd=2, col="magenta")
text(gdp[149], infant[149], paste0(" ", rownames(UN)[149]), adj=c(0, 0))
par(oldpar)

    # more on positioning panels in a multi-panel plot (time permitting)

        # using par(mfrow=c(m, n))

par(mfrow=c(2, 2))

x <- seq(0, 1, length=200)
Ey <- rev(1 - x^2)
y <- Ey + 0.1*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(a) monotone, simple", 
    cex.main=1, xlab="", ylab="")
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

x <- seq(0.02, 0.99, length=200)
Ey <- log(x/(1 - x))
y <- Ey + 0.5*rnorm(200)
plot (x, y, axes=FALSE, frame=TRUE, main="(b) monotone, not simple", 
    cex.main=1, xlab="", ylab="")
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

x <- seq(0.2, 1, length=200)
Ey <- (x - 0.5)^2
y <- Ey + 0.04*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(c) non-monotone, simple", 
    cex.main=1, xlab="", ylab="")
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

        # finer control using par(fig=c(x1, x2, y1, y2))

par(oma=c(0, 0, 1, 0), mar=c(2, 3, 3, 2)) # leave room in top outer margin

par(fig=c(0, .5, .5, 1)) # top-left panel
x <- seq(0, 1, length=200)
Ey <- rev(1 - x^2)
y <- Ey + 0.1*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(a) monotone, simple", 
    cex.main=1, xlab="", ylab="", col="gray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

par(fig=c(.5, 1, .5, 1)) # top-right panel
par(new=TRUE)
x <- seq(0.02, 0.99, length=200)
Ey <- log(x/(1 - x))
y <- Ey + 0.5*rnorm(200)
plot (x, y, axes=FALSE, frame=TRUE, main="(b) monotone, not simple", 
    cex.main=1, xlab="", ylab="", col="gray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)

par(fig=c(.25, .75, 0, .5)) # bottom panel
par(new=TRUE)
x <- seq(0.2, 1, length=200)
Ey <- (x - 0.5)^2
y <- Ey + 0.04*rnorm(200)
plot(x, y, axes=FALSE, frame=TRUE, main="(c) non-monotone, simple", 
    cex.main=1, xlab="", ylab="", col="gray", cex=0.75)
lines(x, Ey, lwd=2)
mtext("x", side=1, adj=1)
mtext("y ", side=2, at=max(y), las=1)
title("Nonlinear Relationships", outer=TRUE)

remove(list=objects())  # clean up

# Popular graphics packages

    # lattice for trellis graphs (part of the standard R distribution)

library("lattice")

Prestige$type <- factor(Prestige$type, levels=c("bc", "wc", "prof"))
xyplot(prestige ~ income | type, data=Prestige,
       panel=function(x, y){
           panel.grid()       # grid lines
           panel.xyplot(x, y) # points
           panel.lmline(x, y, lwd=2) # LS line
           panel.loess(x, y, span=0.8, lwd=2, lty=2, col="magenta") # smooth
       }
)

# ggplot2 ("grammar of graphics")

library("ggplot2")

Prestige2 <- na.omit(Prestige)  # removing missing data

ggplot(data=Prestige2, aes(x=income, y=prestige), show.legend=FALSE) + 
    geom_point() +
    geom_smooth(method="lm", se=FALSE, lty="dashed", col="magenta") +
    geom_smooth(method="loess", span=0.8, se=FALSE) +
    facet_grid(. ~ type) 

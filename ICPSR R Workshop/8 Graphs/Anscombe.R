# Anscombe's Quartet

windows(width=7, height=8) # on a Windows computer

par(mfrow=c(2, 2), oma=c(2, 0, 1, 0))

plot(y1 ~ x1, xlab=expression(x[1]), ylab=expression(y[1]), pch=16, col="blue",
     cex=1.25, main="(a)", xlim=c(4, 19), ylim=c(3, 13), data=anscombe, las=1)
abline(print(lm(y1 ~ x1, data=anscombe)), col="red", lwd=2)

plot(y2 ~ x2, xlab=expression(x[2]), ylab=expression(y[2]), pch=16, col="blue",
     cex=1.25, main="(b)", xlim=c(4, 19), ylim=c(3, 13), data=anscombe, las=1)
abline(print(lm(y2 ~ x2, data=anscombe)), col="red", lwd=2)
text(18, 4, "nonlinearity", pos=2)

plot(y3 ~ x3, xlab=expression(x[3]), ylab=expression(y[3]), pch=16, col="blue",
     cex=1.25, main="(c)", xlim=c(4, 19), ylim=c(3, 13), data=anscombe, las=1)
abline(print(lm(y3 ~ x3, data=anscombe)), col="red", lwd=2)
text(18, 4, "outlier", pos=2)

plot(y4 ~ x4, xlab=expression(x[4]), ylab=expression(y[4]), pch=16, col="blue",
     cex=1.25, main="(d)", xlim=c(4, 19), ylim=c(3, 13), data=anscombe, las=1)
abline(print(lm(y4 ~ x4, data=anscombe)), col="red", lwd=2)
text(18, 4, "high-leverage   \ninfluential point", pos=2)

title("Anscombe's Quartet", outer=TRUE)
mtext(expression(hat(y) == 3 + 0.5*x ~~~~ r == 0.82), side=1, line=0, outer=TRUE)

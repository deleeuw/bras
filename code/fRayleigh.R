
fRayleigh <- function (z, i, x, a, b) {
    aii <- a [i, i]
    bii <- b [i, i]
    ax <- sum (a[, i] * x)
    bx <- sum (b[, i] * x)
    xax <- sum (a * outer (x, x))
    xbx <- sum (b * outer (x, x))
    f1 <- (aii * (z ^ 2)) + (2 * ax * z) + xax
    f2 <- (bii * (z ^ 2)) + (2 * bx * z) + xbx
    return (f1 / f2)
}

a <-  matrix (-1, 3, 3)
diag (a) <- 1
b <-  diag (3)
x <- c(1, 1, -1)
zseq <- seq (-8, 8, length = 100)
png("myOne.png")
plot (zseq, fRayleigh (zseq, 1, x, a, b),type="l",cex=3,col="RED",xlab="theta",ylab="lambda")
abline(h=a[1,1] / b[1,1])
dev.off()
x <- c(1,0,1)
png("myTwo.png")
plot (zseq, fRayleigh (zseq, 2, x, a, b),type="l",cex=3,col="RED",xlab="theta",ylab="lambda")
abline(h=a[2,2] / b[2,2])
dev.off()
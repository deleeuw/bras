
f <- function (x, y) {
    (y ^ 3) / 6 - (x * y ^ 2) / 2 + (y * x ^ 2) / 2 - (x ^ 2 )+ (2 * x)
}

x <- y <- seq(1,3,by=.01)
z <- outer(x, y, f)
contour(x, y, z, col = "RED", nlevels = 100, xlab = "x", ylab = "y")

x <- y <- seq(1,3,by=.05)
z <- outer(x, y, f)
persp(x, y, z, col = "RED", border = "BLACK", xlab = "x", ylab = "y", zlab = "f(x,y)")


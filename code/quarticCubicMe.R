quarticCubicMe  <- function (f0 = p, yy = z, x = g, d = 0) {
    f1 <- deriv (f0)
    f2 <- deriv (f1)
    f3 <- deriv (f2)
    f4 <- deriv (f3)
    ff <- f2 - ((f3 - d) ^ 2) / (f4 * 3)
    plot (x, predict (f0, x), type = "l", col = "RED", lwd = 3,  xlab = "x", ylab = "f(x)")
    for (y in yy) {
        y0 <- predict (f0, y)
        y1 <- predict (f1, y)
        y2 <- predict (f2, y)
        y3 <- predict (f3, y)
        y4 <- predict (f4, y)
        dd <- predict (ff, y) / 2
        bn <- polynomial (c(y0, y1, dd, d / 6))
        lines (x, predict (bn, x - y), type = "l", col = "BLUE", lwd = 1)
        abline (v = y)
    }
}
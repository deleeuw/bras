library(polynom)
f0<-polynomial(c(1,-2,0,1))


tryA <- function (f0, a, y, xlim = c(0, 1)) {
    f1<-deriv(f0)
    f2<-deriv(f1)
    f3<-deriv(f2)
    y0 <- predict (f0, y)
    y1 <- predict (f1, y)
    y2 <- predict (f2, y)
    y3 <- predict (f3, y)
    uu <- polynomial ( c (y3 * y1 *2 / 3, -y2, 1))
    print (solve (uu))
    xx <- seq (xlim[1], xlim[2], length = 100)
    yy <- y0 + y1 * (xx - y) + (a / 2) * (xx - y)^2
    plot (xx, predict (f0, xx), type = "l", xlim = xlim, lwd = 3, col = "RED")
    lines (xx, yy, lwd = 3, col = "BLUE")
    abline (v = y, lwd = 2)
    abline (v = y - 2 * y1 / a, lwd = 2)
    abline (h = y0, lwd = 2)
}

tryAA <- tryA <- function (f, g, a, y, xlim = c(0, 1)) {
    y0 <- f (y)
    y1 <- g (y)
    xx <- seq (xlim[1], xlim[2], length = 100)
    yy <- y0 + y1 * (xx - y) + (a / 2) * (xx - y) ^ 2
    a0 <- ((2 * y1) - 1) / (4 * y)
    print (a0)
    yu <- y0 + y1 * (xx - y) + (a0 / 2) * (xx - y) ^ 2
    plot (xx, f (xx), type = "l", xlim = xlim, lwd = 3, col = "RED")
    lines (xx, yy, lwd = 3, col = "BLUE")
    lines (xx, yu, lwd = 3, col = "GREEN")
    abline (v = y, lwd = 2)
    abline (v = y - 2 * y1 / a, lwd = 2)
    abline (h = y0, lwd = 2)
}


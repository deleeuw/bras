library(polynom)
z <- c(-3,-1,1,3) / 4
g <- seq(-3,5,length=100)
p <- polynomial(c(1,1))*polynomial(c(1,-1))*polynomial(c(1,2))*polynomial(c(1,3))

quarticMe  <- function (f0 = p, yy = z, x = g) {
    f1 <- deriv (f0)
    f2 <- deriv (f1)
    f3 <- deriv (f2)
    f4 <- deriv (f3)
    ff <- f2 - (f3 ^ 2) / (f4 * 3)
    plot (x, predict (f0, x), type = "l", col = "RED", lwd = 3,  xlab = "x", ylab = "f(x)")
    for (y in yy) {
        y0 <- predict (f0, y)
        y1 <- predict (f1, y)
        y2 <- predict (f2, y)
        y3 <- predict (f3, y)
        y4 <- predict (f4, y)
        dd <- predict (ff, y) / 2
        bn <- polynomial (c(y0, y1, dd))
        lines (x, predict (bn, x - y), type = "l", col = "BLUE", lwd = 1)
        abline (v = y)
    }
}

itQuartic <- function (f0, xold, d = 0,  itmax = 100, eps = 1e-10, verbose = TRUE) {
    f1 <- deriv (f0)
    f2 <- deriv (f1)
    f3 <- deriv (f2)
    f4 <- deriv (f3)
    ff <- f2 - (( f3 - d) ^ 2) / (f4 * 3)
    fold <- predict (f0, xold)
    itel <- 1
    repeat {
        p1 <- predict (f1, xold)
        p2 <- predict (ff, xold)
        p3 <- polynomial (c (p1, p2, d / 2))
        p4 <- polynomial (c (p2, d))
        rt <- solve (p3)
        st <- predict (p4, rt)
        xnew <- xold + rt[which.max(st)]
        fnew <- predict (f0, xnew)
        lbd <- 1 - predict (f2, xnew) / predict (ff, xnew)
        if (verbose) cat(
           "Iteration: ", formatC (itel, width = 3, format = "d"),
           "xold: ", formatC (xold, digits = 8, width = 12, format = "f"),
           "xnew: ", formatC (xnew, digits = 8, width = 12, format = "f"),
           "fold: ", formatC (fold, digits = 8, width = 12, format = "f"),
           "fnew: ", formatC (fnew, digits = 8, width = 12, format = "f"),
          "lbd: ", formatC (lbd, digits = 8, width = 12, format = "f"),
           "\n")
       if ((abs(xold - xnew) < eps) || (itel == itmax)) break
       itel <-  itel + 1
       xold <- xnew
       fold <- fnew
    }
    return (list (x = xnew, lbd = lbd))
}
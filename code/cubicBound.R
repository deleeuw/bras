library(polynom)

cubicBound <- function (p, a, b, xold = (a + b) / 2, itmax = 100, eps = 1e-10, verbose = TRUE) {
    p0 <- polynomial (p)
    p1 <- deriv (p0)
    p2 <- deriv (p1)
    pa <- predict (p0, a)
    pb <- predict (p0, b)
    kk<- max (predict (p2, c(a,b)))
    fold <- predict (p0, xold)
    itel <- 1
    repeat {
        po <- predict (p1, xold)
        qq <- polynomial (c(fold, po, kk / 2))
        qa <- predict (qq, a - xold)
        qb <- predict (qq, b - xold)
        xnew <- xold - predict (p1, xold) / kk
        qn <- predict (qq, xnew - xold)
        fnew <- predict (p0, xnew)
        if ((xnew <= b) && (xnew >= a)) {
            if (qn > min (qa, qb)) {
               if (qa <= qb) {
                xnew <- a
                fnew <- pa
                } else {
                xnew <- b
                fnew <- pb
                }
            }
        }
       if ((xnew > b) || (xnew < a)) {
            if (qa <= qb) {
                xnew <- a
                fnew <- pa
            } else {
                xnew <- b
                fnew <- pb
            }
        }
    if (verbose) cat(
        "Iteration: ", formatC (itel, width = 3, format = "d"),
        "fold: ", formatC (fold, digits = 8, width = 12, format = "f"),
        "fnew: ", formatC (fnew, digits = 8, width = 12, format = "f"),
        "xold: ", formatC (xold, digits = 8, width = 12, format = "f"),
        "xnew: ", formatC (xnew, digits = 8, width = 12, format = "f"),
        "\n")
    if ((itel == itmax) || ((fold - fnew) < eps)) break
    itel <- itel + 1
    fold <- fnew
    xold <- xnew
    }
    return (list(itel=itel,f=fnew,x=xnew))
}
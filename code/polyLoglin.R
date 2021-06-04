library(polynom)

polyLogLinF <- function (n, x, lbd = rep (1, length (x))) {
    isp <- x !=0
    xx <- x[isp]
    nn <- n[isp]
    ll <- lbd[isp]
    ii <- as.integer (min(xx) : max(xx))
    ii <- ii[ii != 0]
    ind <- outer (ii, xx, "==")
    ksp <- rowSums (ind) > 0
    ii <- ii[ksp]
    ind <- ind[ksp, ]
    imin <- min (ii)
    imax <- max(ii)
    delta <- sum (ii * (ind %*% nn))
    lbb <- ii * (ind %*% ll)
    if (imin > 0) {
        cf <- rep (0, imax + 1)
        cf[1] <- -delta
        cf[1 + ii] <- lbb
        z <- solve (polynomial (cf))
        mu <- max (Re(z[z == Re(z)]))
        lbd[isp] <- (mu ^ xx) * ll
        return (lbd)
    }
    if (imax < 0) {
       cf <- rep (0, 1 - imin)
       cf[1] <- -delta
       cf[1 - ii] <- lbb
       z <- solve (polynomial (cf))
       mu <- 1 / max (Re(z[z == Re(z)]))
       lbd[isp] <- (mu ^ xx) * ll
       return (lbd)
    }
    cf <- rep (0, 1 + imax - imin)
    cf[1 - imin] <- -delta
    cf[1 + ii - imin] <-  lbb
    z <- solve (polynomial (cf))
    mu <- max (Re(z[z == Re(z)]))
    lbd[isp] <- (mu ^ xx) * ll
    return (lbd)
}

polyLogLin <- function (n, x, lbd = rep (1, nrow (x)), itmax = 100, eps = 1e-6, verbose = 1 ) {
    itel <- 1
    fmax <- sum ((n * log (n)) - n)
    fold <- 2*(fmax - sum ((n * log (lbd)) - lbd))
    m <- ncol (x)
    repeat {
        for (j in 1:m) {
            lbd <- polyLogLinF (n, x[, j], lbd)
            fnew <- 2 * (fmax - sum ((n * log (lbd)) - lbd))
             if (verbose == 2) cat (
                "*** coordinate *** ", formatC (j, width = 3, format = "d"),
                "fnew: ", formatC (fnew, digits = 8, width = 12, format = "f"),
                "\n")
            }
        if (verbose) cat(
            "Iteration: ", formatC (itel, width = 3, format = "d"),
            "fold: ", formatC (fold, digits = 8, width = 12, format = "f"),
            "fnew: ", formatC (fnew, digits = 8, width = 12, format = "f"),
            "\n")
        if ((itel == itmax) || ((fold - fnew) < eps)) break
        itel <- itel + 1
        fold <- fnew
     }
     return (list (lbd = lbd, f = fnew, theta = qr.solve (x, log (lbd))))
}
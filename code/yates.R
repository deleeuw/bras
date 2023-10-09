yates <- function (a, b, xold = rep (1, ncol (a)), eps = 1e-10, itmax = 1000, verbose = TRUE, bnd = 0) {
    cc <- crossprod (a)
    nn <- ncol (a)
    if (bnd == 0) {
       tau = sqrt (sum (diag (cc)))
    } else {
        tau = sqrt (max (eigen (cc, only.values = TRUE) $ values))
    }
    atilde <- a / tau
    fold <- sum ((b - atilde %*% xold) ^ 2)
    cold <- Inf
    btilde <- drop(crossprod(atilde, b))
    ctilde <- cc / (tau ^ 2)
    itel <- 1
    repeat {
        xnew <- btilde + (diag(nn) - ctilde) %*% xold
        fnew <- sum ((b - atilde %*% xnew) ^ 2)
        cnew <- sqrt (sum (xold - xnew) ^ 2)
        ratio <- cnew / cold
        if (verbose) cat(
            "Iteration: ", formatC (itel, width = 3, format = "d"),
            "fold: ", formatC (fold, digits = 8, width = 12, format = "f"),
            "fnew: ", formatC (fnew, digits = 8, width = 12, format = "f"),
            "ratio: ", formatC (ratio, digits = 8, width = 12, format = "f"),
            "\n")
        if ((itel == itmax) || ((fold - fnew) < eps)) break
        itel <- itel + 1
        fold <- fnew
        cold <- cnew
        xold <- xnew
    }
    return (list (itel = itel, x = xnew / tau, f = fnew, ratio = ratio))
}
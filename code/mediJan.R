mediJan <- function (x, theta, mold = mean (x), itmax = 100, eps = 1e-10, verbose = TRUE) {
    wold <- sqrt (((x - mold) ^ 2) + (theta ^ 2))
    fold <- sum (wold)
    itel <- 1
    mall <- mold
    repeat {
        mnew <- sum (x / wold) / sum (1 / wold)
        mall <- c(mall, mnew)
        wnew <- sqrt (((x - mnew) ^ 2) + (theta ^ 2))
        fnew <- sum (wnew)
        if (verbose) cat(
           "Iteration: ", formatC (itel, width = 3, format = "d"),
           "fold: ", formatC (fold, digits = 8, width = 12, format = "f"),
           "fnew: ", formatC (fnew, digits = 8, width = 12, format = "f"),
           "mold: ", formatC (mold, digits = 8, width = 12, format = "f"),
           "mnew: ", formatC (mnew, digits = 8, width = 12, format = "f"),
           "\n")
       if (((fold - fnew) < eps) || (itel == itmax)) break
       mold <- mnew
       wold <- wnew
       fold <- fnew
       itel <- itel + 1
      }
      dev <-  ((x - mnew) ^ 2) / (((x - mnew) ^ 2) + (theta ^ 2))
      rat <-( mall - mnew)/ c(Inf, mall[-length(mall)]-mnew)
      kappa <- sum (dev / wnew) / sum (1 / wnew)
      return (list (f = fnew, mu = mnew, kappa = kappa,  dev = dev, w = 1 / wnew, rat = rat))
}

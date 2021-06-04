

bls <-
  function (z,
            y,
            xold = rep(0, ncol(y)),
            blocks = as.list(1:ncol(y)),
            itmax = 100,
            eps = 1e-10,
            verbose = TRUE) {
    nblocks <- length (blocks)
    fold <- sum((z - y %*% xold) ^ 2)
    xopt <- qr.solve(y, z)
    eold <- sqrt (sum ((xold - xopt) ^ 2))
    itel <- 1
    repeat {
      xwork <- xold
      for (i in 1:nblocks) {
        u <- drop (y %*% xwork)
        yact <- y[, blocks[[i]], drop = FALSE]
        xact <- xwork[blocks[[i]]]
        yres <- z - (u - yact %*% xact)
        xwork[blocks[[i]]] <- qr.solve (yact, yres)
      }
      xnew <- xwork
      fnew <- sum((z - y %*% xnew) ^ 2)
      enew <- sqrt (sum ((xold - xnew) ^ 2))
      if (verbose) {
        cat(
          "itel: ",
          formatC(itel, digits = 3, width = 3),
          "fold: ",
          formatC(
            fold,
            digits = 6,
            width = 10,
            format = "f"
          ),
          "fnew: ",
          formatC(
            fnew,
            digits = 6,
            width = 10,
            format = "f"
          ),
          "ratio: ",
          formatC(
            enew / eold,
            digits = 6,
            width = 10,
            format = "f"
          ),
          "\n"
        )
      }
      if ((abs(fold - fnew) < eps) || (itel == itmax))
        break()
      fold <- fnew
      eold <- enew
      xold <- xnew
      itel <- itel + 1
    }
    return (x)
  }
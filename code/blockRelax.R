


blockRelax <-
  function (f,
            x,
            g,
            itmax = 100,
            eps = 1e-8,
            verbose = TRUE) {
    k <- split (1:length (x), g)
    m <- length (k)
    fold <- f (x)
    itel <- 1
    blockFun <- function (z,  g,  y, i) {
      y[i] <- z
      return (g (y))
    }
    repeat {
      for (i in 1:m) {
        kk <- k[[i]]
        o <-
          optim (
            x[kk],
            blockFun,
            gr = NULL,
            g = f,
            y = x,
            i = kk,
            method = "BFGS"
          )
        x[kk] <- o$par
        fnew <- o$value
      }
      if (verbose)
        cat(
          "Iteration: ",
          formatC (itel, width = 3, format = "d"),
          "fold: ",
          formatC (
            fold,
            digits = 8,
            width = 12,
            format = "f"
          ),
          "fnew: ",
          formatC (
            fnew,
            digits = 8,
            width = 12,
            format = "f"
          ),
          "\n"
        )
      if ((itel == itmax) || ((fold - fnew) < eps))
        break
      itel <- itel + 1
      fold <- fnew
    }
    return (list (x = x, f = fnew))
  }

bls <- function (b) {
  set.seed (12345)
  yvec <- rnorm (100)
  xmat <- matrix (rnorm (300), 100, 3)
  zmat <- matrix (rnorm (200), 100, 2)
  res <- yvec - xmat %*% b[1:3] - zmat %*% b[4:5]
  return (sum (res ^ 2))
}

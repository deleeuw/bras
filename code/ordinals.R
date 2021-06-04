library(quadprog)

index <- function (i, j, n) {
  return (j + (i - 1) * n)
}

difmat <- function (n) {
  m1 <- ifelse(outer(1:(n - 1),1:n,"-") == -1, 1, 0)
  m2 <- ifelse(outer(1:(n - 1),1:n,"-") == 0,-1, 0)
  return (m1 + m2)
}

dmonreg <-
  function (y, w = rep (1, length (y)), a = t(difmat(length(y)))) {
    solve.QP (diag (w), w * y, a)
  }

fmonreg <-
  function (y, w = diag (length (y)), a = t(difmat(length(y)))) {
    solve.QP (w, drop (w %*% y), a)
  }

normalize <- function (y, d) {
  y <- y - sum (y * d) / sum (d)
  return (y / sqrt (sum (d * (y ^ 2))))
}

ordinals <-
  function (x, level = rep("OR", ncol(x)), itmax = 1000, eps = 1e-6, verbose = TRUE) {
    m <- ncol (x)
    n <- nrow (x)
    g <- as.list (1:m)
    d <- as.list (1:m)
    y <- as.list (1:m)
    k <- 1:m
    itel <- 1
    fold <- Inf
    for (j in 1:m) {
      g[[j]] <- ifelse (outer (x[,j], levels(x[,j]), "=="), 1, 0)
      d[[j]] <- colSums (g[[j]]) / n
      k[j] <- length (d[[j]])
      y[[j]] <- normalize (1:k[j], d[[j]])
    }
    b <- as.list (1:(m ^ 2))
    z <- as.list (1:(m ^ 2))
    r <- as.list (1:(m ^ 2))
    w <- as.list (1:m)
    f <- 1:m
    for (j in 1:m) {
      w[[j]] <- matrix (0, k[j], k[j])
      for (l in 1:m) {
        if (l != j) {
          bb <- b[[index(j,l,m)]] <- crossprod (g[[j]], g[[l]]) / n
          w[[j]] <- w[[j]] + bb %*% (t(bb) / d[[l]])
        }
      }
      f[j] <-
        max (eigen (w[[j]] / d[[j]], only.values = TRUE)$values)
    }
    repeat {
      fmid <- 0
      for (j in 1:m)
        for (l in 1:m) {
          ii <- index (j, l, m)
          if (l != j) {
            r[[ii]] <- drop(b[[ii]] %*% y[[l]]) / d[[j]]
            if (level[j] == "NO")
              z[[ii]] <- r[[ii]]
            if (level[j] == "OR")
              z[[ii]] <- dmonreg (r[[ii]], d[[j]])$solution
            if (level[j] == "NU")
              z[[ii]] <- sum(d[[j]] * y[[j]] * y[[l]]) * y[[j]]
            fmid <- fmid + sum (d[[j]] * (z[[ii]] - r[[ii]]) ^ 2)
          }
        }
      for (j in 1:m) {
        uu <- 0
        for (l in 1:m)
          if (l != j)
            uu <-
              uu + drop(b[[index(j,l,m)]] %*% z[[index(l,j,m)]])
          vu <- solve (w[[j]], uu)
          vv <-
            y[[j]] - (w[[j]] %*% (y[[j]] - vu)) / (f[j] * d[[j]])
          if (level[j] == "NO")
            y[[j]] <- normalize(vv, d[[j]])
          if (level[j] == "OR")
            y[[j]] <-
              normalize (fmonreg (vv, w[[j]])$solution, d[[j]])
      }
      fnew <- 0
      for (j in 1:m)
        for (l in 1:m) {
          ii <- index (j, l, m)
          if (l != j) {
            r[[ii]] <- drop(b[[ii]] %*% y[[l]]) / d[[j]]
            fnew <- fnew + sum (d[[j]] * (z[[ii]] - r[[ii]]) ^ 2)
          }
        }
      if (verbose)
        cat(
          "Iteration: ", formatC (itel, width = 3, format = "d"),
          "fold: ", formatC (
            fold, digits = 8, width = 12, format = "f"
          ),
          "fmid: ", formatC (
            fmid, digits = 8, width = 12, format = "f"
          ),
          "fnew: ", formatC (
            fnew, digits = 8, width = 12, format = "f"
          ),
          "\n"
        )
      if ((itel == itmax) || ((fold - fnew) < eps))
        break
      itel <- itel + 1
      fold <- fnew
    }
    return (list(
      y = y,b = b,z = z,r = r
    ))
  }

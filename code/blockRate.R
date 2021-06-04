library (numDeriv)

blockRate <-
  function (f,
            x,
            blocks = as.list (1:length(x)),
            numerical = FALSE,
            product_form = FALSE) {
    if (numerical) {
      h <- hessian (f, x)
    } else {
      h <- f (x)
    }
    nvar <- length (x)
    nblocks <- length (blocks)
    nb <- 1:nblocks
    nn <- 1:nvar
    g <-
      sapply (nn, function (i)
        which (sapply (blocks, function (x)
          any (i == x))))
    if (product_form) {
      sder <- diag (nvar)
      for (i in nb) {
        bi <- blocks [[i]]
        ei <- ifelse (outer(nn, bi, "=="), 1, 0)
        sder <-
          (diag(nvar) - ei %*% solve (h[bi, bi], h[bi, , drop = FALSE])) %*% sder
      }
    } else {
      alow <- h * ifelse (outer (g, g, ">="), 1, 0)
      sder <- -solve (alow, h - alow)
    }
    return (sder)
  }
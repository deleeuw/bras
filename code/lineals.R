lineals <- function (x, k = apply (x, 2, max), yold = lapply (k, function (i) 1:i)) {
    m <- ncol (x)
    n <- nrow (x)
    k <- apply (x, 2, unique)
    lu <- cumsum (k)
    ll <- cumsum (c(1,k))[-(m + 1)]
    g <- matrix (0, n, 0)
    for (j in 1:m) {
        g <- cbind(g, ifelse(outer(x[,j],1:k[j],"=="),1,0))
    }
    burt <- crossprod (g)
    d <- lapply(1:4,function(i) colSums(g[,ll[i]:lu[i]]))
    y <- lapply (k, function (i) 1:i)
}
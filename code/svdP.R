library(MASS)

set.seed(12345)

a<-matrix(rnorm(400),100,4)
b<-matrix(rnorm(400),100,4)
c<-matrix(rnorm(400),100,4)

pert <- function (theta) {
    return (a + theta * b + 0.5 * (theta ^2) * c)
}

port <- function (theta, eps, k) {
    z1 <- pert (theta)
    z2 <- pert (theta + eps)
    s1 <- svd (z1)
    s2 <- svd (z2)
    du <- ((s2$u) - (s1$u))/eps
    dv <- ((s2$v) - (s1$v))/eps
    dd <- ((s2$d)-(s1$d))/eps
    return (list(du=du[,k],dv=dv[,k],dd=dd[k]))
}

ana<-function(theta,k) {
    z <- pert(theta)
    s <- svd (z)
    u <- (s$u)[,k]
    v <- (s$v)[,k]
    d <- (s$d)[k]
    dz <- b + theta * c
    dd <- sum (dz * outer (u, v))
    zz <- crossprod(z, dz)+crossprod(dz,z)
    gg <- ginv (crossprod (z)-(d^2)*diag(4))
    dv <- -drop(gg%*%zz%*%v)
    zz <- tcrossprod(z, dz)+tcrossprod(dz,z)
    gg <- ginv (tcrossprod (z)-(d^2)*diag(100))
    du <- -drop(gg%*%zz%*%u)
    return (list(du=du,dv=dv,dd=dd))
}
library(MASS)
set.seed(12345)
a<-crossprod(matrix(rnorm(40),10,4))
b<-crossprod(matrix(rnorm(40),10,4))

evec <- function (eps) {
    ea <- eigen (a)
    vv <- ea$vectors
    ll <- ea$values
    v0 <- vv[,1]
    l0 <- ll[1]
    cv <- crossprod(vv, b%*% vv)
    ab <- a + eps * b
    ev <- eigen (ab)
    vp <- ev$vectors
    v1 <- vp[,1]
    l1 <- ev$values[1]
    dl <- (l1-l0)/eps
    dv <- (v1-v0)/eps
#    print (c(dl, cv[1,1]))
    cc <- drop(crossprod(vv, b %*% v0))
    dd <- l0 - ll
    dd[1] <- 1
    ee<-cc/dd
    ee[1]<-0
#    print (dv)
#    print (drop(vv%*%ee))
#    print(drop(-ginv(a-l0*diag(4))%*%b%*%v0))
    xx<-tcrossprod(vv[,1:2])
    ap<-a%*%xx
    yy<-tcrossprod(vp[,1:2])
    aq<-ab%*%yy
#   print(ap)
#   print(aq)
    ww <-matrix (0,4,4)
    for (s in 1:2) for (t in 1:4) {
        if (s==t) next
        uu<-(cv[s,t] / (ll [t] - ll[s])) * (outer(vv[,t],vv[,s])+outer(vv[,s],vv[,t]))
        ww <- ww + uu
    }
    print((xx-yy)/eps)
    print (ww)
    print ((aq-ap)/eps)
    print ((b%*%xx)-( a%*%ww))
    eigen((b%*%xx)-( a%*%ww))
}
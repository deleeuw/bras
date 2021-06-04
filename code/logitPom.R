library(polynom)
library(multicool)

logitPomRecursive<-function(n) {
pp <- qq <- rr <- as.list(rep(NULL,n))
pp[[1]]<-polynomial(c(0,1))
qq[[1]]<-1
dd<-polynomial(c(0,1,-1))
for (i in 1:(n-1)) {
    pp[[i+1]]<-dd*deriv(pp[[i]])
    qq[[i+1]]<-solve(pp[[i+1]])
    if (i > 1)
        rr[[i]] <- predict(pp[[i]], qq[[i+1]])
}
return (list(pp=pp,qq=qq,rr=rr))
}

logitPomDirect<-function (n) {
pp <- qq <- rr <- as.list(rep(NULL,n))
f1<-Stirling2(n,1:n)
f2<-factorial(0:(n-1))
f3<-(-1)^(0:(n-1))
for  (i in 1:n) {
    f1<-Stirling2(i,1:i)
    f2<-factorial(0:(i-1))
    f3<-(-1)^(0:(i-1))
    pp[[i]] <- polynomial (c(0,f1*f2*f3))
    qq[[i]] <- solve(pp[[i]])
    if (i > 1)
        rr[[i-1]] <- predict (pp[[i-1]], qq[[i]])
}
return (list(pp=pp,qq=qq,rr=rr))
}



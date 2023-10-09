library(polynom)

ff<-function(a, x = seq (-3, 3, by = .001)) {
    plot(x,(x^4)/4,type="l",col="RED",lwd=3)
    lines(x, (a*x^2)+(x*(1-2*a))+(a-3/4), col="BLUE",lwd=2)
}

gg <- function(a) {
    pp <- polynomial(c(a-3/4,1-2*a,a,0,-1/4))
    qq <- deriv (pp)
    rq <- solve (qq)
    fq <- predict (pp, rq)
    return (list (supp=solve(pp), ext=rq, val = fq))
}

hh<-function (a) {
    pp<-polynomial(c(a-1,1-2*a,a))
    return(solve(pp))
}

jj<-function(a) {
    b<-hh(a)
    x<-seq(b[1],b[2],by=.001)
    plot(x,(x^4)/4,type="l",col="RED",lwd=3)
    lines(x, (a*x^2)+(x*(1-2*a))+(a-3/4), col="BLUE",lwd=2)
}

vv <- function (a) {
pp <- polynomial(c(a-3/4,1-2*a,a,0,-1/4))
p1 <- polynomial(c(-1,1))
p2 <- polynomial(c(3 - 4 * a, 2, 1))
print (pp)
print (- (1/ 4) * p1 * p1 * p2)
}

f0<-function (x) log(1+exp(x))

f1<-function (x) exp(x)/(1+exp(x))

g <-function (x, y) f0(y) + t(f1(y)*t(outer(x,y,"-")))+0.125*outer(x,y,"-")^2


png("gforx.png")
y <- seq(-6,6,length=500)
plot(y, y,type="n", xlim = c(-6,6),ylim = c(0,10))
lines(y,drop(g(2,y)),col="BLUE",lwd=2)
abline(h=f0(2))
lines(y,drop(g(-2,y)),col="BLUE",lwd=2)
abline(h=f0(-2))
lines(y,drop(g(-5,y)),col="BLUE",lwd=2)
abline(h=f0(-5))
lines(y,drop(g(5,y)),col="BLUE",lwd=2)
abline(h=f0(5))
dev.off()

png("gfory.png")
x <- seq(-6,6,length=500)
plot(x, f0(x), type="l", col="RED", lwd = 3)
lines(x,drop(g(x,2)),col="BLUE",lwd=2)
abline(v=2)
lines(x,drop(g(x,-2)),col="BLUE",lwd=2)
abline(v=-2)
lines(x,drop(g(x,-5)),col="BLUE",lwd=2)
abline(v=-5)
lines(x,drop(g(x,5)),col="BLUE",lwd=2)
abline(v=5)
dev.off()

png("perspg.png")
persp(x,y,g(x,y),col="RED",phi=30,theta=-30,border=NA)
dev.off()
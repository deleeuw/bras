 x<-seq(.5,10,length=100)
 lx<-log(x)
 u1<-log(1)+(x/1)-1
 u5<-log(5)+(x/5)-1
 l1<-log(1)-(1/x)+1
 l5<-log(5)-(5/x)+1
 png("mylog.png")
 plot(x,lx,type="l",col="RED",lwd=3,ylab="log(x)")
 lines(x,u1,col="BLUE",lwd=2)
 lines(x,u5,col="BLUE",lwd=2)
 lines(x,l1,col="GREEN",lwd=2)
 lines(x,l5,col="GREEN",lwd=2)
 abline(v=1,lwd=2)
 abline(v=5,lwd=2)
dev.off()

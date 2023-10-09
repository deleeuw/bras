iter<-function(x1,x2,itmax=100) {
itel<-1
repeat{
y1<-(x1+x2)/2
y2<-sqrt(y1*x2)
if (itel==itmax) break
x1<-y1
x2<-y2
itel<-itel+1
}
return(list(y1=y1,y2=y2))
}

gevcca<-function(a,b,n=ncol(a),x=rnorm(n),eps=1e-6,itmax=100,verbose=FALSE,printx=FALSE){
da<-diag(a); db<-diag(b); itel<-1
ax<-drop(a%*%x); bx<-drop(b%*%x)
xax<-sum(x*ax); xbx<-sum(x*bx); rold<-xax/xbx
repeat {
  rald<-rold; xmax<-0
    for (j in 1:n) {
        daj<-da[j]; dbj<-db[j]; axj<-ax[j]; bxj<-bx[j]
        qa<-(daj*bxj)-(dbj*axj)
        qb<-(daj*xbx)-(dbj*xax)
        qc<-(xbx*axj)-(xax*bxj)
    if (identical (all.equal (qa, 0), TRUE)) {
      if (identical (all.equal (qb, 0), TRUE)) {
        th <- 0
      } else {
        th <- - qc / qb
      }
    } else {
            ds<-sqrt((qb^2)-4*qa*qc)
            th<-(ds - qb)/(2*qa)
    }
    xmax<-max(xmax,abs(th))
        x[j]<-x[j]+th
        xax<-xax+(2*th*axj)+(daj*th^2)
        xbx<-xbx+(2*th*bxj)+(dbj*th^2)
        ax<-ax+th*a[,j]; bx<-bx+th*b[,j]
        rnew<-xax/xbx
        if (verbose) {
            cat("Cycle: ",formatC(itel,digits=3,width=3),
                "Coordinate: ",formatC(j,digits=3,width=3),
                "Previous Rayleigh: ",formatC(rald,digits=6,width=10,format="f"),
                "Current Rayleigh: ",formatC(rnew,digits=6,width=10,format="f"),
                "\n")
            if (printx) cat("x: ",formatC(x,digits=6,width=10,format="f"),"\n")
            }
        rald<-rnew
        }
    cat("Cycle: ",formatC(itel,digits=3,width=3),
        "****************",
        "Previous Rayleigh: ",formatC(rold,digits=6,width=10,format="f"),
        "Current Rayleigh: ",formatC(rnew,digits=6,width=10,format="f"),
        "\n")
    if (printx) cat("x: ",formatC(x,digits=6,width=10,format="f"),"\n")
    if ((abs(rnew-rold) < eps) || (xmax < eps) || (itel == itmax)) break()
    rold<-rnew; itel<-itel+1
    }
    return(list(x=x,r=rnew))
}

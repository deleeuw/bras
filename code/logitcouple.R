library(rgl)
library(emdbook)

pfunc <- function(x) 1 / (1 + exp (-x))
ffunc <- function (x) -log (pfunc (x))
dfunc <- function (x) pfunc (x) - 1
gfunc <- function(x,y) ffunc (y) + dfunc (y) * (x - y) + ( (x - y) ^ 2) / 8
hfunc <- function(x,y) ffunc (x)
xseq <- yseq <- seq(-6, 6, by = .01)
qts <- c(-5,-2,0,2,5)
pts <- data.frame (x = xseq, y = yseq, z = ffunc(xseq))
curve3d (hfunc(x,y),xlim=c(-6,6),ylim=c(-6,6),n=c(121,121),sys3d="rgl",col="RED")
curve3d (gfunc(x,y),xlim=c(-6,6),ylim=c(-6,6),n=c(121,121),sys3d="rgl",col="BLUE",add=TRUE)
with(pts,spheres3d(x,y,z,col="white",radius=0.1))
# rgl.snapshot("logitcouple.png")

png("gforx.png")
plot (xseq, ffunc(xseq), type ="l", col = "RED", lwd =3, xlab = "x", ylab = "g and h")
for (y in qts) {
    lines (xseq, gfunc (xseq, y), col = "BLUE", lwd = 2)
    abline (v = y)
}
dev.off()

png("gfory.png")
plot (xseq, ffunc(xseq), type = "n", xlab = "y", ylab = "g and h")
for (x in qts) {
    lines (yseq, gfunc (x, yseq), col = "BLUE", lwd = 2)
    abline (h = ffunc (x), col = "RED", lwd = "3")
}
dev.off()
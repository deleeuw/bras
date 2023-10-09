x<-seq(0,1,by=.001)

f <- function(a) x^2 - a
g <- function (b) b * x

makeMe <- function (a,b) {
    fa <-  f(a)
    gb <- g(b)
    df <- fa - gb
    ef <- abs (df)
    plot (x, fa, type = "l", col = "RED", lwd = 2,ylim=c(0,1-a))
    lines (x, gb)
    abline(h=max(df))
    abline(h=min(df))
    abline(v=0)
    abline(v=1)
    abline(h=0)
    }

    z <- seq (-3,3,by=.01)

    dev <- function (a, z) {
    fa <-  f(a)
    gb <- outer (x,z)
    df <- gb - fa
    ef <- apply (df, 2, function (x) max (abs (x)))
    plot (z, ef, type="l", col="RED",lwd=2)
    return (c(ef[which.min(ef)], z[which.min(ef)]))
    }

    check <- function (a, b) {
        fa <- f(a)
        gb <- g(b)
        ef = max (abs (fa - gb))
        if ((0 <= b) && (b <= 2))
            ff <- max(-a, 1-a-b, a + (b^2)/4)
        else
            ff <- max (abs (a), abs (1 - a - b))
        return (c(ef, ff))
    }


afunc <- function (a) {
        b1 <- seq (-1,0,by=.01)
        b2 <- seq (0,1,by=.01)
        b3 <- seq (1,2,by=.01)
        b4 <- seq (2,3,by=.01)
        f1 <- pmax (abs(a), abs(1-a-b1))
        f2 <- pmax (1-a-b2, a+0.25*b2^2)
        f3 <- pmax (-a, a+0.25*b3^2)
        f4 <- pmax (abs(a), abs(1-a-b4))
        plot (c(b1,b2,b3,b4), c(f1,f2,f3,f4), type= "l", col = "RED", lwd=3)
        abline(v=0,col="BLUE",lwd=2)
        abline(v=1,col="BLUE",lwd=2)
        abline(v=2,col="BLUE",lwd=2)
}

f1 <- function (a) {
    b <- seq (-1,3,by=.001)
    f <- pmax (abs (a), abs (1 - a - b))
    plot (b, f, , type= "l", col = "RED", lwd=3)
    abline (v = 0, col = "BLUE", lwd = 2)
}

ccd <- function (b, itmax = 100, eps = 1e-10, verbose = TRUE, select = "MID") {
    itel <- 1
    bold <- b
    if (bold >= 2) aold <- 0.5 * (1 - bold)
    if ((bold <= 2) && (bold >= 1)) aold <- - 0.125 * (bold ^ 2)
    if ((bold <= 1) && (bold >= 0)) aold <- 0.5 - (bold / 2) - (0.125 * (bold ^ 2) )
    if (bold <= 0) aold <- 0.5 * (1 - bold)
    fold <- fvalue (aold, bold)
    repeat {
        if (aold >= 0.5) bnew <- switch (select, MID = (1 - aold - abs (aold)) / 2,
            UP = 0, LOW = 1 - aold - abs (aold),
            RANDOM = runif (1, 1 - aold - abs (aold), 0))
        if ((aold <= 0.5) && (aold >= -0.125)) bnew <- -2 + 2 * sqrt (2 * (1 - aold))
        if ((aold <= -0.125) && (aold >= -0.5)) bnew <- sqrt (-8 * aold)
        if (aold <= -0.5) bnew <- switch (select, MID = (3 - aold + abs (aold)) / 2,
            UP = 1 - aold + abs (aold), LOW = 2,
            RANDOM = runif (1, 2, 1 - aold + abs (aold)))
        if (bnew >= 2) anew <- 0.5 * (1 - bnew)
        if ((bnew <= 2) && (bnew >= 1)) anew <- - 0.125 * (bnew ^ 2)
        if ((bnew <= 1) && (bnew >= 0)) anew <- 0.5 - (bnew / 2) - (0.125 * (bnew ^ 2) )
        if (bnew <= 0) anew <- 0.5 * (1 - bnew)
        fnew <- fvalue (anew, bnew)
        if (verbose) cat(
           "Iteration: ", formatC (itel, width = 3, format = "d"),
           "bold: ", formatC (bold, digits = 8, width = 12, format = "f"),
           "bnew: ", formatC (bnew, digits = 8, width = 12, format = "f"),
           "aold: ", formatC (aold, digits = 8, width = 12, format = "f"),
           "anew: ", formatC (anew, digits = 8, width = 12, format = "f"),
           "fold: ", formatC (fold, digits = 8, width = 12, format = "f"),
           "fnew: ", formatC (fnew, digits = 8, width = 12, format = "f"),
            "\n")
        if ((max (abs (bold - bnew),abs (aold - anew) ) <  eps) || (itel == itmax)) break
        itel <-  itel + 1
        bold <- bnew
        aold <- anew
        fold <- fnew
    }
    return (list(itel,anew,bnew))
}

fvalue <- function (a,b) {
    h <- function (x) {return (abs(x^2 - b * x - a))}
    f0 <- h (0)
    f1 <- h (1)
    xx <- b / 2
    fx <- ifelse ((xx >=  0) && (xx <= 1), h(xx), 0)
    return (max (f0, f1, fx))
}

upMe <- function (bold, select = "MID") {
    if (bold >= 2) aold <- 0.5 * (1 - bold)
    if ((bold <= 2) && (bold >= 1)) aold <- - 0.125 * (bold ^ 2)
    if ((bold <= 1) && (bold >= 0)) aold <- 0.5 - (bold / 2) - (0.125 * (bold ^ 2) )
    if (bold <= 0) aold <- 0.5 * (1 - bold)
    if (aold >= 0.5) bnew <- switch (select, MID = (1 - aold - abs (aold)) / 2,
            UP = 0, LOW = 1 - aold - abs (aold),
            RANDOM = runif (1, 1 - aold - abs (aold), 0))
    if ((aold <= 0.5) && (aold >= -0.125)) bnew <- -2 + 2 * sqrt (2 * (1 - aold))
    if ((aold <= -0.125) && (aold >= -0.5)) bnew <- sqrt (-8 * aold)
    if (aold <= -0.5) bnew <- switch (select, MID = (3 - aold + abs (aold)) / 2,
            UP = 1 - aold + abs (aold), LOW = 2,
            RANDOM = runif (1, 2, 1 - aold + abs (aold)))
    return (bnew)
}

fixedPlot <- function (select = "MID") {
    x<-seq(-3,5,by=.01)
    y<-rep(0,length(x))
    for (i in 1:length(x)) y[i]=upMe(x[i], select = select)
    plot(x, y, type="l", col="RED", lwd=3, main = select, xlab = "bold", ylab = "bnew")
    abline(0, 1, lwd = 2, col = "BLUE")
}

for (i in c("MID","LOW","UP","RANDOM")) {
png(paste(i,".png",sep=""))
fixedPlot(i)
dev.off()
}
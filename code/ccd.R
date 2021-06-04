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
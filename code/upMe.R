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

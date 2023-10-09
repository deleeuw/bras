folium <- function (x, y) {
    z <- outer (x ^ 3, y ^ 3, "+")- 3 * outer (x,y)
}

par(mfrow=c(1,2))
x <- y <- seq (-2, 2, by = .01)
contour (x, y, folium (x, y), nlevels = 50, col = "RED", lwd = 2)
x <- y <- seq (0, 2, by = .01)
contour (x, y, folium (x, y), nlevels = 50, col = "RED", lwd = 2)
par(mfrow=c(1,1))
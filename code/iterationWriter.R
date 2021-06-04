iterationWriter <- function (a, xold, func, eps = 1e-10, itmax = 25) {
    itel <- 1
    repeat {
        xnew <- func (xold, a)
        cat (formatC (xnew, digits = 15,
                width = 20, format = "f"), "\n")
        if ((abs (xnew - xold) < eps) || (itel == itmax)) {
          break ()
        }
        xold <- xnew
        itel <- itel + 1
     }
}
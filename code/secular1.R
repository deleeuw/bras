```{r}
phi <- c(-3,-2,-1.5,-1.25,0,2,3)
esq <- rnorm(7) ^ 2
lbd <- seq(-5,5,by=.0001)
fun <- sapply (lbd, function (l) sum (esq / (phi - l)))
plot (lbd, fun, type = "l", col = "RED", lwd = 3, ylim=c(-20,20), xlim=c(-4,4))
abline(h=0)
for (l in phi) abline(v=l)
```
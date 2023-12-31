set.seed(12345)

a<-crossprod(matrix(rnorm(400),100,4))/100
b<-crossprod(matrix(rnorm(400),100,4))/100

library(geigen)
e<-geigen(a,b)
i<-which.min(e$values)
l<-e$values[i]
x<-e$vectors[,i]

fR <- function (x) sum(a*outer(x,x))/sum(b*outer(x,x))
fD <- function (x) 2 *(a - fR(x)* b)

source("blockRate.R")

print("(1)(2)(3)(4)")
s1 <- blockRate (fD, x)
print (Mod (eigen(s1)$values))
s2 <- blockRate (fR, x, numerical = TRUE)
print (Mod (eigen(s2)$values))
s3 <- blockRate (fD, x, product_form = TRUE)
print (Mod (eigen(s3)$values))
s4 <- blockRate (fR, x, numerical = TRUE, product_form = TRUE)
print (Mod (eigen(s4)$values))

print("(1)(3)(2)(4)")
t1 <- blockRate (fD, x, blocks = list (1,3,2,4))
print (Mod (eigen(t1)$values))
t2 <- blockRate (fR, x, blocks = list (1,3,2,4), numerical = TRUE)
print (Mod (eigen(t2)$values))
t3 <- blockRate (fD, x, blocks = list (1,3,2,4), product_form = TRUE)
print (Mod (eigen(t3)$values))
t4 <- blockRate (fR, x, blocks = list (1,3,2,4), numerical = TRUE, product_form = TRUE)
print (Mod (eigen(t4)$values))

print("(3)(4)(1)(2)")
r1 <- blockRate (fD, x, blocks = list (3,4,1,2))
print (Mod (eigen(r1)$values))
r2 <- blockRate (fR, x, blocks = list (3,4,1,2), numerical = TRUE)
print (Mod (eigen(r2)$values))
r3 <- blockRate (fD, x, blocks = list (3,4,1,2), product_form = TRUE)
print (Mod (eigen(r3)$values))
r4 <- blockRate (fR, x, blocks = list (3,4,1,2), numerical = TRUE, product_form = TRUE)
print (Mod (eigen(r4)$values))

print("(1,2)(3,4)")
u1 <- blockRate (fD, x, blocks = list (c(1,2),c(3,4)))
print (Mod (eigen(u1)$values))
u2 <- blockRate (fR, x, blocks = list (c(1,2),c(3,4)), numerical = TRUE)
print (Mod (eigen(u2)$values))
u3 <- blockRate (fD, x, blocks = list (c(1,2),c(3,4)), product_form = TRUE)
print (Mod (eigen(u3)$values))
u4 <- blockRate (fR, x, blocks = list (c(1,2),c(3,4)), numerical = TRUE, product_form = TRUE)
print (Mod (eigen(u4)$values))

print("(1)(2,3,4)")
v1 <- blockRate (fD, x, blocks = list (1,c(2,3,4)))
print (Mod (eigen(v1)$values))
v2 <- blockRate (fR, x, blocks = list (1,c(2,3,4)), numerical = TRUE)
print (Mod (eigen(v2)$values))
v3 <- blockRate (fD, x, blocks = list (1,c(2,3,4)), product_form = TRUE)
print (Mod (eigen(v3)$values))
v4 <- blockRate (fR, x, blocks = list (1,c(2,3,4)), numerical = TRUE, product_form = TRUE)
print (Mod (eigen(v4)$values))

print("(1,2,3)(4)")
w1 <- blockRate (fD, x, blocks = list (c(1,2,3),4))
print (Mod (eigen(w1)$values))
w2 <- blockRate (fR, x, blocks = list (c(1,2,3),4), numerical = TRUE)
print (Mod (eigen(w2)$values))
w3 <- blockRate (fD, x, blocks = list (c(1,2,3),4), product_form = TRUE)
print (Mod (eigen(w3)$values))
w4 <- blockRate (fR, x, blocks = list (c(1,2,3),4), numerical = TRUE, product_form = TRUE)
print (Mod (eigen(w4)$values))
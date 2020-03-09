
# figure 1.1 left
data1.m <- c(129.6, 130.5, 130.6, 131.1, 131.5, 132.0, 133.5, 133.9, 135.2, 135.3, 135.4, 136.7, 138.7, 139.3, 141.2)
data1.f <- c(128.5, 128.9, 129.2, 131.5, 131.7, 132.1, 132.1, 132.2, 133.0, 133.4, 133.8, 134.0, 135.2, 137.8, 138.6)
plot(c(rep(1, 15), rep(2, 15)), c(data1.m, data1.f), xlim=c(0, 3), ylim=c(120, 150), xlab="", ylab="height", xaxt="n")
axis(side=1, at=c(1, 2), labels=c("male", "female"))
lines(c(-0.2, 3.2), c(120, 120), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(125, 125), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(130, 130), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(135, 135), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(140, 140), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(145, 145), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(150, 150), lwd=0.5, lty=3)


# figure 1.1 right
data2.m <- c(138.4, 138.9, 142.8, 143.2, 144.0, 144.1, 144.4, 144.6, 144.8, 145.8, 146.2, 146.7, 147.0, 148.6, 149.5)
data2.f <- c(142.3, 142.4, 143.4, 144.0, 144.3, 145.6, 145.9, 147.7, 147.9, 148.6, 149.2, 149.7, 151.6, 151.9, 152.0)
plot(c(rep(1, 15), rep(2, 15)), c(data2.m, data2.f), xlim=c(0, 3), ylim=c(130, 160), xlab="", ylab="height", xaxt="n")
axis(side=1, at=c(1, 2), labels=c("male", "female"))
lines(c(-0.2, 3.2), c(130, 130), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(135, 135), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(140, 140), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(145, 145), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(150, 150), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(155, 155), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(160, 160), lwd=0.5, lty=3)


# figure 2.2 (a)
d <- c(3, 4, 4, 5, 5, 5, 5, 6, 6, 7)
hist(d, breaks=seq(-0.5, 12.5, 1), xlab="score", ylab="freq.", xlim=c(0, 12), ylim=c(0, 4), col="gray")


# figure 2.2 (b)
d <- c(1, 2, 4, 4, 5, 5, 5, 6, 8, 10)
hist(d, breaks=seq(-0.5, 12.5, 1), xlab="score", ylab="freq.", xlim=c(0, 12), ylim=c(0, 4), col="gray")


# figure 2.3 (a)
d <- c(43.3, 43.1, 42.6, 42.4, 42.2, 41.8, 41.7, 41.6, 41.5, 41.4, 
40.8, 40.6, 40.5, 40.4, 40.4, 40.3, 40.2, 39.9, 39.9, 39.8,
39.7, 39.6, 39.6, 39.5, 39.4, 39.3, 38.9, 38.9, 38.8, 38.8,
38.7, 38.7, 38.6, 38.6, 38.5, 38.4, 38.3, 38.2, 38.1, 38.1,
37.6, 37.4, 37.1, 37.8, 37.6, 37.5, 37.4, 37.3, 37.2, 37.1,
37.1, 36.6, 36.5, 36.5, 36.4, 36.3, 36.2, 36.1, 35.4, 35.3,
35.2, 35.1, 35.1, 34.7, 34.3, 34.2, 33.2, 33.1, 32.7, 31.5)
hist(d, xlab="score", ylab="freq.", xlim=c(25, 50), ylim=c(0, 14), col="gray", breaks=15)


# figure 2.3 (b)
d <- c(47.3, 46.1, 45.6, 45.1, 44.5, 44.4, 43.7, 42.6, 42.5, 42.5, 
41.4, 41.8, 41.6, 41.5, 40.7, 40.5, 40.4, 40.3, 40.1, 40.1,
39.9, 39.8, 39.7, 39.6, 39.6, 39.5, 39.4, 38.9, 38.8, 38.8,
38.7, 38.7, 38.6, 38.4, 38.2, 38.1, 38.1, 37.8, 37.7, 37.5,
37.5, 37.4, 37.3, 37.3, 37.1, 36.8, 36.8, 36.7, 36.6, 36.4,
36.2, 35.4, 35.4, 35.4, 35.3, 35.2, 34.9, 34.8, 34.7, 34.7,
33.9, 33.8, 33.7, 33.3, 33.1, 32.8, 32.5, 32.1, 31.7, 29.5)
hist(d, xlab="score", ylab="freq.", xlim=c(25, 50), ylim=c(0, 14), col="gray", breaks=15)


# figure 2.4
d <- rnorm(2000, mean=145.5, sd=4.0)
boxplot(d, ylim=c(130, 160), ylab="length (cm)")


# figure 3.1
plot(c(0:10), dbinom(c(0:10), 10, p=0.5), type="o", xlim=c(0, 10), ylim=c(0, 0.26), xlab="x", ylab="prob.")


# figure 3.3
d <- c(3, 7, 11, 12, 13, 14, 15, 16, 17, 17, 18, 18, 18, 19, 19, 19, 20, 20, 21, 21, 21, 22, 22, 23, 23, 24, 24, 24, 25, 25, 25, 26, 26, 26, 26, 27, 27, 27, 28, 28, 28, 29, 29, 29, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 31, 31, 31, 31, 32, 32, 33, 33, 33, 33, 33, 34, 34, 34, 35, 35, 35, 36, 36, 36, 37, 37, 38, 38, 39, 39, 39, 40, 40, 41, 41, 41, 42, 42, 42, 43, 43, 44, 45, 46, 47, 48, 49, 53, 57)
hist(d, col="gray", xlab="weight gain", ylab="freq.", breaks=10)


# figure 3.4 (a)
x <- seq(-4, 4, 0.01)
y <- dnorm(x, 0, 1)
plot(x, y, type="l")


# figure 3.4 (b)
x1 <- seq(0, 40, 0.01)
y1 <- dnorm(x1, mean=10, sd=2)
x2 <- seq(0, 40, 0.01)
y2 <- dnorm(x2, mean=20, sd=4)
plot(x1, y1, type="l", xlab="x", ylab="y")
lines(x2, y2)


# figure 3.5
x <- seq(-3, 3, 0.01)
y <- dnorm(x, 0, 1)
plot(x, y, type="l", xlab="", ylab="")
lines(c(0, 0), c(-0.01, y[301]), col="blue", lwd=0.5)
lines(c(1, 1), c(-0.01, y[401]), col="blue", lwd=0.5)
lines(c(2, 2), c(-0.01, y[501]), col="blue", lwd=0.5)
text(0.5, 0.17, "0.3413")
text(1.5, 0.04, "0.4772")
arrows(0, 0.15, 1, 0.15, code=3, length=0.1, lwd=0.5)
arrows(0, 0.02, 2, 0.02, code=3, length=0.1, lwd=0.5)


# figure 3.6 (a)
n <- 3
M <- 100000
mean.d <- numeric(M)
for(k in 1:M){
	d <- runif(n, 0, 10)
	mean.d[k] <- mean(d)
}
hist(mean.d, breaks=50, col="gray", ylim=c(0, 5000))


# figure 3.6 (b)
n <- 10
M <- 100000
mean.d <- numeric(M)
for(k in 1:M){
	d <- runif(n, 0, 10)
	mean.d[k] <- mean(d)
}
hist(mean.d, breaks=100, col="gray", ylim=c(0, 5000))


# figure 3.6 (c)
n <- 1000
M <- 100000
mean.d <- numeric(M)
for(k in 1:M){
	d <- runif(n, 0, 10)
	mean.d[k] <- mean(d)
}
hist(mean.d, breaks=100, col="gray", ylim=c(0, 5000))


# figure 3.6 (d)
n <- 1000
M <- 100000
mean.d <- numeric(M)
for(k in 1:M){
	d <- runif(n, 0, 10)
	mean.d[k] <- mean(d)
}
qqnorm(mean.d)


# figure 4.1
d <- rnorm(2000, mean=145.5, sd=4)
hist(d, col="gray", xlim=c(130, 160), xlab="height", breaks=25)


# figure 4.2 (a)
N <- 10
d <- rnorm(2000, mean=145.5, sd=4)
mean_x <- c()
sd_x <- c()
se_x <- c()
for (i in 1:10){ 
x <- sample(d, N)
mean_x[i] <- mean(x)
sd_x[i] <- sqrt(var(x))
se_x[i] <- sd_x[i]/sqrt(N)
}
xaxis <- seq(1, 10, 1)
plot(xaxis, mean_x, xlim =c(1, 10), ylim=c(141, 149), xlab="ID", ylab="height")
arrows(xaxis, mean_x+se_x, xaxis, mean_x-se_x, angle=90, length=0.05)
arrows(xaxis, mean_x-se_x, xaxis, mean_x+se_x, angle=90, length=0.05)
lines(c(0, 11), c(145.5, 145.5), lty=2)


# figure 4.2 (b)
N <- 40
d <- rnorm(2000, mean=145.5, sd=4)
mean_x <- c()
sd_x <- c()
se_x <- c()
for (i in 1:10){ 
x <- sample(d, N)
mean_x[i] <- mean(x)
sd_x[i] <- sqrt(var(x))
se_x[i] <- sd_x[i]/sqrt(N)
}
xaxis <- seq(1,10,1)
plot(xaxis, mean_x, xlim =c(1,10), ylim=c(141, 149), xlab="ID", ylab="height")
arrows(xaxis, mean_x+se_x, xaxis, mean_x-se_x, angle = 90, length = 0.05)
arrows(xaxis, mean_x-se_x, xaxis, mean_x+se_x, angle = 90, length = 0.05)
lines(c(0, 11), c(145.5, 145.5), lty=2)


# figure 4.2 (c)
N <- 160
d <- rnorm(2000, mean=145.5, sd=4)
mean_x <- c()
sd_x <- c()
se_x <- c()
for (i in 1:10){ 
x <- sample(d, N)
mean_x[i] <- mean(x)
sd_x[i] <- sqrt(var(x))
se_x[i] <- sd_x[i]/sqrt(N)
}
xaxis <- seq(1,10,1)
plot(xaxis, mean_x, xlim =c(1,10), ylim=c(141, 149), xlab="ID", ylab="height")
arrows(xaxis, mean_x+se_x, xaxis, mean_x-se_x, angle = 90, length = 0.05)
arrows(xaxis, mean_x-se_x, xaxis, mean_x+se_x, angle = 90, length = 0.05)
lines(c(0, 11), c(145.5, 145.5), lty=2)


# figure 4.2 (d)
N <- 640
d <- rnorm(2000, mean=145.5, sd=4)
mean_x <- c()
sd_x <- c()
se_x <- c()
for (i in 1:10){ 
x <- sample(d, N)
mean_x[i] <- mean(x)
sd_x[i] <- sqrt(var(x))
se_x[i] <- sd_x[i]/sqrt(N)
}
xaxis <- seq(1,10,1)
plot(xaxis, mean_x, xlim =c(1,10), ylim=c(141, 149), xlab="ID", ylab="height")
arrows(xaxis, mean_x+se_x, xaxis, mean_x-se_x, angle = 90, length = 0.05)
arrows(xaxis, mean_x-se_x, xaxis, mean_x+se_x, angle = 90, length = 0.05)
lines(c(0, 11), c(145.5, 145.5), lty=2)


# figure 4.3 (a)
N <- 10
d <- rnorm(2000, mean=145.5, sd=2)
mean_x <- c()
sd_x <- c()
se_x <- c()
for (i in 1:10){ 
x <- sample(d, N)
mean_x[i] <- mean(x)
sd_x[i] <- sqrt(var(x))
se_x[i] <- sd_x[i]/sqrt(N)
}
xaxis <- seq(1,10,1)
plot(xaxis, mean_x, xlim =c(1,10), ylim=c(141, 149), xlab="ID", ylab="height")
arrows(xaxis, mean_x+se_x, xaxis, mean_x-se_x, angle = 90, length = 0.05)
arrows(xaxis, mean_x-se_x, xaxis, mean_x+se_x, angle = 90, length = 0.05)
lines(c(0, 11), c(145.5, 145.5), lty=2)


# figure 4.3 (b)
N <- 40
d <- rnorm(2000, mean=145.5, sd=2)
mean_x <- c()
sd_x <- c()
se_x <- c()
for (i in 1:10){ 
x <- sample(d, N)
mean_x[i] <- mean(x)
sd_x[i] <- sqrt(var(x))
se_x[i] <- sd_x[i]/sqrt(N)
}
xaxis <- seq(1,10,1)
plot(xaxis, mean_x, xlim =c(1,10), ylim=c(141, 149), xlab="ID", ylab="height")
arrows(xaxis, mean_x+se_x, xaxis, mean_x-se_x, angle = 90, length = 0.05)
arrows(xaxis, mean_x-se_x, xaxis, mean_x+se_x, angle = 90, length = 0.05)
lines(c(0, 11), c(145.5, 145.5), lty=2)


# figure 4.3 (c)
N <- 160
d <- rnorm(2000, mean=145.5, sd=2)
mean_x <- c()
sd_x <- c()
se_x <- c()
for (i in 1:10){ 
x <- sample(d, N)
mean_x[i] <- mean(x)
sd_x[i] <- sqrt(var(x))
se_x[i] <- sd_x[i]/sqrt(N)
}
xaxis <- seq(1,10,1)
plot(xaxis, mean_x, xlim =c(1,10), ylim=c(141, 149), xlab="ID", ylab="height")
arrows(xaxis, mean_x+se_x, xaxis, mean_x-se_x, angle = 90, length = 0.05)
arrows(xaxis, mean_x-se_x, xaxis, mean_x+se_x, angle = 90, length = 0.05)
lines(c(0, 11), c(145.5, 145.5), lty=2)


# figure 4.3 (d)
N <- 640
d <- rnorm(2000, mean=145.5, sd=2)
mean_x <- c()
sd_x <- c()
se_x <- c()
for (i in 1:10){ 
x <- sample(d, N)
mean_x[i] <- mean(x)
sd_x[i] <- sqrt(var(x))
se_x[i] <- sd_x[i]/sqrt(N)
}
xaxis <- seq(1, 10, 1)
plot(xaxis, mean_x, xlim =c(1,10), ylim=c(141, 149), xlab="ID", ylab="height")
arrows(xaxis, mean_x+se_x, xaxis, mean_x-se_x, angle = 90, length = 0.05)
arrows(xaxis, mean_x-se_x, xaxis, mean_x+se_x, angle = 90, length = 0.05)
lines(c(0, 11), c(145.5, 145.5), lty=2)


# figure 4.4 left
x <- seq(-3, 3, 0.01)
x.norm <- dnorm(x, mean=0, sd=1)
x.tdistr <- dt(x, df=4)
plot(x, x.norm, xlab="x", ylab="prob. density", xlim=c(-3, 3), ylim=c(0, 0.4), type="l", lty=2)
lines(x, x.tdistr)


# figure 4.4 right
x <- seq(-3, 3, 0.01)
x.norm <- dnorm(x, mean=0, sd=1)
x.tdistr <- dt(x, df=29)
plot(x, x.norm, xlab="x", ylab="prob. density", xlim=c(-3, 3), ylim=c(0, 0.4), type="l", lty=2)
lines(x, x.tdistr)


# figure 4.5
d <- rnorm(2000, mean=145.5, sd=4)
N <- 40
mean_x <- c()
sd_x <- c()
se_x <- c()
for (i in 1:20) {
x <- sample(d, N)
mean_x[i] <- mean(x)
sd_x[i] <- sqrt(var(x))
se_x[i] <- sd_x[i]/sqrt(N) 
}
xaxis <- seq(1, 20, 1)
plot(xaxis, mean_x, xlim=c(1, 20), ylim=c(142, 149), xlab="ID", ylab="height")
arrows(xaxis, mean_x+se_x*qt(1-0.025, df=N-1), xaxis, mean_x-se_x*qt(1-0.025, df=N-1), angle = 90, length = 0.05)
arrows(xaxis, mean_x-se_x*qt(1-0.025, df=N-1), xaxis, mean_x+se_x*qt(1-0.025, df=N-1), angle = 90, length = 0.05)
lines(c(0, 21), c(145.5, 145.5), lty=2)


# figure 4.6
x <- seq(-3, 3, 0.01)
x.tdistr <- dt(x, df=19)
plot(x, x.tdistr, xlab="x", ylab="prob. density", xlim=c(-3, 3), ylim=c(0, 0.4), type="l")


# figure 5.1 (a)
data1.m <- c(143.1, 140.9, 147.2, 139.8, 141.3, 150.7, 149.4, 145.6, 146.5, 148.5, 141.2, 136.5, 145.8, 148.1, 144.3)
data1.f <- c(138.7, 142.8, 150.3, 148.4, 141.7, 149.5, 156.5, 144.6, 144.4, 145.7, 148.3, 140.8, 146.2, 149.9, 144.1)
plot(c(rep(1, 15), rep(2, 15)), c(data1.m, data1.f), xlim=c(0, 3), ylim=c(130, 160), xlab="", ylab="height", xaxt="n")
axis(side=1, at=c(1, 2), labels=c("male", "female"))
lines(c(-0.2, 3.2), c(130, 130), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(135, 135), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(140, 140), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(145, 145), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(150, 150), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(155, 155), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(160, 160), lwd=0.5, lty=3)


# figure 5.1 (b)
data2.m <- c(142.3, 142.5, 145.7, 143.5, 144.2, 145.1, 145.9, 145.2, 146.8, 145.7, 145.4, 144.6, 144.2, 145.9, 142.1)
data2.f <- c(143.5, 144.6, 143.4, 146.6, 145.3, 147.7, 147.2, 147.8, 145.3, 145.7, 147.5, 147.2, 148.8, 147.9, 143.3)
plot(c(rep(1, 15), rep(2, 15)), c(data2.m, data2.f), xlim=c(0, 3), ylim=c(130, 160), xlab="", ylab="height", xaxt="n")
axis(side=1, at=c(1, 2), labels=c("male", "female"))
lines(c(-0.2, 3.2), c(130, 130), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(135, 135), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(140, 140), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(145, 145), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(150, 150), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(155, 155), lwd=0.5, lty=3)
lines(c(-0.2, 3.2), c(160, 160), lwd=0.5, lty=3)


# figure 5.3
x <- seq(-3,3, 0.01)
y <- dt(x, df=14)
plot(x, y, type="l", ylab="prob. density")


# figure 5.4
x <- seq(-3,3, 0.01)
y <- dt(x, df=14)
plot(x, y, type="l", ylab="prob. density")


# figure 5.6
X1 <- c(23, 20, 20, 24, 17, 19, 26, 22, 19, 21)
X2 <- c(17, 23, 25, 34, 25, 28, 20, 31, 26, 36)
boxplot(X1, X2, ylim=c(10, 40), ylab="score")
axis(labels=c("X1", "X2"), at=c(1, 2), side=1)


# figure 5.7 (a)
x <- seq(-10, 10, 0.01)
d1 <- dnorm(x, 0, 1)
d2 <- dnorm(x, 2, 1)
plot(x, d1, type="l", xlab="", ylab="", xlim=c(-3,5))
lines(x, d2, lty=2)


# figure 5.7 (b)
x <- seq(-10,10,0.01)
d1 <- dnorm(x,0,1)
d2 <- dnorm(x,0.1,1)
plot(x, d1, type="l", xlab="", ylab="", xlim=c(-3,5))
lines(x, d2, lty=2)


# figure 6.2
x <- seq(0, 15, 0.01)
Fd <- df(x, 3, 44)
plot(x, Fd, xlab="F value", ylab="prob. density", xlim=c(0, 5), ylim=c(0, 0.8), type="l")
qf(0.95, 3, 44)


# figure 6.4 (a)
d <- read.csv("table6-7.csv")
boxplot(d$score ~ factor(d$group), ylim=c(12, 24), xlab="", ylab="score")


# figure 6.4 (b)
d <- read.csv("table6-8.csv")
boxplot(d$score ~ factor(d$group), ylim=c(6, 14), xlab="", ylab="score")


# figure 6.4 (c)
d <- read.csv("table6-9.csv")
boxplot(d$score ~ factor(d$group), ylim=c(50, 90), xlab="", ylab="score")


# figure 7.2 (a)
d <- c(6, 5, 6, 7, 2, 1, 1, 0, 7, 8, 8, 9, 8, 7, 6, 7)
one <-　c(mean(d[1:4]), mean(d[9:12]))
two <-　c(mean(d[5:8]), mean(d[13:16]))
plot(one, xlim=c(0.5, 2.5),ylim=c(0, 10), type="b", xlab="", ylab="desirebility", pch=1, xaxt="n")
points(two, type="b", pch=2)
axis(side=1, at=1:2, labels=c("small", "large"))


# figure 7.2 (b)
d <- c(6, 5, 6, 7, 2, 1, 1, 0, 7, 8, 8, 9, 8, 7, 6, 7)
small <- c(mean(d[1:4]), mean(d[5:8]))
large <- c(mean(d[9:12]), mean(d[13:16]))
plot(small, xlim=c(0.5, 2.5), ylim=c(0, 10), type="b", xlab="", ylab="desirebility",pch=1,xaxt="n")
points(large, type="b", pch=2)
axis(side=1, at=1:2,l abels=c("one", "two"))


# figure 7.4
d <- read.csv("table7-4.csv", header=F)
Cont <- c(mean(d$V1[1:10]), mean(d$V1[11:20]))
Treat <- c(mean(d$V1[21:30]), mean(d$V1[31:40]))
plot(Cont, type="o", xlim=c(0.5, 2.5), ylim=c(25, 40), xlab="", ylab="plant growth (cm)", xaxt="n")
points(c(1, 2), Treat, type="o", pch=2)
axis(side=1, at=c(1, 2), label=c("natural", "artificial"))


# figure 7.6
d <- read.csv("table7-5.csv")
plot(d$number, d$wt, xlab="ID", ylab="weight", ylim=c(100, 135))


# figure 8.2
r <- 0.5    # -1.0〜1.0 で指定
x <- rnorm(1000, 0, 5)
y <- r*x + sqrt(1-r^2)*rnorm(1000, 0, 5)
plot(x, y, xlim=c(-20,20), ylim=c(-20, 20), pch=19, cex=0.1)


# figure 8.3 (a)
r <- 0.9
x <- rnorm(1000, 0, 5)
y <- r*x + sqrt(1-r^2)*rnorm(1000, 0, 5)
y2 <- 0.5*y
plot(x, y2, xlim=c(-20, 20), ylim=c(-20, 20), xlab="", ylab="", cex=0.1, pch=19)


# figure 8.3 (b)
r <- 0.9
x <- rnorm(1000, 0, 2)
y <- r*x + sqrt(1-r^2)*rnorm(1000, 0, 2)
y2 <- 2*y
plot(x, y2, xlim=c(-20, 20), ylim=c(-20, 20), xlab="", ylab="", cex=0.1, pch=19)


# figure 8.3 (c)
x <- runif(1000, -10, 10)
y <- -x^2*0.1 + runif(1000, -1, 1)
plot(x, y, xlim=c(-10, 10), ylim=c(-11, 1), cex=0.1, pch=19)


# figure 8.3 (d)
x <- runif(1000, -10, 10)
y <- -0.01*x^4 + x^2 + runif(1000, -10, 10)
plot(x, y, xlim=c(-10, 10), ylim=c(-10, 35), cex=0.1, pch=19)


# figure 8.4
x <- c(11.7, 11.9, 10.1, 13.6, 12.8, 10.5, 9.8, 10.9, 11.6, 11.8, 13.1, 12.4, 12.2)
y <- c(8.2, 8.2, 7.1, 9.2, 8.7, 7.7, 6.5, 7.7, 7.8, 8.0, 9.3, 8.6, 8.3)
plot(x, y, xlim=c(9.5, 14), ylim=c(6.5, 9.5), pch=19, cex=0.7)

	
# figure 8.5
x <- runif(40, 0, 7)
y <- x*10 + rnorm(40, mean=20, sd=20)
y[y>100] <- 100
y[y<0] <- 0
plot(x,y)


# figure 9.2 (a)
x <- c(19, 23, 27, 35, 44, 51, 59, 66)
y <- c(124, 117, 120, 132, 128, 142, 143, 135)
plot(x, y, xlab="age", ylab="blood pressure", xlim=c(18, 70), ylim=c(110, 150))
abline(lm(y ~ x))


# figure 9.2 (b)
x <- c(19, 23, 27, 35, 44, 51, 59, 66)
y <- c(111, 129, 113, 137, 147, 126, 135, 139)
plot(x, y, xlab="age", ylab="blood pressure", xlim=c(18, 70), ylim=c(110, 150))
abline(lm(y ~ x))


# figure 9.2 (c)
x <- c(19, 23, 27, 35, 44, 51, 59, 66)
y <- c(111, 129, 113, 137, 147, 126, 135, 139)
plot(c(x, x),c(y+0.5, y-0.5),type="p",xlab="age",ylab="blood pressure",xlim=c(18, 70),ylim=c(110, 150))
abline(lm(c(y, y) ~ c(x, x)))


# figure 9.3 (a)
x <- c(19, 23, 27, 35, 44, 51, 59, 66)
y <- c(125, 128, 127, 124, 129, 128, 126, 129)
plot(x, y, xlab="age", ylab="blood pressure", pch=19, xlim=c(15, 70), ylim=c(110, 150), cex=0.5)
abline(lm(y ~ x))


# figure 9.3 (b)
x <- c(19, 23, 27, 35, 44, 51, 59, 66)
y <- c(119, 134, 112, 138, 122, 128, 148, 129)
plot(x, y, xlab="age", ylab="blood pressure", pch=19, xlim=c(15, 70), ylim=c(110, 150), cex=0.5)
abline(lm(y ~ x))


# figure 9.4
x <- c(1, 2, 4, 5, 3, 2, 3, 1, 5, 4, 4, 2)
y <- c(3, 4, 4, 5, 5, 3, 4, 3, 6, 6, 5, 5)
plot(x, y, xlab="X (year)", ylab="Y (year)", pch=19, xlim=c(0, 7), ylim=c(0, 7))
abline(lm(y ~ x))


# figure 9.5
x <- c(3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17)
y <- c(1.4, 1.5, 2.2, 2.4, 3.1, 3.2, 3.2, 3.9, 4.1, 4.7, 4.5, 5.2, 5.0)
plot(x, y, xlab="age", ylab="wing length (cm)", pch=19, xlim=c(0, 18), ylim=c(0, 6))
abline(lm(y ~ x))


# figure 9.6 (a)
x <- c(3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17)
y <- c(1.4, 1.5, 2.2, 2.4, 3.1, 3.2, 3.2, 3.9, 4.1, 4.7, 4.5, 5.2, 5.0)
result <- lm(y ~ x)
result.plot <- predict(result, interval="confidence", level=0.95)
plot(x, y, xlab="day", ylab="wing length (cm)", ylim=c(0, 6), pch=c(19), cex=0.5)
par(new=T)
matplot(x, result.plot, xlab="", ylab="", ylim=c(0, 6), type="l", axes=F)


# figure 9.6 (b)
x <- c(3, 4, 5, 6, 8, 9, 10, 11, 12, 14, 15, 16, 17)
y <- c(1.4, 1.5, 2.2, 2.4, 3.1, 3.2, 3.2, 3.9, 4.1, 4.7, 4.5, 5.2, 5.0)
result <- lm(y ~ x)
result.plot <- predict(result, interval="prediction", level=0.95)
plot(x, y, xlab="day", ylab="wing length (cm)", ylim=c(0, 6), pch=c(19), cex=0.5)
par(new=T)
matplot(x, result.plot, xlab="", ylab="", ylim=c(0, 6), type="l", axes=F)


# figure 10.1 (a)
x <- seq(-3,3, 0.1)
b1 <- 2
b0 <- 0
q1 <- 1/(1+exp(-(b0 + b1*x)))
plot(x, q1, xlim=c(-3, 3), ylim=c(0, 1.0), xlab="x", ylab="q", type="l", lwd=2)
b0 <- 2
q2 <- 1/(1+exp(-(b0 + b1*x)))
lines(x, q2, lwd=2, col="grey")
b0 <- -3
q3 <- 1/(1+exp(-(b0 + b1*x)))
lines(x, q3, lwd=2, col="grey")


# figure 10.1 (b)
x <- seq(-3,3, 0.1)
b1 <- 2
b0 <- 0
q1 <- 1/(1+exp(-(b0 + b1*x)))
plot(x, q1, xlim=c(-3, 3), ylim=c(0, 1.0), xlab="x", ylab="q", type="l", lwd=2)
b1 <- 1
q2 <- 1/(1+exp(-(b0 + b1*x)))
lines(x, q2, lwd=2, col="grey")
b1 <- 4
q3 <- 1/(1+exp(-(b0 + b1*x)))
lines(x, q3, lwd=2, col="grey")
b1 <- -1.5
q4 <- 1/(1+exp(-(b0 + b1*x)))
lines(x, q4, lwd=2, col="grey")


# figure 10.2 (a)
x <- seq(0, 15, 1)
prob <- dpois(x, lambda=5)
plot(x, prob, type="o", xlim=c(0, 15), ylim=c(0, 0.18), xlab="x", ylab="prob.")


# figure 10.2(b)
x <- seq(0, 15, 1)
freq1 <- dpois(x, lambda=1)
plot(x, freq1, xlim=c(0, 15), ylim=c(0, 0.4), type="o", col="grey", xlab="x", ylab="prob.", cex=0.5)
freq2 <- dpois(x, lambda=3)
lines(x, freq2, type="o", col="grey", cex=0.5)
freq3 <- dpois(x, lambda=5)
lines(x, freq3, type="o", cex=0.5)
freq4 <- dpois(x, lambda=7)
lines(x, freq4, type="o", col="grey", cex=0.5)


# figure 10.3
x <- seq(0, 2, 0.01)
y <- exp(3*x-3)
dx <- seq(0, 2, 0.1)
dy <- rpois(length(dx), lambda=exp(3*dx-3))
plot(x, y, type="l", xlim=c(0, 2), ylim=c(0, 30), tcl=-0.2)
points(dx, dy, cex=0.7)


# figure 10.4
d <- read.csv("table10-2.csv")
result <- glm(cbind(d$dead, d$live) ~ d$dose, family=binomial(logit))
plot(d$dead ~ d$dose, xlab="dose", ylab="dead", xlim=c(1, 6), ylim=c(0, 1), cex=0.7)
pred.dose <- seq(0, 7.0, 0.01)
pred.dead <- 1/(1+exp(-(result$coefficient[1] + result$coefficient[2]*pred.dose)))
lines(pred.dose, pred.dead, lwd=2, col="grey")


# figure 10.5
d <- read.csv("table10-3.csv")
result <-glm(d$flw ~ d$wt, family=poisson)
plot(d$wt, d$flw, xlab="weight", ylab="number of flowers", xlim=c(15, 40), ylim=c(0, 10), cex=0.7)
x.wt <- seq(12, 45, 0.1)
y.flw <- exp(result$coefficient[1]+result$coefficient[2]*x.wt) 
lines(x.wt, y.flw, col="grey", lwd=2) 


# figure 10.6 (a)
l <- 3
d <- rpois(50, lambda=l)
hist(d, ylim=c(0, 14), breaks=c(-0.5:8.5), xlab="x", ylab="freq.", main="")
par(new=T)
x <- seq(0, 8, 1)
y <- 50*dpois(x, lambda=2) 
plot(x, y, xlab="", ylab="", xlim=c(-0.5, 8.5), ylim=c(0, 14), type="o", col="grey", cex=0.5)


# figure 10.6 (b)
hist(d, ylim=c(0, 14), breaks=c(-0.5:8.5), xlab="x", ylab="freq.", main="")
par(new=T)
x <- seq(0, 8, 1)
y <- 50*dpois(x, lambda=3) 
plot(x, y, xlab="", ylab="", xlim=c(-0.5, 8.5), ylim=c(0, 14), type="o", col="grey", cex=0.5)


# figure 10.6 (c)
hist(d, ylim=c(0, 14), breaks=c(-0.5:8.5), xlab="x", ylab="freq.", main="")
par(new=T)
x <- seq(0, 8, 1)
y <- 50*dpois(x, lambda=3.5) 
plot(x, y, xlab="", ylab="", xlim=c(-0.5, 8.5), ylim=c(0, 14), type="o", col="grey", cex=0.5)


# figure 10.6 (d)
hist(d, ylim=c(0, 14), breaks=c(-0.5:8.5), xlab="x", ylab="freq.", main="")
par(new=T)
x <- seq(0, 8, 1)
y <- 50*dpois(x, lambda=4) 
plot(x, y, xlab="", ylab="", xlim=c(-0.5,8.5), ylim=c(0, 14), type="o", col="grey", cex=0.5)


# figure 10.7 
logL <- function(m) sum(dpois(d, m, log=T))
lambda <- seq(2, 5, 0.1)
plot(lambda, sapply(lambda, logL), type="l", xlim=c(2, 5), xlab="lambda", ylab="logL")


# figure 10.9 (a)
d <- read.csv("table10-5.csv")
yv <- jitter(d$y, 0.3)
result <- glm(cbind(y, 1-y) ~ dose, family=binomial(logit), data=d)
plot(d$dose, yv, xlab="dose", ylab="dead", xlim=c(1, 4), ylim=c(-0.05, 1.05), pch=c(1, 2)[d$sex], cex=0.7)
pred.dose <- seq(0, 5, 0.01)
pred.ym <- 1/(1+exp(-(result$coefficient[1] + result$coefficient[2]*pred.dose)))
lines(pred.dose, pred.ym, lwd=2, col="black")


# figure 10.9 (b)
d <- read.csv("table10-5.csv")
yv <- jitter(d$y, 0.3)
result <- glm(cbind(y, 1-y) ~ dose + sex, family=binomial(logit), data=d)
plot(d$dose, yv, xlab="dose", ylab="dead", xlim=c(1, 4), ylim=c(-0.05, 1.05), pch=c(1, 2)[d$sex], cex=0.7)
pred.dose <- seq(0, 5, 0.01)
pred.ym <- 1/(1+exp(-(result$coefficient[1] + result$coefficient[2]*pred.dose + result$coefficient[3]*1)))
pred.yf <- 1/(1+exp(-(result$coefficient[1] + result$coefficient[2]*pred.dose + result$coefficient[3]*2)))
lines(pred.dose, pred.ym, lwd=2, col="grey")
lines(pred.dose, pred.yf, lwd=2, col="black")


# figure 10.9 (c)
d <- read.csv("table10-5.csv")
yv <- jitter(d$y,0.3)
result <- glm(cbind(y, 1-y) ~ dose + sex + dose:sex, family=binomial(logit), data=d)
plot(d$dose, yv, xlab="dose", ylab="dead", xlim=c(1, 4), ylim=c(-0.05, 1.05), pch=c(1, 2)[d$sex], cex=0.7)
pred.dose <- seq(0, 5, 0.01)
pred.ym <- 1/(1+exp(-(result$coefficient[1] + result$coefficient[2]*pred.dose + result$coefficient[3]*1 + result$coefficient[4]*1*pred.dose)))
pred.yf <- 1/(1+exp(-(result$coefficient[1] + result$coefficient[2]*pred.dose + result$coefficient[3]*2 + result$coefficient[4]*2*pred.dose)))
lines(pred.dose, pred.ym, lwd=2, col="grey")
lines(pred.dose, pred.yf, lwd=2, col="black")


# figure 10.10
d <- read.csv("table10-6.csv")
result <- glm(plants ~ light, offset=log(w_area), family=poisson, data=d)
plot(d$plants/d$w_area ~ d$light, xlab="light", ylab="density", xlim=c(0, 4000), ylim=c(0, 0.65), cex=0.7)
pred.light <- seq(-200, 4200, 0.1)
pred.y <- exp(result$coefficient[1] + result$coefficient[2]*pred.light)
lines(pred.light, pred.y, lwd=2, col="grey")


# figure 11.1 (a)
d <- read.csv("table11-1.csv")
plot(d$x, d$y, xlab="x", ylab="y")


# figure 11.1 (b)
d <- read.csv("table11-1.csv")
plot(d$x, d$y, xlab="x", ylab="y", pch=as.character(d$block))


# figure 11.2
library(lme4)
library(glmmML)
d <- read.csv("table11-2.csv")
res.1 <-glmer(y ~ x + (1|ID), family=binomial(logit), data=d)
res.1 <- summary(res.1)
res.2 <-glmmML(y ~ x , cluster=ID, data=d)
plot(d$x, d$y, xlab="x", ylab="y", xlim=c(0, 5), tcl=-0.2)
pred.x <- seq(0, 5, 0.01)
pred.y1 <- 1/(1+exp(-( res.1$coefficients[1] + res.1$coefficients[2] * pred.x)))
pred.y2 <- 1/(1+exp(-( res.2$coefficients[1] + res.2$coefficients[2] * pred.x)))
lines(pred.x, pred.y1, lwd=2, col="black")
lines(pred.x, pred.y2, lwd=2, col="grey", lty=3)


# figure 11.3
library(lme4)
library(glmmML)
d <- read.csv("table11-3.csv")
mother <- as.factor(d$mother)
wt <- d$wt
y <- d$y
res.1 <-glmer(y ~ wt + (1|mother), family=binomial(logit))
res.1 <- summary(res.1)
res.2 <-glmmML(y ~ wt , cluster=mother)
plot(d$wt, d$y, xlab="weight", ylab="sex ratio", xlim=c(0, 0.6))
pred.wt <- seq(0, 0.6, 0.01)
pred.y1 <- 1/(1+exp(-(res.1$coefficient[1] + res.1$coefficient[2]*pred.wt)))
pred.y2 <- 1/(1+exp(-(res.2$coefficient[1] + res.2$coefficient[2]*pred.wt)))
lines(pred.wt, pred.y1, lwd=2, col="black")
lines(pred.wt, pred.y2, lwd=2, col="grey", lty=3)


# figure 11.4 (a)
d <- read.csv("table11-4.csv")
result <-glm(flw ~ wt, family=poisson, data=d)
plot(d$wt, d$flw, xlab="weight", ylab="number of flowers", xlim=c(20,60), ylim=c(0,16))
x.wt <- seq(20, 60, 0.1)
y.flw <- exp(result$coefficient[1] + result$coefficient[2]*x.wt) 
lines(x.wt, y.flw)


# figure 11.4 (b)
library(MASS)
d <- read.csv("table11-4.csv")
result <- glm(flw ~ wt, family=negative.binomial(1), data=d)
plot(d$wt, d$flw, xlab="weight", ylab="number of flowers", xlim=c(20,60), ylim=c(0,16))
x.wt <- seq(20, 60, 0.1)
y.flw <- exp(result$coefficient[1]+result$coefficient[2]*x.wt) 
lines(x.wt, y.flw)


# figure 11.4 (c)
library(MASS)
d <- read.csv("table11-4.csv")
result <- glm.nb(flw ~ wt, data=d)
plot(d$wt, d$flw, xlab="weight", ylab="number of flowers", xlim=c(20,60), ylim=c(0,16))
x.wt <- seq(20, 60, 0.1)
y.flw <- exp(result$coefficient[1]+result$coefficient[2]*x.wt) 
lines(x.wt, y.flw)


# figure 11.5
d <- read.csv("table11-5.csv")
res.1 <- glm(d$y ~ d$x, family=quasipoisson)
res.2 <- glm(d$y ~ d$x, family=negative.binomial(1))
res.3 <- glm.nb(d$y ~ d$x)
plot(d$y ~ d$x, xlim=c(1, 7), ylim=c(0, 50), xlab="x", ylab="y")
pred.x <- seq(0, 7, 0.01)
pred.y1 <- exp((res.1$coefficient[1] + res.1$coefficient[2]*pred.x))
pred.y2 <- exp((res.2$coefficient[1] + res.2$coefficient[2]*pred.x))
pred.y3 <- exp((res.3$coefficient[1] + res.3$coefficient[2]*pred.x))
lines(pred.x, pred.y1)
lines(pred.x, pred.y2, col="grey")
lines(pred.x, pred.y3, col="grey", lty=2)


# figure 11.6 (a)
negative_binom_values <- rnbinom(10000, size=5, prob=0.4)
data <- negative_binom_values[negative_binom_values<23]
hist(data, breaks=seq(-0.5, 23.5, 1), col="grey", xlim=c(0, 23), xlab="x", ylab="freq.", main="")


# figure 11.6 (b)
gamma_values <- rgamma(10000, shape=5, rate=0.4/(1-0.4))
poisson_values <- sapply(gamma_values, function(lambda) rpois(1, lambda))
data <- poisson_values[poisson_values<23]
hist(data, breaks=seq(-0.5, 23.5, 1), col="grey", xlim=c(0, 23), xlab="x", ylab="freq.", main="")


# figure 11.6 (c)
x <- seq(0, 22, 0.01)
plot(x, dgamma(x, shape=5, rate=0.4/(1-0.4)), type="l", xlab="x", ylab="prob. density", xlim=c(0, 22), ylim=c(0, 0.13))


# figure 12.1
x <- seq(0, 15, 0.01)
chisq <- dchisq(x, 3)
plot(x, chisq, xlab="Chi-square", ylab="prob. density", xlim=c(0,15), ylim=c(0,0.3), type="l")


# figure 12.2
egg <- c(rep(0, 0), rep(1, 0), rep(2, 60), rep(3, 101), rep(4, 84), rep(5, 48), rep(6, 7))
r <- hist(egg, breaks=seq(-0.5, 9.5, 1), main="", xlab="number of eggs per bean", ylab="freq.", col= "grey")
x <- c(0:9)
pois <- length(egg)*dpois(x, mean(egg))
lines(r$mids, pois)


# figure 13.1
a <- c(2, 4, 1, 7, 3, 5, 6)
b <- c(3, 5, 2, 6, 1, 4, 7)
plot(a, b, xlab="a's rank", ylab="b's rank", xlim=c(1, 7), ylim=c(1, 7))


# figure 13.2
a <- c(57, 45, 72, 78, 53, 63, 86, 98, 59, 71)
b <- c(83, 37, 41, 84, 56, 85, 77, 87, 70, 59)
plot(a, b, xlab="", ylab="", xlim=c(30, 100), ylim=c(30, 100))


# figure 14.1
n <- 1000
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)
r <- sqrt(x*x + y*y)
length(which(r<1))*4/n
plot(x, y, pch=20, cex=0.2, xlim=c(-1, 1), ylim=c(-1, 1), xlab="x", ylab="y")


# figure 14.2 (a)
x <- seq(-5, 5, 0.1)
y <- seq(-5, 5, 0.1)
f <- function(x, y){ 
	r <- 0.7
	v <- 1/(2*pi*sqrt(1-r^2))*exp(-1/(2*(1-r^2))*(x^2+y^2-2*r*x*y))
	return (v)}
z <- outer(x, y, f)
persp(x, y, z, theta=0, phi=45, expand=0.7, col="gray", xlab="x", ylab="y", zlab="", ticktype="detailed")


# figure 14.2 (b)
N <- 10000
x1 <- 5
x2 <- 5
rho <- 0.7
for (i in 2:N){ 
	{if(i%%2==0){
  x1[i] <- rnorm(1, rho*x2[i-1], sqrt(1-rho^2))
  x2[i] <- x2[i-1]
  }
  else{
  x1[i] <- x1[i-1]
  x2[i] <- rnorm(1, rho*x1[i-1], sqrt(1-rho^2))	
  	}
  	}
}
plot(x1[1:500], x2[1:500], xlim=c(-5, 5), ylim=c(-5, 5), xlab="x", ylab="y", type='o', cex=0.4, lwd=0.5)


# figure 14.3-14.5 Rstan を使うため省略


# 付録 figure A.1 (a)
x <- seq(0, 15, 0.01)
chisq <- dchisq(x, 5)
plot(x, chisq, xlab="chi-square", ylab="prob. density", xlim=c(0, 15), ylim=c(0, 0.2), type="l")


# 付録 figure A.1 (b)
x <- seq(0, 15, 0.01)
Fd <- df(x, 4, 8)
plot(x, Fd, xlab="F value", ylab="prob. density", xlim=c(0, 5), ylim=c(0, 0.8), type="l")


# 付録 figure A.2 (a)
x <- seq(0, 30, 0.01)
chisq1 <- dchisq(x, 1)
chisq2 <- dchisq(x, 4)
chisq3 <- dchisq(x, 8)
chisq4 <- dchisq(x, 16)
plot(x, chisq1, xlab="chi-square", ylab="prob. density", xlim=c(0, 30), ylim=c(0, 0.4), type="l")
lines(x, chisq2, lty=3)
lines(x, chisq3, col="grey")
lines(x, chisq4, lty=3, col="grey")


# 付録 figure A.2 (b)
x <- seq(0, 30, 0.01)
F1 <- df(x, 1, 1)
F2 <- df(x, 10, 10)
F3 <- df(x, 30, 30)
F4 <- df(x, 100, 100)
plot(x, F1, xlab="F value", ylab="prob. density", xlim=c(0, 4), ylim=c(0, 2.0), type="l")
lines(x, F2, lty=3)
lines(x, F3, col="grey")
lines(x, F4,lty=3, col="grey")


# 演習問題 8.1
x <- c(12.5, 13.1, 18.9, 9.7, 16.4, 8.3, 13.7, 17.5, 11.4, 16.2, 19.3, 15.3)
y <- c(10.5, 8.9, 13.6, 6.3, 12.5, 10.3, 10.8, 16.7, 8.3, 9.5, 12.4, 10.1)
plot(x, y)


# 演習問題  9.1
d <- read.csv("enshu9-1.csv")
x <- d$x
y <- d$y
result <- lm(y ~ x)
y.plot <- predict(result, newdata=data.frame(x=seq(70, 115, 0.1)), interval="confidence")
plot(x, y, xlab="days", ylab="number", xlim=c(70, 115),ylim=c(0, 30),tcl=-0.2, yaxt="n")
par(new=T)
matplot(seq(70, 115, 0.1), y.plot, xlab="", ylab="", ylim=c(0, 30), type="l", xlim=c(70, 115), tcl=-0.2, xaxt="n")


# 演習問題 10.1
d <- read.csv("enshu10-1.csv")
result <- glm(mihari ~ wt, offset=log(minutes), family=poisson, data=d)
plot(d$mihari/d$minutes ~ d$wt, xlab="weight", ylab="number of guards", xlim=c(80, 120), ylim=c(0, 0.15), cex=0.7)
pred.wt <- seq(0, 140, 0.1)
pred.y <- exp(result$coefficient[1] + result$coefficient[2]*pred.wt)
lines(pred.wt, pred.y, lwd=2, col="grey")


# 演習問題　11.1
library(lme4)
library(glmmML)
d <- read.csv("enshu11-1.csv")
res.1 <- glmer(d$y ~ d$x +(1|d$cond), family=poisson(log))
res.2 <- glmmML(d$y ~ d$x, family=poisson(log), cluster=d$cond)
res.1 <- summary(res.1)
plot(d$x, d$y, xlim=c(0,8), ylim=c(0, 10), xlab="x", ylab="y")
pred.x <- seq(0, 8, 0.01)
pred.y1 <- exp(res.1$coefficient[1] + res.1$coefficient[2]*pred.x)
pred.y2 <- exp(res.2$coefficient[1] + res.2$coefficient[2]*pred.x)
lines(pred.x, pred.y1, lwd=2, col="black")
lines(pred.x, pred.y2, lwd=2, col="grey", lty=3)



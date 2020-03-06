
#install.packages("mgcv")
#install.packages("RColorBrewer")
library(mgcv)
library(RColorBrewer)

################################
################################ spatiotemporal interpolation
################################

############### Read data
setwd("/Users/murakami/Dropbox/MyPapers/H31/LDAT/final/mgcv_example")
dat 	<-read.csv("dat.csv")
mdat	<-read.csv("mdat.csv")

############### Modeling
f     <- y ~ te(px,py, year, 
                  bs = c("tp", "cr"), 
                  k = c(50, 10),
                  d = c(2, 1))
mod   <- gam(f, data = dat)
#mod   <- bam(f, data = dat)# for very large samples

################ spatiotemporal prediction for 2008
mdat$year<-2008
pred0  <- predict(mod, mdat, se.fit = TRUE)
pred   <- data.frame(mdat[,c("px","py")],pred=pred0$fit, se.pred=pred0$se.fit)

nc   <- 11
cols <- rev(brewer.pal(n = nc, name = "RdYlBu"))
cuts<-c(-Inf, quantile(pred0$fit ,probs=seq(0.1,0.9,0.1)), Inf)
coordinates(pred)<-c("px","py")
spplot(pred, "pred",cuts=cuts, col="transparent",cex=2,pch=15)

################################
################################ spatiotemoiral regression analysis
################################

############### Read data
setwd("/Users/murakami/Dropbox/MyPapers/H31/LDAT/final/mgcv_example")
dat 	<-read.csv("dat.csv")

############### Modeling
f     <- y ~ te(px,py, year, k=c(50, 10), d=c(2, 1)) + s(tachid) + s(ekid) + s(city,bs="re")
mod   <- gam(f, data = dat)

################ Estimation result
summary(mod)
plot(mod, select=2)
plot(mod, select=3,ylim=c(-1,1))
plot(mod, select=4)








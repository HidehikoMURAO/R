
# install.packages("spTimer")
# install.packages("RColorBrewer")
library(spTimer)
library(RColorBrewer)

########################### Read data
setwd("/Users/murakami/Dropbox/MyPapers/H31/LDAT/final/spTimer_example")
dat     <- read.csv("dat.csv")
coords  <- dat[dat[,"year"]==dat[1,"year"],c("px","py")]

test    <-mean(dist(coords))
mdat    <- read.csv("mdat.csv")
mcoords <- mdat[mdat[,"year"]==mdat[1,"year"],c("px","py")]

########################### Modeling
sp.decay<-spT.decay(distribution=Gamm(2,1), tuning=0.08)# prior distribution for (1/range)
mod     <- spT.Gibbs(formula = y ~ tachid +ekid, coords=coords, data=dat, model="AR",nItr=5000, nBurn=1000,
                     spatial.decay=sp.decay, tol.dist=0)# It is more typical to set like "nIter=10000, nBurn =2000"
summary(mod)

########################### Spatiotemporal prediction
pred0    <-predict(mod, newdata=mdat, newcoords = mcoords, tol.dist=0)

pred_mean<-pred0$Mean
pred_sd  <-pred0$SD
pred   <- data.frame(mdat[,c("year","px","py")],pred_mean=c(pred_mean), pred_sd=c(pred_sd))


########################### Plot predicted values in 2008
year  <-2008
pred_sub <-pred[pred$year==year,]
coordinates(pred_sub)<-c("px","py")

nc   <- 11
cols <- rev(brewer.pal(n = nc, name = "RdYlBu"))
cuts<-c(-Inf, quantile( pred$pred_mean ,probs=seq(0.1,0.9,0.1)), Inf)
spplot(pred_sub, "pred_mean",cuts=cuts, col="transparent",cex=2,pch=15)








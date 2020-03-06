
#install.packages("gstat")
#install.packages("spacetime")
#install.packages("sp")

library(gstat)
library(spacetime)
library(sp)

#################### Set working directly
setwd("/Users/murakami/Dropbox/MyPapers/H31/LDAT/final/gstat_example")

#################### Data at observation sites
dat     <- read.csv("dat.csv")
loc     <- dat[dat[,"year"]==dat[1,"year"],c("px","py")]
loc	    <- SpatialPoints(loc)
time	  <- as.Date(ISOdate(unique(dat$year), 1,1))
dat2	  <- STFDF(sp=loc, time=time, data=dat[,c("y", "tachid", "ekid")])
dat2    <- as(dat2,"STSDF")

#################### Data at missing sites
mdat     <- read.csv("mdat.csv")
loc     <- mdat[mdat[,"year"]==mdat[1,"year"],c("px","py")]
loc	    <- SpatialPoints(loc)
time	  <- as.Date(ISOdate(unique(mdat$year), 1,1))
mdat2	  <- STFDF(sp=loc, time=time, data=mdat[,c("tachid", "ekid")])

vv = variogram(y~tachid+ekid, dat2,tlags=0:5)#
plot(vv)
plot(vv, map = FALSE)

#################### Separable covariance function
separableModel 	<- vgmST("separable", 
                         space=vgm(0.08,"Exp", 0.05, 0.01),#tau^2, range, sigma^2
                         time =vgm(0.08,"Exp", 500 , 0.01),#tau^2, range, sigma^2
                         sill=0.07)
separableVgm 	<- fit.StVariogram(vv, separableModel)
attr(separableVgm, "optim")$value    ## Root mean squared error
plot(vv,separableVgm)

#################### Product-sum covariance function
prodSumModel <- vgmST("productSum",
                      space=vgm(0.08,"Exp", 0.05, 0.01), #tau^2, range, sigma^2
                      time =vgm(0.08,"Exp", 500   ,0.01),#tau^2, range, sigma^2
                      k=0.5)
prodSumVgm 	<- fit.StVariogram(vv, prodSumModel)
attr(prodSumVgm, "optim")$value      ## Root mean squared error
plot(vv,prodSumVgm)
plot(vv,prodSumVgm,map=FALSE)

#################### Spatiotemporal prediction
pred  <- krigeST(y~tachid+ekid, data = dat2,
                 newdata = mdat2, prodSumVgm, computeVar=TRUE)

stplot(pred)        ## Predicted values

pred$se <- sqrt(pred$var1.var)
stplot(pred[,,"se"])## Standard error of the predicted values


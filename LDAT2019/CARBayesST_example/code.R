
# install.packages("CARBayesST")
# install.packages("CARBayesdata")
# install.packages("sp")
# install.packages("rgdal")
# install.packages("RColorBrewer")
library(CARBayesST)
library(spdep)
library(spacetime)
library(rgdal)
library(RColorBrewer)

########################### Read data
setwd("/Users/murakami/Dropbox/MyPapers/H31/LDAT/final/CARBayesST_example")
dat0   <-read.csv("dat.csv")
zones  <-readOGR("zones.shp")

dat0$id<-1:length(dat0[,1])
dat <- merge(x=zones, y=dat0, by="IG", all.x=FALSE, duplicateGeoms = TRUE)
dat <- dat[order(dat$id),]

########################### Spatial proximity matrix
W.nb   <- poly2nb(zones)
W.list <- nb2listw(W.nb, style = "B")
W      <- nb2mat(W.nb, style = "B")

########################### Poisson regression
formula<- observed ~ offset(log(expected)) + jsa + price + pm10
model1 <- glm(formula = formula, family = "poisson", data = dat)
summary(model1)

########################### Spatiotemporal Poisson CAR
model2 <- ST.CARar(formula = formula, family = "poisson",
                   data = dat, W = W, burnin = 2000, n.sample = 22000)
model2

dat$resGLM  <-model1$fitted.values
dat$resCAR  <-model2$fitted.values
dat$resCAR_se<-apply(model2$samples$fitted,2,sd)

########################### Disease mapping result in 2008
year        <-2008#2007:2011
dat_sub     <-dat[dat$year==year,]
nc	<- 11
summary(model2$fitted.values)
cuts	<- seq(20,200,len=nc+1)
cols 	<- rev(brewer.pal(n = nc, name = "RdYlBu"))

spplot(dat_sub, "observed",col.regions = cols,lwd=0.01,
       colorkey=list(at= cuts),at=cuts, col="transparent")### Observations

spplot(dat_sub, "resGLM",col.regions = cols,lwd=0.01,
       colorkey=list(at= cuts),at=cuts, col="transparent")### Poisson regression

spplot(dat_sub, "resCAR",col.regions = cols,lwd=0.01,
       colorkey=list(at= cuts),at=cuts, col="transparent")### Spatiotemporal Poisson CAR






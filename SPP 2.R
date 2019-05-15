### SPP 2
rm(list=ls())
library(spatstat)
data(bei)
X<-bei
plot(X) ##evidentemente no es uniforme

## fitting
ppm(X~1)
plot(bei.extra) ##elevation and slope

fit<-ppm(X~grad,data=bei.extra)
fit ## exp(I + AG)

plot(effectfun(fit,"grad",se.fit = T)) ##intensity vs slope with first model

## recordar que de la forma anterior establecemos una relación log-lineal, lo cual puede 
## no ser cierto

fit2<-ppm(bei ~ grad + I(grad^2), data=bei.extra) ## I : identity for specifying correctly
fit2
plot(effectfun(fit2,"grad",se.fit = T))

#################################

data("murchison") ## yacimientos de oro y fallas geologicas
mur <- solapply(murchison, rescale, s=1000, unitname="km")

plot(as.layered(mur))
dfault<-with(mur,distfun(faults)) ##distance to the nearest fault

fit<-ppm(gold~dfault,data = mur)
fit
plot(effectfun(fit,"dfault",se.fit = T))

### logical covariate

fitl<-ppm(gold~greenstone,data = mur)
fitl

## cambiamos el modelo para quitar el intercepto y así queda una indicadora sobre si está o no 
## en la sección que queremos

fitl2<-ppm(gold~greenstone-1,data = mur)
fitl2


## Categorical covariates

data("gorillas")
gor<-rescale(gorillas,1000,unitname = "km")
gex<-lapply(gorillas.extra, rescale, s = 1000, unitname = "km")
plot(gorillas.extra)

levels(gex$vegetation)
## Disturbed, colonising, grassland, primary, secondary, transition
names(gex)
shorten<-function(x)substr(x,1,4)
names(gex)<-shorten(names(gex))
names(gex)
isfactor<-!sapply(lapply(gex,levels),is.null)
for(i in which(isfactor))
  levels(gex[[i]])<-shorten(levels(gex[[i]]))
levels(gex$vege)

## no hace el ajuste por que dice que no está implementado ppm para marked pp
fit<-ppm(gor~vege, data = gex)

vt <- tess(image=gex$vege)
intensity(quadratcount(gor, tess=vt))
plot(vt)S

#### Additive model

data(bei)


fitadd<-ppm(bei~elev+grad, data = bei.extra)
fitadd


### additive gold

fitaddg<-ppm(gold~dfault+greenstone, data = mur)
fitaddg

###############
data("japanesepines")
jpines<-residualspaper[["Fig1"]]

ppm(jpines~x+y)

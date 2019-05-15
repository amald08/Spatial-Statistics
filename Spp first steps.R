####Modelling SPP
library(spatstat)
data("swedishpines")
P<-swedishpines

plot(P)
Q<-quadratcount(P,3,3)
Q
plot(Q,add = T)
##test para checar uniformidad

QT<-quadrat.test(P,nx=3,ny=3)
QT
plot(P)
plot(QT,add = T)

##Kernel smoothing

den<-density(P,sigma = 15)
plot(den) ##intensity map
plot(P,add = T)

##ppm fits a log-intensity model
##################
## Examples

ppm(P~1) #Uniform
ppm(P~x)
ppm(P~x+y)

ppm(P~polynom(x,y,3))##polinomio de grado 3 en la intensidad
ppm(P~I(y>18))## different constants above and velow y = 18

#######################

fit<-ppm(P~x+y)
lam<-predict(fit) ##fitted values of intensity function l(x)
plot(lam)

######################
## Probamos dos modelos y los compatamos
fit0<-ppm(P~1)
fit1<-ppm(P~polynom(x,y,2))
anova(fit0,fit1,test = "Chi")
## como p-value>.05  la parte cuadrática no es significativa

diagnose.ppm(fit0,which = "smooth")


#############################
## Spatial covariates

data(copper) ###  
P <- copper$SouthPoints ## yacimientos de cobre
Y <- copper$SouthLines ## fallas geológicas
plot(P)
plot(Y, add=TRUE)

Z<-distmap(Y) # Distance to the nearest line
plot(Z)

## Z es una covariable y queremos ver si influye en la intensidad 
#Plot C(z) against z, where C(z) = fraction of data points xi for which Z(xi) ??? z.
#Also plot C0(z) against z, where C0(z) = fraction of area of study region where Z(u) ??? z.

lurking(ppm(P), Z) ##spp residuals vs covariate

#No hay función kstest(P, Z) ## formal test

fit<-ppm(P~Z)
plot(fit)

fit5 <- ppm(P ~polynom(Z,5))
plot(predict(fit5))

plot(effectfun(fit5)) ##plots fitted intensity vs Z

#test
fit0 <- ppm(P ~1)
fit1 <- ppm(P ~polynom(Z,5))
anova(fit0, fit1, test="Chi")
## como p-value >.05 no es significativo el polinomio
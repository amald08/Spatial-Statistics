rm(list=ls())
library(spatstat)
data(nztrees)
plot(nztrees)
ppm(nztrees)
# fit the stationary Poisson process
# to point pattern 'nztrees'

Q <- quadscheme(nztrees) 
ppm(Q) 
# equivalent.

ppm(nztrees, ~ x)
# fit the nonstationary Poisson process 
# with intensity function lambda(x,y) = exp(a + bx)
# where x,y are the Cartesian coordinates
# and a,b are parameters to be estimated

ppm(nztrees, ~ polynom(x,2))
# fit the nonstationary Poisson process 
# with intensity function lambda(x,y) = exp(a + bx + cx^2)

library(splines)
ppm(nztrees, ~ bs(x,df=3))
#       WARNING: do not use predict.ppm() on this result
# Fits the nonstationary Poisson process 
# with intensity function lambda(x,y) = exp(B(x))
# where B is a B-spline with df = 3

ppm(nztrees, ~1, Strauss(r=10), rbord=10)
# Fit the stationary Strauss process with interaction range r=10
# using the border method with margin rbord=10

ppm(nztrees, ~ x, Strauss(13), correction="periodic")
# Fit the nonstationary Strauss process with interaction range r=13
# and exp(first order potential) =  activity = beta(x,y) = exp(a+bx)
# using the periodic correction.

# Huang-Ogata fit:
## Not run: ppm(nztrees, ~1, Strauss(r=10), rbord=10, method="ho")


# COVARIATES
#
X <- rpoispp(42)
plot(X)
X
weirdfunction <- function(x,y){ 10 * x^2 + runif(length(x))}
Zimage <- as.im(weirdfunction, unit.square())
#
# (a) covariate values in pixel image
plot(Zimage)
ppm(X, ~ y + Z, covariates=list(Z=Zimage))
#
# (b) covariate values in data frame
Q <- quadscheme(X)
xQ <- x.quad(Q)
yQ <- y.quad(Q)
Zvalues <- weirdfunction(xQ,yQ)
ppm(Q, ~ y + Z, covariates=data.frame(Z=Zvalues))
# Note Q not X

################################################
################################################
########### PREDICT ############################
data(cells)
plot(cells)
m <- ppm(cells ~ polynom(x,y,2), Strauss(0.05))
trend <- predict(m, type="trend")
## Not run: 
image(trend)
points(cells)
summary(trend)
## End(Not run)
cif <- predict(m, type="cif")
## Not run: 
persp(cif)
plot(cif)
## End(Not run)
mj <- ppm(japanesepines ~ harmonic(x,y,2))
se <- predict(mj, se=TRUE) # image of standard error
ci <- predict(mj, interval="c") # two images, confidence interval
plot(japanesepines)
# prediction interval for total number of points
predict(mj, type="count", interval="p")

# prediction intervals for counts in tiles
predict(mj, window=quadrats(japanesepines, 3), type="count", interval="p")

# prediction at arbitrary locations
predict(mj, locations=data.frame(x=0.3, y=0.4))

X <- runifpoint(5, Window(japanesepines))
predict(mj, locations=X, se=TRUE)

# multitype
rr <- matrix(0.06, 2, 2)
ma <- ppm(amacrine ~ marks,  MultiStrauss(rr))
Z <- predict(ma)
Z <- predict(ma, type="cif")
predict(ma, locations=data.frame(x=0.8, y=0.5,marks="on"), type="cif")

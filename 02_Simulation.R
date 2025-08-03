##################################### Sequential Gaussian simulation ##########################################################

library(compositions) # to deal with compositional data
library(gstat) # for geostatistical analysis of multivariate data
library(sp) # for spatial analysis
library(gmGeostats) # for geostats with compositional data

# Other packages to help with specific tasks

library(magrittr)
library(dplyr)
library(MVN)
library(FNN)
library(DescTools)
library(manipulate)


########################################### Simulations ################################################

# Can do simulations with predict() function, similar to kriging, by specifying argument "nsim".

wind.alr.gg = make.gmMultivariateGaussianSpatialModel(data=data.frame(alr(wind.acomp)), coords=wind.coords,
                                                      nmax=20, nmin=4, maxdist=20) %>% as.gstat

wind.alr.vgAnis = variogram(wind.alr.gg, cutoff=50, width=3.5, alpha=c(90,180), tol.hor=45)

wind.alr.mdAnis = vgm(psill=1, model="Exp", range=15, nugget=1, anis=c(90,0.7))

wind.alr.gg = fit_lmc(v=wind.alr.vgAnis, g=wind.alr.gg, model=wind.alr.mdAnis, fit.lmc=TRUE, 
                      correct.diagonal=1.001)

variogramModelPlot(wind.alr.vgAnis, wind.alr.gg$model)

wind.grid.fine.masked = setMask(wind.grid.fine, mask=wind.mask.fine)

wind.alr.osim = predict(wind.alr.gg, newdata=wind.grid.fine.masked, nsim= 10, debug.level=-1)

wind.alr.osim

# Gives ordinary Sequential Gaussian Simulation

# Simplest way of doing it. Can also do universal sequential Gaussian simulation.
# Complete code as an exercise and run.


##################################### Inspection of Results #######################################

# To easily examine results need to first recast into more easily accessible format.

wind.alr.osim.stack = wind.alr.osim@data %>%
  DataFrameStack(dimnames=list(
    loc=1:sum(wind.mask.fine), sim=1:10,
    var=c("alr1","alr2","alr3","alr4","alr5","alr6","alr7")),
  stackDim="sim")

wind.alr.osim.stack

# Provides the data as a stacked data frame
# Then we backtransform the data to concentrations.

wind.compo.osim.aux = gmApply(X=wind.alr.osim.stack, FUN=alrInv, orig=wind.acomp)

# Unmask for further manipulations. 

wind.compo.osim = unmask(wind.compo.osim.aux, mask=wind.mask.fine)

# And plot

out = wind.grid.fine %>% SpatialPointsDataFrame(
  data = getStackElement(wind.compo.osim,1)) %>%
  image_cokriged(ivar="Fe2O3", legendPos = "top")
  
# Note, the number after wind.compo.osim indicates the simulation we are looking at. 

# Or, for more comprehensive plots: 

myfun = function(i,j){
  out = wind.grid.fine %>%SpatialPointsDataFrame(
    data = getStackElement(wind.compo.osim,i)) %>%
    image_cokriged(ivar=j, legendPos = "top")
  points(wind.coords, pch=21, col=1, cex=1,
    bg=out$col[cut(wind.acomp[,j],out$breaks)])
}

manipulate(myfun(i,j),
             i=slider(1, max = 10),
             j=picker("Fe2O3", "Al2O3", "SiO2", "MnO2", "P2O5", "SO3", "LOI", "CL"))


################################### Model validation ######################################

# Want to check how well our model represents reality. 
# To do so, explore how well salient statistical features of the data are reproduced. 

# Comparing compositional means: 

a = clo(gmApply(wind.compo.osim, MARGIN=c("sim","var"), FUN=geometricmean, na.rm=T))
par(mfrow=c(1,1))
boxplot(data.frame(a), log="y", border=2)
points(1:8,mean(wind.acomp), lty=2)

# Q-Q-Plots:

myQQfun = function(x, j="MnO2"){
  erg = qqplot(wind.acomp[,j], x[,j], plot.it = F)
  return(data.frame(erg))
  }
a = myQQfun(getStackElement(wind.compo.osim.aux,1))
summary(a)

auxFe = swarmPlot(wind.compo.osim.aux, PLOTFUN=myQQfun, j="MnO2",
                  .plotargs=list(asp=1, xlab="observed quantiles",
                  ylab="simulation quantiles", main="Q-Q-Plot"))
abline(0,1,col=2)

par(mfrow=c(1,1))

# Mean square deviation: 

meanSqDev = function(x) mean((x$x-x$y)^2)
statHistFe = sapply(auxFe, meanSqDev)
boxplot(statHistFe)

# Variograms

vgFe = gstat(id="P2O5", formula=P2O5~1, locations=~Easting+Northing,
          data = cbind(data.frame(wind.acomp), wind.coords)) %>%
  variogram(cutoff=50)

myVGfun = function(x){
  vg = gstat(id="P2O5", formula=P2O5~1, locations=~Easting+Northing,
               data = cbind(data.frame(x), wind.grid.fine.masked)) %>%
    variogram(cutoff=50)
  return(data.frame(x=c(0,vg$dist),y=c(0,vg$gamma)))
  }

aux = swarmPlot(wind.compo.osim.aux, PLOTFUN= myVGfun,
                  .plotargs=F)
plot(aux, xlim=range(0, vgFe$dist), ylim=range(0, vgFe$gamma),
       xlab="lag distance", ylab="semivariogram")

points(vgFe$dist, vgFe$gamma, col=2)

par(mfrow=c(1,1))

# Check different elements and note which ones are well
# reproduced, and which ones are not!

#
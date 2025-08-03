##################################### Basic interpolation and kriging ########################################################

########################################## Packages & data #######################################################

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

# Data

data("Windarling", package="gmGeostats")

windarling = Windarling

rm(Windarling)

# --> Small dataset for single bench of a Pilbara iron mine (6 m bench height)



################################### Data inspection & handling #######################################

head(windarling, n=3) # show first 3 lines of data table

wind.coords = dplyr::select(windarling, Easting:Northing) # Extract coordinates:
wind.compo = dplyr::select(windarling, Fe:LOI) # Extract compositional data only:

summary(rowSums(wind.compo)) # Do components sum to 100%? 
wind.compo$Fe2O3 = wind.compo$Fe/0.6994 # Introduce new variable: Fe2O3
wind.compo = wind.compo[,-1] # Remove Fe variable
summary(rowSums(wind.compo)) # Do components sum to 100% now? 

# Need to do same to P, S and Mn --> exercise! (Do it for yourself)

wind.compo$MnO2 = wind.compo$Mn*1.58 # Introduce new variable: MnO2
wind.compo$P2O5 = wind.compo$P*2.29 # Introduce new variable: P2O5
wind.compo$SO3 = wind.compo$S*2.50 # Introduce new variable: SO3
wind.compo = wind.compo[,-c(1,4,5)] # Remove Mn, P and S

summary(rowSums(wind.compo)) # Do components sum to 100% now? 
hist(wind.compo$Fe2O3)
hist(rowSums(wind.compo), breaks=40)


#--> What do we do about the values greater than 1, i.e., 100%?

# Check which rows have sums substantially below 95%

length(which(rowSums(wind.compo) < 0.99))


# --> Normalise everything to 1.00 (will be done in compositional data transformations, 
# explained further down)



### Compositional data plots 

wind.compo %>% dplyr::select(SiO2, Al2O3, LOI) %>% plot.acomp(cex = 0.2) # Ternary
wind.compo %>% dplyr::select(SiO2, Al2O3, Fe2O3) %>% plot.acomp(cex = 0.2)

isoPortionLines(by = 0.2, col=8) # Add isolines

# Play around with this a little to get first impression of the data

# Play around with plot() and hist() for a little bit to explore data

hist(wind.compo$Fe2O3, breaks=60)
hist(wind.compo$SiO2, breaks=60)

plot(Al2O3 ~ Fe2O3, data=wind.compo)
plot(Al2O3 ~ Fe2O3, data=wind.compo, log="xy")

# Do these plots look like the data is normally distributed?



### Compositional data transforms

# Let's try something else: 

hist(wind.compo$Al2O3/wind.compo$Fe2O3, breaks=60)
hist(log(wind.compo$Al2O3/wind.compo$Fe2O3), breaks=60)

# Compare to normal distribution function:
m = mean(log(wind.compo$Al2O3/wind.compo$Fe2O3))
std = sd (log(wind.compo$Al2O3/wind.compo$Fe2O3))

curve(160*dnorm(x, mean=m, sd=std), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

plot(wind.compo$Al2O3/wind.compo$Fe2O3, wind.compo$SiO2/wind.compo$Fe2O3)
plot(wind.compo$Al2O3/wind.compo$Fe2O3, wind.compo$SiO2/wind.compo$Fe2O3, log="xy")

# Log-ratio transforms are the natural transforms for compositional data.
# Will use one of these in the following models: the aitchison log-ratio
# transform, or alr. This is the one used in the plots above, where we divide
# everything by one component, then log-transform. We will use Fe in the present
# case. 

wind.compo <- wind.compo[,c(1,2,3,4,6,7,8,5)]

wind.compo.alr <- alr(wind.compo)

wind.compo.alr

# This apparently loses us the information on Fe for now, but this will be recovered
# through back-transformation later.

# Still, there were some anomalous samples in the dataset. What are these?
# Let's look at the original data again. 

windarling

summary(windarling$Lithotype)


### Other data transforms 

# Other important compositional data transforms are the centred log-ratio transform (clr)
# and the isometric log-ratio transform (ilr). There are also various normal-score
# forms available to deal with non-normal data, and make it more tractable. See books by
# Raimon Tolosana for reference (including implementation in R).

# Normal distribution of the data is particularly important for later simulations.



### Maps 

# Before starting with the modelling, we may wish to get an impression
# of the distribution of different parameters from the raw data, by looking 
# at some maps. 

pairsmap(data=wind.compo, loc=wind.coords, pch=19, cexrange=c(1,1)*0.9, mfrow=c(4,2),
         foregroundcolor=NA)

pairsmap(data=wind.compo.alr, loc=wind.coords, pch=19, cexrange=c(1,1)*0.9, mfrow=c(4,2),
         foregroundcolor=NA)

# We also wish to know how the different sample types are distributed. This helps us to
# assess whether our domain is reasonably well-defined. 

plot(wind.coords, col=windarling$Lithotype, asp=1)
legend("topright", fill=1:4, legend=levels(windarling$Lithotype))

# Finally, we remove anomalous samples from our working data: 

wind.compo <- wind.compo[-which(windarling$Lithotype == "basalt" | windarling$Lithotype == "schist"),]
wind.coords <- wind.coords[-which(windarling$Lithotype == "basalt" | windarling$Lithotype == "schist"),]
windarling <- windarling[-which(windarling$Lithotype == "basalt" | windarling$Lithotype == "schist"),]

### Exploration of trends in the data

# Use swath plots for this of log-ratios to see whether anything happens

swath(acomp(wind.compo), wind.coords[,"Easting"], col=8, xlab="Easting")

swath(acomp(wind.compo), wind.coords[,"Northing"], col=8, xlab="Northing")


############################################# Variography #######################################################

# Make object for geostatistical modelling: 

wind.alr.gg = make.gmCompositionalGaussianSpatialModel(wind.compo, wind.coords, V="alr")

# logratio variograms for all log-ratios:

wind.pwlr.vg = logratioVariogram(acomp(wind.compo), loc=wind.coords, nbins=60,maxdist=200)

plot(wind.pwlr.vg)

# compute variograms for alr (base Fe2O3, as above):

wind.alr.vg = variogram(wind.alr.gg, cutoff=50, width=3.5)

plot(wind.alr.vg)

# adjust to plot for shorter distance (50 m)

# Anisotropy (does direction matter?)

wind.alr.vgAnis = variogram(wind.alr.gg, cutoff=50, width=5, map=TRUE)

plot(wind.alr.vgAnis)

wind.alr.vgAnis2 = variogram(wind.alr.gg, cutoff=100, width=3.5, alpha=c(90,180), tol.hor=45)

plot(wind.alr.vgAnis2, group.id=FALSE, col=c("red", "black"))

# Anisotropy (nicer maps for all logratios)

wind.pwlr.vgAnis = logratioVariogram(data=acomp(wind.compo), loc=wind.coords, maxdist=50, nbins=10, 
  azimuth.tol=15, azimuth=10*(0:35))

image(wind.pwlr.vgAnis)



######################################### Variogram modelling ###################################################

# Variogram model fitting in R is simple. We will simultaneously fit the models to the experimental variograms we generated
# above.

# LMR - linear model of regionalisation: random function as linear combination of univariate functions.
# LMC - linear model of coregionalisation: random function as linear combination of multivariate functions.



#### isotropic models

md0 = vgm("Sph") # Define variogram template to be fitted ("Sph" - spherical, "Exp" - Exponential, "Gau" - Gaussian)

wind.alr.gg = fit_lmc(v=wind.alr.vg, g=wind.alr.gg, model=md0)

plot(wind.alr.vg, wind.alr.gg$model)

# Looking at model parameters:

wind.alr.gg$model

# Clearly, model is not satisfactory for some of the cross-variograms. 

# Make an alternative model with 2 spherical structures: 

md1 = vgm (psill=1, model="Sph", range=15, nugget=1)
md1 = vgm (psill=1, model="Sph", range=45, add.to=md1)


wind.alr.gg = fit_lmc(v=wind.alr.vg, g=wind.alr.gg, model=md1, correct.diagonal=1.001)
plot(wind.alr.vg, wind.alr.gg$model)

# This is looking fairly good.

# fitted parameters:

wind.alr.gg$model$alr1

# Note which parameters were adjusted by fit_lmc(), and which were not.



#### anisotropic models 

# To adequately fit the data, we need to account for anisotropy observed in data examination.
# Specify an anisotropic model:

mdA = vgm(psill=1, model="Exp", range=10, nugget=1, anis=c(90,0.6))

# Anisotropy included through "anis" argument, specifying direction and ratio of ranges for
# major directions.

wind.alr.ggAnis = fit_lmc(v=wind.alr.vgAnis2,g=wind.alr.gg, model=mdA, correct.diagonal = 1.001)

variogramModelPlot(wind.alr.vgAnis2, wind.alr.ggAnis)

# Reasonable up to ~30m, not very good beyond.
# Bump in NS variogram beyond this is difficult to model using standard 
# variogram models, and may be an artefact of trends in the data.




############################################## (Co-)Kriging ########################################################### 

### Ordinary

# Define grid of locations for estimation (1x1m grid)
# First, get parameters:

xmin = floor(min(wind.coords[,1]))
xmax = ceiling(max(wind.coords[,1]))
ymin = floor(min(wind.coords[,2]))
ymax = ceiling(max(wind.coords[,2]))
x0 = c(xmin,ymin)
names(x0) = colnames(wind.coords)
Dx = c(1,1)
nx = c(xmax-xmin, ymax-ymin)/Dx + 1

# Construct grid

wind.gt = GridTopology(x0,Dx,nx)

wind.grid.fine = SpatialGrid(wind.gt)

# Set mask for to restrict kriging to points close to measurements: 

wind.mask.fine = constructMask(wind.grid.fine, method="maxdist", maxval=7, x=wind.alr.ggAnis$data$alr1$data)
wind.grid.fine.masked = setMask(wind.grid.fine, wind.mask.fine)

# Define Kriging parameters:

wind.ng = KrigingNeighbourhood(nmax=20,nmin=4,maxdist=20)

wind.alr.gg = make.gmCompositionalGaussianSpatialModel(wind.compo, wind.coords, V="alr", ng=wind.ng, model=wind.alr.ggAnis$model)

# Executing kriging estimates: 

wind.alr.ock = predict(wind.alr.gg, newdata=wind.grid.fine.masked, debug.level=-1) %>% unmask(mask=wind.mask.fine)

# Can now obtain maps of estimates (back-transformed to concentrations) via: 

wind.compo.ock = gsi.gstatCokriging2compo(wind.alr.ock, V="alr", orignames=colnames(wind.compo))
wind.acomp = acomp(wind.compo)

myfun = function(i){
  bks=quantile(wind.acomp[,i],probs=seq(0,1,0.1), na.rm=T)
  out=image_cokriged(wind.compo.ock, ivar=i, breaks=bks)
  points(wind.coords, pch=21, col=1, cex=1.5, bg=out$col[cut(wind.acomp[,i],out$breaks)])
}

sapply(c("Fe2O3", "Al2O3", "SiO2", "MnO2", "P2O5", "SO3", "LOI"), myfun)

### Universal

# Let's try with a simple N-S trend:

wind.alr.gg.uck = make.gmCompositionalGaussianSpatialModel(wind.compo, wind.coords, V="alr", formula = ~1+Northing, 
                                                           ng=wind.ng, model=wind.alr.ggAnis$model)

# Formula used is sepcified with "formula" argument.

wind.alr.uck = predict(wind.alr.gg.uck, newdata=wind.grid.fine.masked, debug.level=-1) %>% unmask(mask=wind.mask.fine)

# Can now obtain maps of estimates (back-transformed to concentrations) via: 

wind.compo.uck = gsi.gstatCokriging2compo(wind.alr.uck, V="alr", orignames=colnames(wind.compo))
wind.acomp = acomp(wind.compo)

myfun = function(i){
  bks=quantile(wind.acomp[,i],probs=seq(0,1,0.1), na.rm=T)
  out=image_cokriged(wind.compo.ock, ivar=i, breaks=bks)
  points(wind.coords, pch=21, col=1, cex=1.5, bg=out$col[cut(wind.acomp[,i],out$breaks)])
}

sapply(c("Fe2O3", "Al2O3", "SiO2", "MnO2", "P2O5", "SO3", "LOI"), myfun)


######################################## Model cross-validation #################################################

# Important to check goodness of model via x-validation.
# leave-one-out: remove one data point and predict from others; successively for all data points
# Then compare with original data to see how good fit is. 

wind.alr.xvAnis = gstat.cv(as.gstat(wind.alr.gg), remove.all = TRUE, all.residuals = TRUE, verbose = FALSE)

xv.true = data.frame(alr(wind.compo))
xv.res = wind.alr.xvAnis
xv.preds = xv.true - xv.res
summary(xv.preds)

# Plots 1

par(mfcol=c(4,7), mar=c(4,4,3,1))
for(i in 1:7){
  plot(xv.true[,i] ~ xv.preds[,i], ylab="observed", xlab="predicted", asp=1, main=colnames(xv.preds)[i])
  abline(a=0, b=1)
  abline(lm(xv.true[,i] ~ xv.preds[,i]), col=2)
  hist(xv.res[,i], xlab="residual", main="")
  qqnorm(xv.res[,i], main="Normal QQ plot")
  qqline(xv.res[,i], col=2)
  abline(h=0)
  boxplot(xv.res[,i] ~windarling$Lithotype, horizontal = T)
  abline(v=0)
  }

# Plots 2

xv.preds.comp = alrInv(xv.preds, orig=wind.compo)
xv.true.comp = alrInv(xv.true, orig=wind.compo)
xv.res.comp = alrInv(xv.res, orig=wind.compo)
par(mfrow=c(6,6), mar=c(1,1,1,1)/4, oma=c(3,3,3,3))

for(i in 1:6){
  for(j in 1:6){
  if(i==j){
  plot(c(0,1), c(0,1), bty="n",xaxs="i", yaxs="i",
  xaxt="n", yaxt="n", pch="")
  text(0.5, 0.5, labels =
  expression(Fe, Al[2]*O[3],SiO[2], Mn, P , Rest)[i], cex=1.25)
  }else if(i>j){
  # lower tri
  x = log(xv.preds.comp[,i]/xv.preds.comp[,j])
  y = log(xv.res.comp[,i]/xv.res.comp[,j])
  vp.kde2dplot(x,y, add=F, colpalette = spectralcolors)
  abline(h=0)
  }else{
  # upper tri
  x = log(xv.preds.comp[,i]/xv.preds.comp[,j])
  y = log(xv.true.comp[,i]/xv.true.comp[,j])
  vp.kde2dplot(x,y, add=F, colpalette = spectralcolors)
  abline(a=0, b=1)
  }
  }
  }

mtext(side = 1:4, line=1, outer=TRUE, text=paste("log-ratio",
                          c("predictions", "residuals","predictions","observations")))


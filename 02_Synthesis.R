############################# Mining simulation - NPV calculation ########################################

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

############################# Blocking of compositional estimates ########################################

### Kriging estimates

#Convert data to spatial points dataframe:
spMean = SpatialPointsDataFrame(coords=wind.grid.fine, data.frame(wind.compo.ock))

# How many blocks do we need?
summary(wind.grid.fine)

(summary(wind.grid.fine)$bbox[,2]-summary(wind.grid.fine)$bbox[,1])/5

#Define block grid:
wind.blockgrid.topo = GridTopology(cells.dim=c(89,22), cellcentre.offset = c(-240,125), cellsize=c(5,-5))

wind.blockgrid = SpatialGrid(wind.blockgrid.topo)

spAggr = aggregate(spMean, by=wind.blockgrid, FUN=mean)

spplot(spAggr) # plots all components

cbind(coordinates(spAggr), spAggr@data) %>% image_cokriged(ivar="Fe2O3") # plots Fe2O3 only


# As an exercise: examine object spAggr and its structure, and see how we can best extract individual 
# block compositions and coordinates for further processing.


### Simulations

# Define aggregation again: 

myAggrFun = function(x){
  spx = SpatialPointsDataFrame(coords=wind.grid.fine,
                                 data=data.frame(x))
  spAggr = aggregate(spx, by=wind.blockgrid, FUN=mean)
  return(spAggr@data)
}

wind.compo.sim.block = gmApply(wind.compo.osim, FUN=myAggrFun)

dim(wind.compo.sim.block)

dimnames(wind.compo.sim.block)[-1]

# Plot: 

myfun = function(i,j){
  bks = quantile(wind.acomp[,j], prob=(0:10)/10)
  out = coordinates(wind.blockgrid) %>%
    cbind(getStackElement(wind.compo.sim.block,i)) %>%
    image_cokriged(ivar=j, legendPos = "top", breaks = bks)
  points(wind.coords, pch=21, col=1, cex=1,
    bg=out$col[cut(wind.acomp[,j],out$breaks)])
  }

manipulate(myfun(i,j),
      i=slider(1, max = 3),
      j=picker("Fe2O3", "Al2O3", "SiO2", "MnO2", "P2O5", "SO3", "LOI", "CL"))


# How do we extract block compositions for single simulations? (Exercise)


#################################### Calculation of block values #########################################

### Best-fit model (kriging)

# First, do the calculation for the kriging model (best.fit). 
# You can do this most easily by coercing object spAggr to a data.frame:

Krig_NPV = data.frame(spAggr)

# Now make new columns with the mineralogy, density and estimated cost, sale value and revenue 
# for each of the blocks. Also include a classification of what is ore and what is waste. For 
# waste materials: assume that they will still need to be extracted, at a cost of 10$ / t.

# Implement this as an exercise. 

Krig_NPV <- Krig_NPV[which(Krig_NPV$Fe2O3!="NA"),]

Krig_NPV$Hm = Krig_NPV$Fe2O3
Krig_NPV$Dl = Krig_NPV$MnO2 + Krig_NPV$P2O5/0.418
Krig_NPV$Sh = Krig_NPV$Al2O3*2
Krig_NPV$Qz = Krig_NPV$SiO2 - Krig_NPV$Al2O3

Krig_NPV$Qz[which(Krig_NPV$Qz<0)] = -Krig_NPV$Qz[which(Krig_NPV$Qz<0)]

Krig_NPV$sum = Krig_NPV$Hm + Krig_NPV$Dl + Krig_NPV$Sh + Krig_NPV$Qz

Krig_NPV[which(wind.compo.aux$sum>1),] = Krig_NPV[which(Krig_NPV$sum>1),]/
  Krig_NPV$sum[which(Krig_NPV$sum>1)]

Krig_NPV$rho = Krig_NPV$sum/(Krig_NPV$Hm/3.5 + Krig_NPV$Dl/3.5 + 
                                       Krig_NPV$Sh/2.5 + Krig_NPV$Qz/2.5)  

Krig_NPV$Cost <- Cost(Krig_NPV)
Krig_NPV$Sale.Value <- Sale.Value(Krig_NPV)
Krig_NPV$Revenue <- Krig_NPV$Sale.Value - Krig_NPV$Cost
Krig_NPV$Type <- Ore.Waste(Krig_NPV)

# Including costs for waste rocks:
Krig_NPV$Revenue[which(Krig_NPV$Type=="W")] = -5*5*6*Krig_NPV$rho[which(Krig_NPV$Type=="W")]*10

summary(Krig_NPV$Revenue)



### Simulations

# Before we proceed, will transfer our simulation data from a DataFrameStack into a list.
# This is more intuitive in terms of manipulation.We will also repeat all calculations for the single
# deposit model resulting from kriging. 

NPV_Sim = list()

length(NPV_Sim) = 20

for (i in 1:20) {
  NPV_Sim[[i]] <- getStackElement(wind.compo.sim.block, i)

  NPV_Sim[[i]] <- NPV_Sim[[i]][which(NPV_Sim[[i]]$Fe2O3!="NA"),]
  
  }


# Mineralogy

for(i in 1:20) {
  
  NPV_Sim[[i]]$Hm = NPV_Sim[[i]]$Fe2O3
  NPV_Sim[[i]]$Dl = NPV_Sim[[i]]$MnO2 + NPV_Sim[[i]]$P2O5/0.418
  NPV_Sim[[i]]$Sh = NPV_Sim[[i]]$Al2O3*2
  NPV_Sim[[i]]$Qz = NPV_Sim[[i]]$SiO2 - NPV_Sim[[i]]$Al2O3
  
  NPV_Sim[[i]]$Qz[which(NPV_Sim[[i]]$Qz<0)] = -NPV_Sim[[i]]$Qz[which(NPV_Sim[[i]]$Qz<0)]
  
  NPV_Sim[[i]]$sum = NPV_Sim[[i]]$Hm + NPV_Sim[[i]]$Dl + NPV_Sim[[i]]$Sh + NPV_Sim[[i]]$Qz
  
  NPV_Sim[[i]][which(wind.compo.aux$sum>1),] = NPV_Sim[[i]][which(NPV_Sim[[i]]$sum>1),]/
    NPV_Sim[[i]]$sum[which(NPV_Sim[[i]]$sum>1)]

  NPV_Sim[[i]]$rho = NPV_Sim[[i]]$sum/(NPV_Sim[[i]]$Hm/3.5 + NPV_Sim[[i]]$Dl/3.5 + 
                                         NPV_Sim[[i]]$Sh/2.5 + NPV_Sim[[i]]$Qz/2.5)  
  
}


# Value / Revenue + Ore / Waste

for(i in 1:20) {
  
  NPV_Sim[[i]]$Cost <- Cost(NPV_Sim[[i]])
  NPV_Sim[[i]]$Sale.Value <- Sale.Value(NPV_Sim[[i]])
  NPV_Sim[[i]]$Revenue <- NPV_Sim[[i]]$Sale.Value - NPV_Sim[[i]]$Cost
  NPV_Sim[[i]]$Type <- Ore.Waste(NPV_Sim[[i]])
  NPV_Sim[[i]]$Revenue[which(NPV_Sim[[i]]$Type=="W")] = 
    -5*5*6*NPV_Sim[[i]]$rho[which(NPV_Sim[[i]]$Type=="W")]*10
}

head(NPV_Sim[[1]])



############################### Calculation of total resources / reserves ################################

### Total tons of ore:

# This is the sum of the masses of all blocks identified as "Ore".

# Calculate it for the kriging model first: 

sum(Krig_NPV$rho[which(Krig_NPV$Type=="O")]*5*5*6)


# Then try to calculate it for the different simulations, collecting results in a new data.frame

Results = data.frame(Tonnage=1:20)

for(i in 1:20) {
  
  Results$Tonnage[i] <- sum(NPV_Sim[[i]]$rho[which(NPV_Sim[[i]]$Type=="O")]*5*5*6)
  
}

hist(Results$Tonnage)

### Total grade: 

# Amount of hematite in all "Ore" blocks, divided by mass of "Ore".
# Best fit results first:

sum(Krig_NPV$rho[which(Krig_NPV$Type=="O")]*5*5*6*Krig_NPV$Hm[which(Krig_NPV$Type=="O")]) / 
  sum(Krig_NPV$rho[which(Krig_NPV$Type=="O")]*5*5*6)

# Simulations after:

for(i in 1:20) {
  
  Results$Grade[i] <- sum(NPV_Sim[[i]]$rho[which(NPV_Sim[[i]]$Type=="O")]*5*5*6 * 
                            NPV_Sim[[i]]$Hm[which(NPV_Sim[[i]]$Type=="O")]) / 
    sum(NPV_Sim[[i]]$rho[which(NPV_Sim[[i]]$Type=="O")]*5*5*6)
  
  
}

hist(Results$Grade)



### Total in-ground value (without accounting for time of mining):

# This is simply the sum of the individual block revenues for all blocks.
# For kriging: 

sum(Krig_NPV$Revenue[which(Krig_NPV$Type=="O")])

# For the simulations: 

for(i in 1:20) {
  Results$Total.Rev[i] <- sum(NPV_Sim[[i]]$Revenue[which(NPV_Sim[[i]]$Type=="O")])
}

hist(Results$Total.Rev)



############################################ Mine Schedule #############################################

# A schedule can be defined via a vector with the row numbers of the blocks to be mined in the order
# in which mining is planned. Here, we will work with three different mine-schedules to illustrate 
# the effects of scheduling:

# 1) Ordering the blocks according to their revenue (assuming we can extract blocks 
#    by themselves, without regard for the surrounding blocks); most valuable blocks first.

Schedule1 <- order(Krig_NPV$Revenue, decreasing = TRUE)


# 2) Using block order identical to order in the current data.file (going from ... to ... )

Schedule2 <- 1:879


# 3) Ordering blocks by ascending value; the least valuable blocks first.

Schedule3 <- order(Krig_NPV$Revenue)



############################################ NPV Calculation ###########################################

# Will do this calculation assuming we have no investment costs (CAPEX). Can still compare outcomes.
# First thing we need to do is to define a time-line. Let's say our bench will be mined over the course 
# of 5 years at a rate of 80000 t of ore / yr

# Use this to construct a timeline - each ore block is assigned a processing time
# based on its tonnage. 


### Kriging estimates

Krig_NPV$t <- Krig_NPV$rho*5*5*6 / 80000

# These individual contributions are then added up to give cumulative time in a separate column

Krig_NPV$T <- 1:879

Krig_NPV$T[Schedule1][1] <- Krig_NPV$t[Schedule1][1]

for(i in 2:879) {
  
  Krig_NPV$T[Schedule1][i] <- Krig_NPV$T[Schedule1][i-1] + Krig_NPV$t[Schedule1][i]
  
}


# Can now calculate contribution to NPV of each block via our standard formula:

Krig_NPV$NPV <- Krig_NPV$Revenue/((1+0.08)^Krig_NPV$T)

# Overall NPV is simply the sum of these: 

sum(Krig_NPV$NPV)



### Simulations

# Again, need to implement schedules

for (i in 1:20) {
  
  NPV_Sim[[i]]$t <- NPV_Sim[[i]]$rho*5*5*6 / 80000
  
}

# These individual contributions are then added up to give cumulative time in a separate column

for (i in 1:20) {
  
  NPV_Sim[[i]]$T <- 1:879
  
  NPV_Sim[[i]]$T[Schedule1][1] <- NPV_Sim[[i]]$t[Schedule1][1]

    for(j in 2:879) {
    
    NPV_Sim[[i]]$T[Schedule1][j] <- NPV_Sim[[i]]$T[Schedule1][j-1] + NPV_Sim[[i]]$t[Schedule1][j]
    
  }
}

# Calculate NPV contributions:

for (i in 1:20) {

  NPV_Sim[[i]]$NPV <- NPV_Sim[[i]]$Revenue/((1+0.08)^NPV_Sim[[i]]$T)  
  
}


# Calculate overall NPVs for the different simulations and add to Results data.frame:

for (i in 1:20) {
  
  Results$NPV[i] <- sum(NPV_Sim[[i]]$NPV)
  
}

hist(Results$NPV)

mean(Results$NPV)

abline(v=sum(Krig_NPV$NPV))


# How do results from the two calculation methods compare?

# Why may this be the result? What was different in the example I showed you in the lecture?
# (Note: the example in the lectures was for a gold mine!)


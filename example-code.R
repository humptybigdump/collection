## ---- message=FALSE, warning=FALSE------------------------------------------------
# # Install (if applicable) and load all the required R packages we are going to use in ths exercise. 
if(!require('pacman')) install.packages('pacman')
pacman::p_load('data.table', 'dismo', 'ggplot2','raster', 'RColorBrewer', 'rgdal', 'scales', 'SDMtune', 'sf', 'stars', 'terra', 'tmap')


## ---------------------------------------------------------------------------------
# Load custom functions/methods that we will use during the exercise.
source('R/utility-functions.R')


## ---------------------------------------------------------------------------------
# Load data
basepreds <- terra::rast('data/baseline-predictors.tif')
projpreds <- terra::rast('data/future-predictors.tif')


## ---------------------------------------------------------------------------------
basepreds


## ---------------------------------------------------------------------------------
names(basepreds)


## ---------------------------------------------------------------------------------
# Select the respective layers from basepreds and projpreds 
mydat <- rast(list(subset(basepreds, 'IF'), subset(projpreds, 'IF')))

# Assign meaningful names
names(mydat) <- c('Intensively.Farmed.Baseline', 'Intensively.Farmed.2080')

# Plot
plt_maps(dat=mydat)


## ---------------------------------------------------------------------------------
# Select the respective layers from basepreds and projpreds 
mydat <- rast(list(subset(basepreds, 'MF'), subset(projpreds, 'MF')))

# Assign meaningful names
names(mydat) <- c('Managed.Forest.Baseline', 'Managed.Forest.2080')

# Plot
plt_maps(dat=mydat)


## ---- warning=FALSE, message=FALSE------------------------------------------------
plt_interactive(dat=mydat, asLay=TRUE)

plt_interactive(dat=mydat, asLay=FALSE)


## ---------------------------------------------------------------------------------
# Names of land-use predictors 
lun <- c('IF', 'IG', 'EG', 'VEG', 'UL', 'MF', 'UF', 'UR')

# Subset land-use predictors 
basepredsdt <- subset(basepreds, lun)
projpredsdt <- subset(projpreds, lun)

# Convert percentages to areas [km²]
basepredsdt <- (basepredsdt/100)*(cellSize(basepredsdt)/1000000)
projpredsdt <- (projpredsdt/100)*(cellSize(projpredsdt)/1000000)

# Convert to data.tables
basepredsdt <- for_ras(basepredsdt)
projpredsdt <- for_ras(projpredsdt)

# Reshape to long format and add time information 
basepredsdt <- melt(basepredsdt, id.vars=c('x', 'y'))
basepredsdt[, tick:='base']
projpredsdt <- melt(projpredsdt, id.vars=c('x', 'y'))
projpredsdt[, tick:='proj']

# Combine 
h <- rbind(basepredsdt, projpredsdt)

# Summarize 
pdt <- h[, .(area=sum(value)), by=.(variable, tick)]

# Create boxplot
p <- ggplot(pdt, aes(x=tick, y=area, fill=variable))+
  geom_bar(stat='identity', color='lightgrey')

p <- p+
  scale_fill_brewer(type='qual', palette='Set2', name='Land use')+
  scale_y_continuous(labels=scales::comma)+
  theme_bw()+
  theme(axis.title.x=element_blank())+
  ylab('Area [km²]')

p


## ---------------------------------------------------------------------------------
# Load SDMs 
sdms <- readRDS('data/sdm.rds')
#sdms


## ---------------------------------------------------------------------------------
# Calculate variable importance 
vi <- lapply(sdms, FUN=varImp, permut=5)

# Plot variable importance
lapply(vi, FUN=plotVarImp)



## ---- warning=FALSE, message=FALSE------------------------------------------------
# Probabilities: Apply SDMs to baseline conditions
basemaps <- lapply(sdms, FUN=predict, data=stack(basepreds))

bm <- rast(stack(basemaps))

# Change the 'var' parameter to show/compare different or all five species. 
plt_interactive(bm, var=c('schrencki', 'terrestris'))


## ---------------------------------------------------------------------------------
# Get thresholds 
th <- lapply(sdms, FUN=function(x) thresholds(x, type='cloglog')[3, 2])



## ---------------------------------------------------------------------------------
# For each species convert probability maps to presence/absence maps based on species-specific threshold
basepa <- mapply(FUN=function(x, y) x>=y, x=basemaps, y=th)


## ---- warning=FALSE, message=FALSE------------------------------------------------
# Convert to SpatRaster and plot
bpa <- rast(stack(basepa))

plt_interactive(bpa, var=c('schrencki', 'terrestris'))


## ---- warning=FALSE, message=FALSE------------------------------------------------
# Apply the SDMs to future conditions
projmaps <- lapply(sdms, FUN=predict, data=stack(projpreds))

pm <- rast(stack(projmaps))

# Convert to presence/absence using the same thresholds than for baseline conditions
projpa <- mapply(FUN=function(x, y) x>=y, projmaps, th)

ppa <- rast(stack(projpa))

plt_interactive(ppa, var=c('schrencki', 'terrestris'))


## ---- warning=FALSE, message=FALSE------------------------------------------------
# Select a species from the presence/absence maps for both time periods 
mydat <- get_mydat(bpa, ppa, 'schrencki')

plt_interactive(mydat, 'schrencki')


## ---- warning=FALSE, message=FALSE------------------------------------------------
crs(ppa) <- crs(bpa)
#--> ONLY valid as we know that the projections are identical! 

# Change maps 
chpa <- ppa-bpa

plt_interactive(chpa, var='terrestris', cha=TRUE)



## ---------------------------------------------------------------------------------
bdt <- for_ras(rast(stack(bpa)))
bdtm <- melt(bdt, id.vars=c('x', 'y'), variable.name='species', value.name='base')

pdt <- for_ras(rast(stack(ppa)))
pdtm <- melt(pdt, id.vars=c('x', 'y'), variable.name='species', value.name='proj')

# Combine both datasets and calculate changes 
resdt <- merge(bdtm, pdtm, by=c('x','y','species'))
resdt[, change:=proj-base]

# We are working on a lat/lon grid, i.e. grid cells differ in their actual area along a north/south gradient. We account for this by adding the area of the grid cells to the data.table 
parea <- cellSize(subset(bpa, 1))/1000000
pareadt <- for_ras(parea)
resdt <- merge(resdt, pareadt, by=c('x', 'y'))

# Calculate areas and gains/losses
resdt[, c('basea', 'proja', 'changea'):=lapply(.SD, function(x) x*area), .SDcols=c('base', 'proj', 'change')]

resdt[, gain:=ifelse(change==1, changea, 0)]
resdt[, loss:=ifelse(change==-1, changea, 0)]

# Now summarize the data (i.e., sum gains/losses from all grid cells per species) and prepare for an overview plot 
gl <- resdt[, .(gain=sum(gain), loss=sum(loss), baseline=sum(basea)), by=species]
gl[, net:=gain+loss]

# Convert to percentages
gl[, c('gain', 'loss', 'net'):=lapply(.SD, function(x) {(x/baseline)*100}), 
   .SDcols=c('gain', 'loss', 'net')]

gl[, baseline:=NULL]

gl <- melt(gl, id.vars='species')

ggplot(data=gl[variable %in% c('gain', 'loss')], aes(x=species, y=value, fill=variable))+
  geom_bar(stat='identity')+
  geom_bar(data=gl[variable=='net'], aes(x=species, y=value), fill='black',
           stat='identity', alpha=0.7)+
  scale_fill_manual(values=c('#7570b3', '#1b9e77'), name='Variable')+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0))+
  ylab('% of baseline')+
  xlab('')+
  scale_y_continuous(labels=scales::comma)


## ---------------------------------------------------------------------------------
# Calculate range shifts
cent1 <- resdt[base==1, .(bmaxy=max(y), bminy=min(y)), by=species]
cent2 <- resdt[proj==1, .(pmaxy=max(y), pminy=min(y)), by=species]

cent <- merge(cent1, cent2, by='species')
cent[, c('bcenty', 'pcenty'):=.((bmaxy+bminy)/2, (pmaxy+pminy)/2)]

cent <- melt(cent, id.vars='species')

ggplot(data=cent[variable %in% c('bcenty', 'pcenty')], aes(x=species, y=value, color=variable))+
  geom_point(size=4)+
  scale_color_manual(values=c('#7570b3', '#1b9e77'), name='Variable')+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=0))+
  xlab('')+
  ylab('Latitude')


## ---------------------------------------------------------------------------------
# Load baseline presence/absence maps
bpaall <- rast('data/basepa.tif')

# Load future presence/abence maps
ppaall <- rast('data/projpa.tif')

crs(ppaall) <- crs(bpaall)
#--> ONLY valid as we know that the projections are identical!


## ---- warning=FALSE, message=FALSE------------------------------------------------
# Calculate species richness (=number of species per grid cell) for both time periods 
bsrall <- app(bpaall, fun=sum)
psrall <- app(ppaall, fun=sum)

# Plot next to each other
plt_interactive(get_mydat(bsrall, psrall, 'sum'))

# Calculate change in species richness 
chsr <- psrall-bsrall

plt_interactive(chsr, var='sum', pal=brewer.pal(n=10, 'RdBu'))


#------------------------------------------------------------------------------#
# FUNCTIONS/METHODS HELPFUL IN SIMPLE VERSION
#------------------------------------------------------------------------------#

# FORTIFY RASTER OBJECT -------------------------------------------------------#
# 
# Description:
# Method to convert 'RasterLayer' (raster) or 'SpatRaster' (terra) objects to 
# data.table objects. Useful to prepare data for ggplot.  
# 
# Arguments:
# @param  x         RasterLayer OR SpatRaster object to be converted to 
#                   data.table.
# @param  na.rm     Logical. If TRUE, NA cells are omitted from data.table re-
#                   presentation (default). 
#                   
# Value: 
# data.table

for_ras <- function(x, na.rm = TRUE) {
  
  dt <- setDT(as.data.frame(x, xy = TRUE))
  
  if(na.rm) {
    
    dt <- na.omit(dt)
    
  }
  
  return(dt)
  
}

#------------------------------------------------------------------------------#

# PLOT STATIC MAP -------------------------------------------------------------#
#
# Description:
# Method to plot RasterLayer/SpatRaster to a static map using ggplot2.  
# 
# Arguments:
# @param  dat       RasterLayer OR SpatRaster.
# @param  na.rm     character. Indicating the name of the layer you would like 
#                   to plot. 
#                   
# Value: 
# ggplot
plt_maps <- function(dat, var=NULL) {
  
  # Make sure that dat is a SpatRaster ----------------------------------------#
  if (!isTRUE(class(dat)=='SpatRaster')) {
    a <- terra::rast(dat)
  } else {
    a <- dat
  }

  # Subset the variable/layer to be plotted -----------------------------------# 
  if (!is.null(var)) {
    a <- terra::subset(dat, names(dat)[names(dat) %in% var])
  } 

  # Convert to data.table -----------------------------------------------------#
  b <- for_ras(a)
  #setnames(b, c('x', 'y', 'base', 'proj'))
  b <- melt(b, id.vars=c('x', 'y'))
  
  # Plot ----------------------------------------------------------------------#
  c <- ggplot(b, aes(x=x, y=y, fill=value))+
    geom_tile()+
    scale_fill_gradientn(colors=rev(terrain.colors(10)), name='Coverage [%]')+
    coord_equal()+
    theme_bw()+
    theme(axis.text=element_blank(), 
          axis.title=element_blank(), 
          axis.ticks=element_blank())
  
  if(nlyr(a) > 1) c <- c+facet_wrap(.~variable)
  
  c
}
#------------------------------------------------------------------------------#

# PLOT INTERACTIVE MAP---------------------------------------------------------#
#
# Description:
# Method to plot RasterLayer/SpatRaster to an interactive map using tmap.  
# 
# Arguments:
# @param  dat       RasterLayer OR SpatRaster.
# @param  na.rm     character. Indicating the name of the layer you would like 
#                   to plot. 
# @param  asLay     Logical. If TRUE and several layers available in dat/var, 
#                   these are plotted on top of each other. Next to each other,
#                   if FALSE (default).
# @param  cha       Logical. If TRUE apply c('red', 'white', 'blue') color 
#                   palette to visualize change in presence/absence.
#                   
# Value: 
# ggplot

plt_interactive <- function(dat, var=NULL, asLay=FALSE, cha=FALSE, pal=NULL){
  
  # Make sure that dat is a SpatRaster ----------------------------------------#
  if (!isTRUE(class(dat)=='SpatRaster')) {
    a <- terra::rast(dat)
  } else {
    a <- dat
  }
  
  # Subset the variable/layer to be plotted -----------------------------------#
  if (!is.null(var)) {
    if(length(var) > 1) {
      var1 <- paste(var, collapse='|')
    } else {
      var1 <- var
      }
    a <- terra::subset(a, names(a)[names(a) %like% var1])
  }
  
  # Set very small (imprecise) negative values to 0 ---------------------------#
  a[a<0 & a>-0.0001] <- 0
  
  # Write to file and reload as stars object ----------------------------------# 
  tmptif <- paste0(getwd(), '/', 'tmp.tif')
  writeRaster(a, tmptif, overwrite=TRUE)
  a <- read_stars(tmptif)

  # Convert to stars object ---------------------------------------------------# 
  #b <- stars::st_as_stars(a)
  #if(nlyr(a) > 1) b <- split(b, 'band')
  if(length(attr(a, 'dimension'))>2) {
    b <- split(a, 'band')
  } else {
    b <- a
    names(b) <- var
  }

  #names(b) <- names(a)
  #if(names(b) %in% var1) b <- b[var1]
  
  # Plot ----------------------------------------------------------------------#
  # Define color palette
  if(is.null(pal)) {
    cp <- rev(terrain.colors(10))
  } else {
    cp <- pal
  }
  
  tmap_mode(mode='view')
  
  c <- tm_shape(b)
  
  if (cha) {
    c <- c+tm_raster(style='cat', palette=c('red', 'white', 'blue'))
  } else {
    c <- c+tm_raster(style='cont', palette=cp)+
      tm_facets(free.scales.raster=FALSE)
  }
  
  if(asLay){
    c <- c+tm_facets(as.layers=TRUE, 
                     free.scales.raster=FALSE)
  }
  
  file.remove(paste0(getwd(), '/', 'tmp.tif'))
  
  c
}
#------------------------------------------------------------------------------#

# GET BASE/PROJ DATASET -------------------------------------------------------#
#
# Description:
# Method to extract the same layer (e.g., land-use, species) from two SpatRaster
# representing baseline and future conditions.  
# 
# Arguments:
# @param  baseDat   SpatRaster. Baseline conditions.
# @param  projDat   SpatRaster. Future conditions. 
# @param  var       Character. Layer/variable name to extract.
#                   
# Value: 
# SpatRaster

get_mydat <- function(baseDat, projDat, var) {
  
  ln <- names(baseDat)[names(baseDat) %like% var]
  
  mydat <- rast(list(subset(baseDat, ln), subset(projDat, ln)))
  
  names(mydat) <- paste0(var, '.', c('base', 'proj'))
  
  return(mydat)
}
#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
# FUNCTIONS/METHODS HELPFUL IN ADVANCED VERSION
#------------------------------------------------------------------------------#

# CREATE CRAFTY/IAP RASTER --------------------------------------------------------#
# 
# Description:
# Method to create a 'RasterLayer' object from CRAFTY/IAP2 cell id and result 
# file. Cell values can be either numeric cell id, numeric AFT, service/capital 
# levels (CRAFTY) or any IAP2 output.
# 
# Arguments: 
# @param  idfile    Character OR data.table. File that holds all information 
#                   about CRAFTY/IAP2 cells [e.g., ID, coordinates, 
#                   administrative unit, ...]
# @param  resfile   Character OR data.table. File that holds all information 
#                   about CRAFTY/IAP2 results for a certain time step [e.g., 
#                   services, capitals, AFT, land-use percentages, ...]
# @param  valcol    Character. Column name of the column that will be the raster
#                   cell values. Default is 'LandUseIndex' [= Numeric AFTs].
# 
# Value: 
# RasterLayer

cty_ras <- function(idfile, resfile, valcol = 'LandUseIndex') {
  
  # Read/assign files 
  if (is.character(idfile)) {
    
    id <- data.table::fread(idfile)
    
  } else {
    
    id <- idfile
    
  }
  
  if (is.character(resfile)) {
    
    res <- data.table::fread(resfile)
    
  } else {
    
    res <- resfile 
    
  }
  
  # There is no joint identifier to link cells from both files. However, cells 
  # are given in the same order in both files and can thus just be bind together.
  
  dt <- cbind(id, res)
  
  # Longitude/Latitude information in the id file miss some precisions, which 
  # leads to an error [='grid cells not equally spaced'] upon converting to 
  # raster object directly. A detour through a 'SpatialPixelsDataFrame' allows 
  # to steer the imprecision by the parameter 'tolerance'. Subsequent resampling
  # ensures extent and exact cell size. 
  
  map <- SpatialPixelsDataFrame(points = SpatialPoints(cbind(dt$Longitude, 
                                                             dt$Latitude)), 
                                data = dt[, valcol, with = FALSE], 
                                tolerance = 0.0011)
  
  map <- raster(map)
  
  map <- raster::resample(x = map, 
                          y = raster(extent(map), 
                                     resolution = c(1/6, 1/6)),
                          method = 'ngb')
  
  crs(map) <- CRS('+init=epsg:4326')
  
  if (names(map) == "LandUseIndex") names(map) <- 'AFT'
  
  return(map)
  
}

#------------------------------------------------------------------------------#


# GET AFT DESCRIPTION ---------------------------------------------------------#
# 
# Description: 
# Method to get mapping between numeric AFT and AFT acronyms [and AFT full names]
# 
# Arguments: 
# @param  ctyfile     Character OR data.table. CRAFTY [result] file that contains 
#                     information on both numeric and acronym AFT description. 
# @param  aftnum      Character. Column name of the variable that holds numeric 
#                     AFT description. Default is 'LandUseIndex'. 
# @param  aftacr      Character. Column name of the variable that holds acronym 
#                     AFT description. Default is 'Agent'. 
# @param  fullnames   Character. External file that provides mapping between AFT
#                     acronyms and full names. Default is NULL.
#                     
# Value: 
# data.table

cty_aftconv <- function(ctyfile, fullnames = NULL, aftnum = 'LandUseIndex',
                        aftacr = 'Agent') {
  
  # Read/assing file 
  if(is.character(ctyfile)){
    
    dt <- fread(ctyfile)
    
  } else {
    
    dt <- ctyfile 
    
  }
  
  # Extract mapping table 
  dt <- unique(dt, by = c(aftnum, aftacr))
  dt <- dt[, c(aftnum, aftacr), with = FALSE]
  
  # Optional: Add fullnames 
  if (!is.null(fullnames)) {
    
    dt1 <- fread(fullnames)
    
    dt  <- merge(dt, dt1, by = aftacr)
    
  }
  
  return(dt)
  
}

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# MATCH SPECIES RECORDS TO SDM EXTENT AND RESOLUTION 
#------------------------------------------------------------------------------#
# Description: 
# 
# Arguments: 
# @param  x         data.table. Data.table containing species records. Must at
#                   least include columns 'Species', 'Longitude', 'Latitude'.
# @param  sdmext    raster. RasterLayer/SpatRaster that determines extent and 
#                   spatial resolution of study area.
# @param  period    numeric. Time period from which records shall be considered.
# 
# @param  xproj     CRS of x. 
# 
# @param  sdmproj   CRS of mask (=sdmext).
# 
# Value: 
# data.table

match_species_records <- function(x, sdmext, period=NULL, 
                                  xproj, sdmproj) {
  
  # CONVERT sdmext TO TERRA::SPATRASTER ---------------------------------------#
  if (!isTRUE(class(sdmext)=='SpatRaster')) sdmext <- terra::rast(sdmext)
  
  # SELECT TIME PERIOD --------------------------------------------------------#
  if(!is.null(period)) x <- x[Year>=period[1] & Year<=period[2]]
  
  # SPECIES NAMES -------------------------------------------------------------#
  sn <- unique(x$Species)
  
  # MATCH RECORDS -------------------------------------------------------------#
  out <- list()
  
  for(i in sn) {
    
    # SELECT SPECIES ----------------------------------------------------------#
    a <- x[Species==i]
    
    # NUMBER OF RECORDS -------------------------------------------------------#
    nr <- nrow(a)
    
    # CONVERT TO POINTS -------------------------------------------------------#
    pts <- terra::vect(as.matrix(a[, .(Longitude, Latitude)]), type='points', crs=xproj)
    
    # PROJECT TO sdmext CRS ---------------------------------------------------#
    if(isTRUE(xproj!=sdmproj)) pts <- terra::project(pts, y=sdmproj)
    
    # CROP TO sdmext EXTENT ---------------------------------------------------#
    pts <- terra::crop(pts, y=sdmext)
    
    # PROCESS ONLY IF OCCURENCES IN sdmext ------------------------------------#
    if (length(pts)!=0) {
      
      # CONVERT TO RASTER -------------------------------------------------------#
      ras <- terra::rasterize(x=pts, y=terra::rast(sdmext))
      
      # CONVERT TO PRESENCE/ABSENCE 
      ras[!is.na(ras)] <- 1
      
      # MASK BY sdmext ----------------------------------------------------------#
      ras <- terra::mask(x=ras, mask=sdmext)
      
      # CONVERT TO DATA.TABLE ---------------------------------------------------#
      dt <- for_ras(ras)
      
      # NUMBER OF UNIQUE RECORDS ------------------------------------------------#
      nur <- nrow(dt)
      
      # CLEAN DATA.TABLE --------------------------------------------------------#
      setnames(dt, c('Longitude', 'Latitude', 'Occurrence')) 
      dt[, c('Original_Records', 'SDM_Records') := .(nr, nur)]
      
      # ASSIGN TO LIST ----------------------------------------------------------#
      out[[i]] <- dt
      
    }
    
  }
  
  out <- rbindlist(out, idcol='Species')
  
  return(out)
  
}
#------------------------------------------------------------------------------#

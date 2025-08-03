######################################## Geometallurgical modelling ###########################################################

############################################ Block compositions ###########################################

# Conversion to mineral/lithic components: 

# Hematite = Fe2O3

# Apatite = Ca5(PO4)3(OH)

# Deleterious = MnO2 + Apatite

# Shale = Al2O3*2

# Quartz = SiO2 - 1/2*Shale

# Add these variables as columns to data-frame wind.compo as an exercise:
# Make auxiliary copy of wind.compo first:

wind.compo.aux = wind.compo

wind.compo.aux$Hm = wind.compo$Fe2O3
  
wind.compo.aux$Dl = wind.compo$MnO2 + wind.compo$P2O5/0.418

wind.compo.aux$Sh = wind.compo$Al2O3*2
  
wind.compo.aux$Qz = wind.compo$SiO2 - wind.compo$Al2O3

summary(wind.compo.aux$Qz)

summary(wind.compo.aux$Hm + wind.compo.aux$Dl + wind.compo.aux$Sh + wind.compo.aux$Qz)

# Need to correct negative Qz concentrations, as well as sums larger than 100 

wind.compo.aux$Qz[which(wind.compo.aux$Qz<0)] = -wind.compo.aux$Qz[which(wind.compo.aux$Qz<0)]

wind.compo.aux$sum = wind.compo.aux$Hm + wind.compo.aux$Dl + wind.compo.aux$Sh + wind.compo.aux$Qz

summary(wind.compo.aux$sum)

wind.compo.aux[which(wind.compo.aux$sum>1),] = wind.compo.aux[which(wind.compo.aux$sum>1),]/
  wind.compo.aux$sum[which(wind.compo.aux$sum>1)]


############################################## Block density #############################################

# Calculate assuming densities of 3.5 t/m3 for Hm + Dl; and 2.5 t/m3 for Sh and Qtz:

wind.compo.aux$rho = wind.compo.aux$sum/(wind.compo.aux$Hm/3.5 + wind.compo.aux$Dl/3.5 + 
                                           wind.compo.aux$Sh/2.5 + wind.compo.aux$Qz/2.5)

summary(wind.compo.aux$rho)

####################################### Processing of blocks ###########################################

# 1) Sell as lump (A) if Hm > 88% - entire mass

# 2) Deslime if Shale > 5% - mass modification: (1, 1, 1, 0.85) (Hm, Dl, Qz, Sh)

# 3) Separation always - mass modification: (0.7,0.7,0,0.9)

# 4) Product Fines (B) if Hm > 85% - entire mass
#    Product Fines (C) if Hm > 80% - entire
#    Waste otherwise

# Cf. Tolosana-Delgado et al. (2015)

# Need to write simple function for cost

Cost = function(x) {
    out = ifelse(x$Hm > 0.88,
          5*5*6*x$rho*70,
          ifelse(x$Sh > 0.05,
                  5*5*6*x$rho*(70+25)+5*5*6*0.05+5*5*6*x$rho*(1-0.15*x$Sh)*0.02,
                  5*5*6*x$rho*(70+25+0.02)
                )
            )

    }

summary(Cost(wind.compo.aux))

########################################### Block revenue ################################################

# Selling price for products: 

# Can write a function describing sale values of blocks now: 

Sale.Value = function(x) {
  out = ifelse(x$Hm > 0.88,
               5*5*6*x$rho*150,
               ifelse(x$Sh > 0.05,
                      ifelse(x$Hm*0.7/(1-(0.15+0.085)*x$Sh-0.3*x$Hm-0.3*x$Dl-x$Qz)>0.85,
                            140*5*5*6*x$rho*(1-(0.15+0.085)*x$Sh-0.3*x$Hm-0.3*x$Dl-x$Qz),
                            ifelse(x$Hm*0.7/(1-(0.15+0.085)*x$Sh-0.3*x$Hm-0.3*x$Dl-x$Qz)>0.80,
                                   130*5*5*6*x$rho*(1-(0.15+0.085)*x$Sh-0.3*x$Hm-0.3*x$Dl-x$Qz),
                                   0)),
                      ifelse(x$Hm*0.7/(1-(0.1)*x$Sh-0.3*x$Hm-0.3*x$Dl-x$Qz)>0.85,
                             140*5*5*6*x$rho*(1-0.1*x$Sh-0.3*x$Hm-0.3*x$Dl-x$Qz),
                             ifelse(x$Hm*0.7/(1-0.1*x$Sh-0.3*x$Hm-0.3*x$Dl-x$Qz)>0.80,
                                    130*5*5*6*x$rho*(1-0.1*x$Sh-0.3*x$Hm-0.3*x$Dl-x$Qz),
                                    0))
               )
  )

  }

summary(Sale.Value(wind.compo.aux))

# Revenue is simply Sale.Value minus Cost calculated earlier:

summary(Sale.Value(wind.compo.aux)-Cost(wind.compo.aux))

# Can now also write a function which tells us whether a block is ore or waste

Ore.Waste = function(x) {
  out = ifelse((Sale.Value(x)-Cost(x))>0,"O","W")

}

summary(as.factor(Ore.Waste(wind.compo.aux)))


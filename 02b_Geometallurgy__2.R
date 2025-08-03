######################################## Geometallurgical modelling ###########################################################

# You will need the article by Tolosana-Delgado et al. (2015) if you wish to understand more of the 
# reasoning here.

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

wind.compo.aux$Hm = 
  
wind.compo.aux$Dl = 

wind.compo.aux$Sh = 
  
wind.compo.aux$Qz = 

# Look at a summary of Qz:

# Check the sum of these components:



############################################## Block density #############################################

# Calculate this assuming densities of 3.5 t/m3 for Hm + Dl; and 2.5 t/m3 for Sh and Qtz:

wind.compo.aux$rho = 



####################################### Processing of blocks ###########################################

# 1) Sell as lump (A) if Hm > 88% - entire mass - Cost: $70/t (Extraction + Crushing)
#    Grind and mill otherwise - entire mass - Cost: $70/t + $25/t 

# 2) Deslime if Shale > 5% - mass modification: (1, 1, 1, 0.85) (Hm, Dl, Qz, Sh) - Cost: $0.05/m3

# 3) Separation always - mass modification: (0.7,0.7,0,0.9) - Cost: $0.02/t

# 4) Product Fines (B) if Hm > 85% - entire mass
#    Product Fines (C) if Hm > 80% - entire mass
#    Waste otherwise

# Cf. Tolosana-Delgado et al. (2015)

# Write a simple function for the processing cost (each block is 5*5*6 m3)

Cost = function(x) {}

########################################### Block revenue ################################################

# Selling price for products: A - $150/t, B - $140/t, C - $130/t 

# Now, write a simple function for the sales value of a block, incorporating 
# the rules above: 

Sale.Value = function(x) {}


# Revenue is simply Sale.Value minus Cost calculated earlier:

summary(Sale.Value(wind.compo.aux)-Cost(wind.compo.aux))

# You can now also write a function which tells us whether a block is ore or waste:

Ore.Waste = function(x) {}

  
summary(as.factor(Ore.Waste(wind.compo.aux)))


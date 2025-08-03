# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(data.table)
library(dplyr)

### Initialise code
apollo_initialise()

### Set core controls

number = 37

### Set core controls
apollo_control = list(
  modelName  = paste0("MNL_BS_MOIA_", number),
  modelDescr ="MNL-Modell fuer Bikesharing basierend auf MiD",
  indivID    ="HP_ID"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

setwd("//ifv-fs.ifv.kit.edu/Forschung/Projekte/MOIA HH/Work/Modelle/Besitzmodelle/MiD_Datenaufbereitung/")

database = read.csv("MiD_BS.csv", header = TRUE, sep = ";", dec = ",")
database = filter(database, bikesharing != 9 & age %in% c(4,5,6,7,8,9,10,11))
database <- rename(database,"verfueg" = "availability_bs")

setwd("//ifv-fs.ifv.kit.edu/Forschung/Projekte/MOIA HH/Work/Modelle/Besitzmodelle/MOIA_Bikesharing/MNL_models/")

dir.name = paste0("MNL_", number)
dir.create(dir.name)

setwd(paste0("//ifv-fs.ifv.kit.edu/Forschung/Projekte/MOIA HH/Work/Modelle/Besitzmodelle/MOIA_Bikesharing/MNL_models/", dir.name))



# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_bs           = 0,
                
                b_male           = 0,
                
                #b_age_4          = 0,
                b_age_5          = 0,
                b_age_6          = 0,
                b_age_7          = 0,
                #b_age_8          = 0,
                #b_age_9          = 0,
                #b_age_10         = 0,
                #b_age_11         = 0,
                
                b_taet_arbeit      = 0,
                #b_taet_teil      = 0,
                b_taet_stud      = 0,
                #b_taet_schul     = 0,
                #b_taet_ausbil    = 0,
                #b_taet_haus      = 0,
                b_taet_rent      = 0,
                #b_taet_bild      = 0,
                #b_taet_arblos    = 0
                
                b_eco_1_2          = 0,
                #b_eco_2          = 0,
                #b_eco_3          = 0,
                #b_eco_4          = 0,
                b_eco_5          = 0,
                
                #b_hhgro_1        = 0,
                #b_hhgro_2        = 0,
                #b_hhgro_3        = 0,
                #b_hhgro_4        = 0,
                b_hhgro_5        = 0,
                
                b_bedien          = 0,
                
                b_hhtype_1        = 0,
                #b_hhtype_2        = 0,
                #b_hhtype_3        = 0
                b_hhtype_4        = 0,
                
                #b_pkwhh_0        = 0,
                b_pkwhh_1        = 0,
                b_pkwhh_234        = 0,
                
                b_pkw_fs_0       = 0,
                
                b_carsharing     = 0,
                
                b_ticket         = 0
                
             )


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['bikesharing']]    =  asc_bs +
    
                         b_male * (gender == 1) +
  
                         #b_age_4 * (age == 4)   +
                         b_age_5 * (age == 5)   +
                         b_age_6 * (age == 6)   +
                         b_age_7 * (age == 7)   +
                         #b_age_8 * (age == 8)   +
                         #b_age_9 * (age == 9)   +
                         #b_age_10 * (age == 10)   +
                         #b_age_11 * (age == 11)   +
    
                         b_taet_arbeit * (employment %in% c(1,2))  +
                         #b_taet_teil * (employment == 2)   +
                         b_taet_stud * (employment == 5)    +
                         #b_taet_schul * (employment == 3)    +
                         #b_taet_ausbil * (employment == 4)    +
                         #b_taet_haus * (employment == 6)    +
                         b_taet_rent * (employment == 7) +
                         #b_taet_bild * (employment %in% c(3,4,5)) +
                         #b_taet_arblos * (employment == 8)    
    
                         b_eco_1_2 * (economicalStatus %in% c(1,2))  +
                         #b_eco_2 * (economicalStatus == 2)  +
                         #b_eco_3 * (economicalStatus == 3)  + 
                         #b_eco_4 * (economicalStatus == 4) +
                         b_eco_5 * (economicalStatus == 5) +
    
                         #b_hhgro_1 * (nominalSize == 1) +
                         #b_hhgro_2 * (nominalSize == 2) +
                         #b_hhgro_3 * (nominalSize == 3) +
                         #b_hhgro_4 * (nominalSize == 4) +
                         b_hhgro_5 * (nominalSize == 5) +
    
                         b_bedien * (verfueg == 1) +
    
                         b_hhtype_1 * (hhtype == 1) +
                         #b_hhtype_2 * (hhtype == 2) +
                         #b_hhtype_3 * (hhtype == 3)
                         b_hhtype_4 * (hhtype == 4) +
    
                         #b_pkwhh_0 * (totalNumberOfCars == 0) +
                         b_pkwhh_1 * (totalNumberOfCars == 1) +
                         b_pkwhh_234 * (totalNumberOfCars %in% c(2,3,4)) +
    
                         b_pkw_fs_0 * (hasLicense_car == 0) +
    
                         b_carsharing * (mobilityProviderCustomership == 1) +
    
                         b_ticket * (hasCommuterTicket == 1)
  


  V[['nobikesharing']]  =  0
  
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(bikesharing=1, nobikesharing=0),
    avail        = list(bikesharing= 1, nobikesharing=1),
    choiceVar     = bikesharing,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt", sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- LR TEST AGAINST SIMPLE MNL MODEL                           ----
# ----------------------------------------------------------------- #
setwd("//ifv-fs.ifv.kit.edu/Forschung/Projekte/MOIA HH/Work/Modelle/Besitzmodelle/MOIA_Zeitkarte_Besitzmodell/")

apollo_lrTest(model, "MNL_BS_MOIA_18")

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()
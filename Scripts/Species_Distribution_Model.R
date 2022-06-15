# GJAM - Species Distribution model


# Tutorial https://cran.r-project.org/web/packages/gjam/vignettes/gjamVignette.html

#-----------------------------------------------------------------------------
library(gjam)


# THEORETICAL APPROACH: gjamSimData => with stochastic data, do not fit for personal data
#---------------------------------------------------------------------------------------
 
# gjamSimData is actually a simulation to "check that the algorithm can recover true parameters values and predict data"
# This is not personal data, you just simulate stochastic data and predictors to see the model behavior
# n= number of samples (These are total Sites * Depths * Quadrats)
# Q: environmental descriptors --> (pred)
# S: number of species --> (resp)
# typeNames: 'CA' continous abundances 

f    <- gjamSimData(n=2880,Q=3, S = 13, typeNames = 'CA')

summary(f)
f$formula
f$xdata
f$ydata
f$trueValues

par(bty = 'n', mfrow = c(1,2), family='')
h <- hist(c(-1,f$y),nclass = 50,plot = F)
plot(h$counts,h$mids,type = 's')
plot(f$w,f$y,cex = .2)

# Gibbs sampler to estimate parameters and fits the data (theoretical too, can be applied from the gjamSimData object previously created)
# ng: number of Gibbs steps 
# needs the formula for the model
#       the xdata data.frame which includes the predictors 
#       the ydata (response matrix)
#       infos for model : ng, burnin and typeNames

ml   <- list(ng = 100, burnin = 10, typeNames = f$typeNames) #  list for the model
out  <- gjam(f$formula, f$xdata, f$ydata, modelList = ml) # run the Gibbs sampler
summary(out)
out$inputs$classBySpec
out$parameters$betaMu
out$parameters$betaTable 
out$parameters$sensBeta
out$prediction$xpredMu

# Plot output of simulation data
pl  <- list(trueValues = f$trueValues, GRIDPLOTS = T)
gjamPlot(output=out, plotPars = pl)
# click on enter to validate and pursue the process 
# Plot observations and estimations of simulated data


                      #............................
                      # NOW WORK WITH PERSONAL DATA
                      #............................

# load data
#---------
# Response matrix
resp <- read.csv(file = "Data/resp_data_living_benthos.csv", header = T, dec = ".", sep = ",", row.names = 1)
head(resp)


# Environmental predictors data
pred <- read.csv(file = "Data/env_data.csv", header = T, dec = ".", sep = ",", row.names = 1)
head(pred)
# summary(pred)



# change colnames from resp because otherwise in the text it becomes very difficult to read
colnames(resp)[1] <- "Branching.coral"
colnames(resp)[2] <- "Encrusting.coral"
colnames(resp)[3] <- "Massive.coral"
colnames(resp)[4] <- "Laminar.coral"
colnames(resp)[5] <- "Solitary.coral"
colnames(resp)[6] <- "Gorgonia"
colnames(resp)[7] <- "Antipatharia"
colnames(resp)[8] <- "Hydroids"
colnames(resp)[9] <- "CCA"
colnames(resp)[10] <- "Other.Cnidaria" # Sinularia inside
colnames(resp)[11] <- "Sponges"
colnames(resp)[12] <- "Fleshy.macroalgae" 
colnames(resp)[13] <- "Crustose.algae" 
colnames(resp)[14] <- "Halimeda" 
colnames(resp)[15] <- "Others" # Mobile animals




# As mentioned before, we are deleting others because otherwise this variable decreases the model accuracy
# Others consists of - mobile invertebrates, human trace, non-identified  points, etc. 
# We want to focus only on benthic categories
resp <- subset( resp, select = -Others )
# We are also deleting Other.Cnidaria because it consist from multiple cnidaria from shallow sinularia to other deeper non-identified cnidaria
# Results do not make sense
resp <- subset( resp, select = -Other.Cnidaria )


# Set categories numeric or factor columns of pred.
pred$Depth <- as.numeric (pred$Depth)
pred$Depth <- pred$Depth * (-1) # To consider depth in the right direction
pred$Geomorphology <- as.factor (pred$Geomorphology)
pred$Bathymetry_slope <- as.factor (pred$Bathymetry_slope)
pred$Fixed.substrate <- as.numeric (pred$Fixed.substrate)
pred$Sediment.substrate <- as.numeric (pred$Sediment.substrate)
pred$Rubble <- as.numeric (pred$Rubble)

pred$Light_Rel_Ind <- as.numeric (pred$Light_Rel_Ind)
pred$Temp_Variability <- as.numeric (pred$Temp_Variability)
pred$Temp_Rel_Ind <- as.numeric (pred$Temp_Rel_Ind)

pred$Coast_Orientation <- as.factor (pred$Coast_Orientation)

pred$Latitude <- as.numeric (pred$Latitude)
pred$Latitude <- pred$Latitude*(-1)
pred$Longitude <- as.numeric (pred$Longitude)
pred$Longitude <- pred$Longitude*(-1)

pred$Dominant.Substrate <- as.factor (pred$Dominant.Substrate)
pred$Dominant.benthic.non_coral <- as.factor (pred$Dominant.benthic.non_coral)
# Some variables such as SST_Satellite_mean, Coast orientation, Latitude, Longitude are finally not used in the model because they are constant with depth




# Extract islands and sites
pred$Island_Site <- data.frame(do.call("rbind",strsplit(sub("\\."," ",rownames (pred)),"_")))[2]

      ########## MODELING ###########
                                          
# Final model formula and variables - This was considered after testing different combinations of formula and environmental data input. 
# Inside the formula we also tested Depth interactions (:) with other environmental data. 

      #### Final selected MODEL #### 
# After a lot of testing, the best is:  ~ Depth + Slope + Substrate 
# It converged and had the highest accuracy and sense in the results
# With other environmental variables such as Light, Temp_Ind, Temp_Var, Geomorphology, Dominant.Benthic.non_coral, bleaching data the model does not converge
# For example, light and depth together does not make sense because colinearity. I deleted light. 

      #### Final selected MODEL ####

# modelList
ml   <- list(ng = 2500, burnin = 500, typeNames = 'CA') # Gibbs sampler to control the choice of ng and burnin...
# With different ng = 5000 and burnin 2000 it does not change

# launch the model
# out  <- gjam( as.formula( ~  Depth + Bathymetry_slope + Dominant.Substrate), xdata = pred, ydata = resp, modelList = ml) 
# save(out, file="Data/Converged_models/gjam_sdm_Depth_Slope_Substrate.RData")
load("Data/Converged_models/gjam_sdm_Depth_Slope_Substrate.RData") 






# select the colors 
specNames <- colnames(resp)

specColor <- rep('black',ncol(resp))
specColor[ grep('Branching.coral',specNames) ] <- 'blue'
specColor[ grep('Encrusting.coral',specNames) ] <- 'blue'
specColor[ grep('Massive.coral',specNames) ] <- 'blue'
specColor[ grep('Laminar.coral',specNames) ] <- 'blue'
specColor[ grep('Solitary.coral',specNames) ] <- 'blue'
# specColor[ grep('Scleractinian',specNames) ] <- 'blue'

specColor[ grep('Halimeda',specNames) ] <- 'black'
specColor[ grep('Fleshy.macroalgae',specNames) ] <- 'black'
specColor[ grep('Crustose.algae',specNames) ] <- 'black'

specColor[ grep('Gorgonia',specNames) ] <- 'black'
specColor[ grep('Hydroids',specNames) ] <- 'black'
specColor[ grep('CCA',specNames) ] <- 'black'
specColor[ grep('Sponges',specNames) ] <- 'black'
specColor[ grep('Antipatharia',specNames) ] <- 'black'




# In the working directory, a folder called "gjamOutput" will be created

pl   <- list(GRIDPLOTS=T, specColor = specColor, SAVEPLOTS=T) # results will be stored as pdf, not printed on screen 

# details on outputs are explained in the "plotting output" section of https://cran.r-project.org/web/packages/gjam/vignettes/gjamVignette.html

gjamPlot(output = out, plotPars = pl)


          #### OUTPUTS #####
# I changed the name of the folder to : "gjamOutput_Depth_Slope_Substrate"
# clusterGridB was simplified a posteriori with AI to create Fig. 4
# clusterDataE is supplementary figure 6
# betaDepth was supplemtnary figure 7b

          #### OUTPUTS #####

# Extra info contained in the plot actually, that you can read/store (the info that were used by R to make the plots)
extra_out <- gjamPlot(output = out, plotPars = pl)
extra_out$clusterIndex #cluster index for responses in grid/cluster plots
extra_out$clusterOrder #order for responses in grid/cluster plots.
extra_out$ematrix #S X S response correlation matrix for E.
extra_out$fit



###### Extra model without considering scleractinian forms and using only Scleractinia cover #######

resp2 <- read.csv(file = "Data/resp_data_living_benthos_Scleractinia_noform.csv", header = T, dec = ".", sep = ",", row.names = 1)

# With no form:
colnames(resp2)[1] <- "Scleractinian"
colnames(resp2)[2] <- "Gorgonia"
colnames(resp2)[3] <- "Antipatharia"
colnames(resp2)[4] <- "Hydroids"
colnames(resp2)[5] <- "Other.Cnidaria" # Sinularia inside
colnames(resp2)[6] <- "Sponges"
colnames(resp2)[7] <- "Fleshy.macroalgae"
colnames(resp2)[8] <- "Crustose.algae"
colnames(resp2)[9] <- "Halimeda"
colnames(resp2)[10] <- "Others"
colnames(resp2)[11] <- "CCA"

resp2 <- subset( resp2, select = -Others )
resp2 <- subset( resp2, select = -Other.Cnidaria )


### Launch the model using no coral forms, only Scleractinia
# out2  <- gjam( as.formula( ~  Depth + Bathymetry_slope + Dominant.Substrate), xdata = pred, ydata = resp2, modelList = ml) 
# save(out, file="gjam_sdm_Depth_Slope_Substrate_sclerctinia_cover_noforms.RData")
load("Data/Converged_models/gjam_sdm_Depth_Slope_Substrate_sclerctinia_cover_noforms.RData") 
# Folder: "gjamOutput_Scleractinian_Cover_Noforms_Depth_Slope_Subs"
### Also possible



# With scleractinian cover and No forms
specNames <- colnames(resp2)
specColor <- rep('black',ncol(resp2))
specColor[ grep('Scleractinian',specNames) ] <- 'blue'
specColor[ grep('Halimeda',specNames) ] <- 'black'
specColor[ grep('Fleshy.macroalgae',specNames) ] <- 'black'
specColor[ grep('Crustose.algae',specNames) ] <- 'black'
specColor[ grep('Gorgonia',specNames) ] <- 'black'
specColor[ grep('Hydroids',specNames) ] <- 'black'
specColor[ grep('CCA',specNames) ] <- 'black'
specColor[ grep('Sponges',specNames) ] <- 'black'
specColor[ grep('Antipatharia',specNames) ] <- 'black'




# In the working directory, a folder called "gjamOutput" will be created

pl   <- list(GRIDPLOTS=T, specColor = specColor, SAVEPLOTS=T) # results will be stored as pdf, not printed on screen 

# details on outputs are explained in the "plotting output" section of https://cran.r-project.org/web/packages/gjam/vignettes/gjamVignette.html

gjamPlot(output = out, plotPars = pl)


            #### OUTPUTS model of scleractinian cover (no forms) #####
# I changed the name of the folder to : "gjamOutput_Scleractinian_Cover_Noforms_Depth_Slope_Subs"
# betaDepth was supplementary figure 7a to compare differences between modeling Scleractinian cover alone or differentiating by sclerctinian morphologies

            #### OUTPUTS model of scleractinian cover (no forms)  #####

# Extra info contained in the plot actually, that you can read/store (the info that were used by R to make the plots)
extra_out <- gjamPlot(output = out, plotPars = pl)
extra_out$clusterIndex #cluster index for responses in grid/cluster plots
extra_out$clusterOrder #order for responses in grid/cluster plots.
extra_out$ematrix #S X S response correlation matrix for E.
extra_out$fit

# The results from the gjamOutput folder with plots were worked a posteriori with AI to simplify the interpretation. Specially the clusterGridB becoming Fig. 4







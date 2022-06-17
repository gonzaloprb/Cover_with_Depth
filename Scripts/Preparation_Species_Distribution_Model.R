

library (dplyr); library(tidyverse); require (plyr); require (reshape2); library (FactoMineR); library (RColorBrewer); library (randomcoloR)
rm (list = ls ())


# Open database - here we are working with Scleractinian_form instead of with the different genera
data <- read.csv(file = "Data/rndpts_genus_form.csv", header = T, dec = ".", sep = ";", row.names = 1)

# Set the categories of Scleractinian corals to Scleractinian_form (dropping differences in genus)
data <- data %>% unite(., col = "Category",  Category, Coral_form, na.rm=TRUE, sep = "_")

# Summary by new categories
summary <- ddply(data, ~ Site + Archipelago ~ Island +Island_Site + Depth +  Category, function(x){c(Cover = mean(x$Cover)) })

# Keep only benthic categories (abiotic, not alive)
remove <- c("Consolidate_substrate","Sediment_substrate","Rubble", "Sand",  "Dead Coral")
summary = filter(summary, !(Category %in% c("Consolidate_substrate","Sediment_substrate","Rubble", "Sand",  "Dead Coral", "Dead Coral_Turf")))


##### Transform final categories of benthic data #####
# These are the category names we want (the ones described in Methods and displayed in Fig. 4) 
# unique (data$Category)

# Coral - keep only Scleractinian_form
# Added together Scleractinian table and branching as branching
data$Category <- gsub("Scleractinian_branching|Scleractinian_table", "Scleractinian_branching", data$Category)


# Millepora considered as Scleractinian for its similar ecological role (see manuscript)
data$Category <- gsub("Scleractinian_massive|Millepora", "Scleractinian_massive", data$Category)


# All non_Scleractinian
# Keeping separately: Gorgonia, Black_coral, Hydroids, Millepora, "Sinularia"
data$Category <- gsub("Stylaster|Other_soft_corals|Sessile_Invertebrates|Sinularia", "Cnidaria (non Scleractinian)", data$Category)


# Sponges - All Clionidae, massive and encrusting sponges together
data$Category <- gsub("Clionidae|Encrusting sponges|Massive sponges|Sponges|Encrousting sponges", "Sponges", data$Category)

# Macroalgae - Halimeda, fleshy, Ulva - Considered as (+) Calcifying and (-) competition space
# Macroalgae - Brown, Turbinaria, etc (-) Competition 
# Finally decided to make two algae categories: Crustose algae and Fleshy macroalgae
data$Category <- gsub("Green_Algae encrusting|Brown_algae encrusting|Green_Sediment_Algae filamentous", "Crustose algae", data$Category)
data$Category <- gsub("Red_Algae fleshy|Brown_Algae fleshy|Turbinaria|Macroalgae|Ulva|Green_Algae fleshy", "Fleshy macroalgae", data$Category)

# CCA - kept separately
data$Category <- gsub("Calcifying_algae|Dead Coral_CCA|Rublle_CCA|Rubble CCA|Fixed substrate_CCA|Calcifying_algae", "CCA", data$Category)


# Others - Mobile animals (Molluscs,Fish), Human Trace and non-identified points put togehter as Others
# Since it becomes important in some of the plots, we decided to remove this category as it is not representative
data$Category <- gsub("COTs|Worms|Molluscs|Mobile_invertebrates|Fish|Human_trace|Unidentifiable|Unassigned", "Others", data$Category)


##### Transform final categories of benthic data #####


#### Extra necessary information from non scleractinian categories ####
# results added to Bathymetric_Coord_Info
# Quantitative results of the non slceractinian benthic categories for the environmental modeling. In the END, these were not used so I could delete

Non_scler <-  c ("Cnidaria (non Scleractinian)", "Sponges", "Macroalgae", "Others", "CCA")
benthic_Non_coral <- data[data$Category %in% Non_scler, ]

Dominant_benthic_non_coral <- ddply(benthic_Non_coral, ~ Site + Archipelago ~ Island +Island_Site + Depth  + Category, function(x){c(Cover = mean(x$Cover)) })

Dominant_benthic_non_coral_summary <- ddply(Dominant_benthic_non_coral,  ~  Depth  + Category, function(x){c(Cover = mean(x$Cover)) })

#### Extra necessary information from non scleractinian categories ####



####### Transform final categories of benthic environmental data ##############
# Aggregation of substrate categories. These will be considered quantitatively as environmental data
# Fixed substrate - Uncolonised substrate. We decided to keep here Turf and Cyanobacteria. Competing with corals for available substrate
data$Category <- gsub("Consolidate_substrate|Dead Coral_Turf|Dead Coral|Turf|Cyanobacteria", "Fixed substrate", data$Category)
# Separating turf from the list - it is a problem because sometime I did not differentiate turf from substrate

# Sediment substrate - Sand, sediment, gravel sand. 
# Here Sediment substrate CCA was considered as sediment because the CCA were largely covered by sediment
data$Category <- gsub("Sediment_substrate|Sand|Gravel_sand|Sediment substrate CCA", "Sediment substrate", data$Category)

# Rubble 
data$Category <- gsub("Rubble_CCA|Rubble turf|Rubble", "Rubble", data$Category)
####### Transform final categories of benthic environmental data ##############



# Check of final categories for the species distribution model
unique (data$Category)

######
# This is an extra and necessary database for the species distribution model creating a category of all scleractinian together (no forms)
data2 <- data
data2$Category <- gsub("Scleractinian_laminar|Scleractinian_massive|Scleractinian_branching|Scleractinian_encrusting|Scleractinian_solitary|Scleractinian_table|Millepora", "Scleractinian", data2$Category)
######
unique (data2$Category)


# Delete unnecessary columns, data and values 
data <- ddply(data, ~ Site + Archipelago ~ Island +Island_Site + Depth + Quadrat + Category, function(x){c(Cover = sum(x$Cover)) })
data2 <- ddply(data2, ~ Site + Archipelago ~ Island +Island_Site + Depth + Quadrat + Category, function(x){c(Cover = sum(x$Cover)) })


#### Benthic substrate data ####

# Take out benthic substrate environmental from the database and save environment
keep <-  c ("Fixed substrate", "Sediment substrate", "Rubble")
substrate_env <- data[data$Category %in% keep, ]

Dominant_Substrate <- ddply(substrate_env, ~ Site + Archipelago ~ Island +Island_Site + Depth  + Category, function(x){c(Cover = mean(x$Cover)) })
#### Benthic substrate data ####
# These will be considered quantitatively as environmental data for the modeling categories according to dominant substrate. Information added to Bathymetric_Coord_Info



melt_substrate_env = melt(substrate_env, id=c("Depth","Site", "Quadrat","Category"), measure.vars="Cover", na.rm=FALSE)

# Set categories in the order we want 
melt_substrate_env$Category <- factor(melt_substrate_env$Category, levels = c("Fixed substrate","Sediment substrate","Rubble"))
benthic_substrate_data = dcast(melt_substrate_env, Depth + Site + Quadrat ~ Category, mean, add.missing = T)

# Missing values to 0
benthic_substrate_data[is.na(benthic_substrate_data)] <- 0

# complete to have all quadrats with all categories although empty 
benthic_substrate_data <- benthic_substrate_data %>% complete( Depth,Site, Quadrat,fill = list(Cover = 0))


benthic_substrate_data$ID<- with(benthic_substrate_data, paste0(Depth, sep = "_",  Site, sep = "_",Quadrat))
benthic_substrate_data <- as.matrix (benthic_substrate_data)
row.names(benthic_substrate_data) <- benthic_substrate_data[,"ID"]


benthic_substrate_data <- benthic_substrate_data[,-c(1,2,3,7)]

class(benthic_substrate_data) <- "numeric" 

# Unnecessary because later you create a database of all environmental data together
# write.csv (benthic_substrate_data,  "Data/benthic_substrate_data.csv")



####################################################################################
# Save all benthos data - also considering substrate
write.csv (data,  "Data/allbenthosdata.csv") # I think this can be delete it

# Drop non-living benthic categories from data 
data <- subset(data,!Category %in% keep)
# Save dataframe for Depth distribution of categories - Sup. Fig. 5
write.csv (data,  "Data/Living_Benthic_Categories_Data.csv")


### Response dataframe 

# Resume of living categories Max, min, mean
ddply(data, ~ Site + Depth + Category, function(x){c(Mean = mean(na.omit (x$Cover)),Max = max(na.omit (x$Cover)), Min = min(na.omit (x$Cover)))})


# Sett Category with the ones you want to keep

melt_data = melt(data, id=c("Depth","Site", "Quadrat","Category"), measure.vars="Cover", na.rm=FALSE)

# Put categories in the order we want 
# With the final living benthic categories we want to keep!
melt_data$Category <- factor(melt_data$Category, levels = c("Scleractinian_branching","Scleractinian_encrusting","Scleractinian_massive","Scleractinian_laminar","Scleractinian_solitary","Scleractinian_table",
                                                              "Gorgonia", "Black_coral","Hydroids","Sinularia","CCA",
                                                              "Cnidaria (non Scleractinian)","Sponges","Fleshy macroalgae","Crustose algae","Halimeda","Others" ))


resp_data = dcast(melt_data, Depth + Site + Quadrat ~ Category, mean, add.missing = T)

# Missing values to 0
resp_data[is.na(resp_data)] <- 0

# complete to have all quadrats with all categories although empty 
resp_data <- resp_data %>% complete( Depth,Site, Quadrat,fill = list(Cover = 0))


resp_data$ID<- with(resp_data, paste0(Depth, sep = "_",  Site, sep = "_",Quadrat))
resp_data <- as.matrix (resp_data)
row.names(resp_data) <- resp_data[,"ID"]

# Keeping matrix for the modeling
resp_data <- resp_data[,-c(1,2,3,19)]

class(resp_data) <- "numeric"

# Save datafram of response data

write.csv (resp_data,  "Data/resp_data_living_benthos.csv")


###### database with non scleractinian forms ######
# Drop non-living benthic categories from data2
data2 <- subset(data2,!Category %in% keep)

# No keeping form - Only scleractinian
melt_data2 = melt(data2, id=c("Depth","Site", "Quadrat","Category"), measure.vars="Cover", na.rm=FALSE)

melt_data2$Category <- factor(melt_data2$Category, levels = c("Scleractinian","Gorgonia", "Black_coral","Hydroids","Cnidaria (non Scleractinian)",
                                                              "Sponges","Fleshy macroalgae","Crustose algae","Halimeda","Others", "CCA"))


resp_data2 = dcast(melt_data2, Depth + Site + Quadrat ~ Category, mean, add.missing = T)

# Missing values to 0
resp_data2[is.na(resp_data2)] <- 0

# complete to have all quadrats with all categories although empty 
resp_data2 <- resp_data2 %>% complete( Depth,Site, Quadrat,fill = list(Cover = 0))


resp_data2$ID<- with(resp_data2, paste0(Depth, sep = "_",  Site, sep = "_",Quadrat))
resp_data2 <- as.matrix (resp_data2)
row.names(resp_data2) <- resp_data2[,"ID"]

# For non-coral forms (only Scleractinian)
resp_data2 <- resp_data2[,-c(1,2,3,15)]
class(resp_data2) <- "numeric"

write.csv (resp_data2,  "Data/resp_data_living_benthos_Scleractinia_noform.csv")

###### database with non scleractinian forms ######
# This database will also be used for the species distribution model


###################################################################################################################

#### Extra Environmental data 
# and combine dataframe with benthic data. 
Extra_env_data <- read.csv("Data/Environment/Bathymetry_Coord_info.csv", sep=";")


# Add the thirty photoquadrats per site and depth. You need the same dimensions as benthic_substrate_data and resp_data
Extra_env_data <- dplyr::arrange(merge(Extra_env_data,list(Quadrat =1:30)),Site, Depth)


# Set rownames
Extra_env_data$ID<- with(Extra_env_data, paste0(Depth, sep = "_",  Site, sep = "_",Quadrat))

# Set class of categories
cols.num <- c("Depth","Latitude","Longitude")
Extra_env_data[cols.num] <- sapply(Extra_env_data[cols.num],as.numeric)

Extra_env_data <- as.matrix (Extra_env_data)
row.names(Extra_env_data) <- Extra_env_data[,"ID"]

# Keep only the columns we want

# First info data:
Info_data <- Extra_env_data[,c(3,4,5)]

class(Info_data) <- "numeric"

# Unnecessary because later you create a database of all environmental data togehter
# write.csv (Info_data,  "Data/Info_data.csv")


# Create second, extra env data with additional variables: geomorphology, coast orientation, bathymetry slope, major biotic and abiotic, dominant substrate, dominant benthic non coral and richness
Extra_Env_Info_data <- Extra_env_data[,c(6,7,8,9, 10, 11,12)] 

# Unnecessary because later you create a database of all environmental data together
# write.csv (Extra_Env_Info_data,  "Data/Extra_Env_Info_data.csv")

###################################################################################################################

#### Environmental data from loggers and CTD
Loggers_data <- read.csv("Data/Environment/Loggers_CTD_Sat.csv", sep=";")


# Measure Temp_Rel_Ind at each depth, according to SST and CTD_Temp_Rel loose of temperature with depth! 
Loggers_data$Temp_Rel_Ind <- (Loggers_data$SST_Satellite_mean * Loggers_data$CTD_Temp_Rel_6 )/100

# Add the thirty photoquadrats per site and depth. You need the same dimensions as benthic_substrate_data and resp_data
Loggers_data <- dplyr::arrange(merge(Loggers_data,list(Quadrat =1:30)),Site, Depth)

# Set rownames
Loggers_data$ID<- with(Loggers_data, paste0(Depth, sep = "_",  Site, sep = "_",Quadrat))

# Set class of categories
cols.num <- c("Depth","Light_Log","Light_Rel_Ind", "Temp_Hoboo", "Temp_Variability","CTD_PAR_Rel_6", "CTD_Temp_Rel_6", "SST_Satellite_mean","Temp_Rel_Ind")
Loggers_data[cols.num] <- sapply(Loggers_data[cols.num],as.numeric)

Loggers_data <- as.matrix (Loggers_data)
row.names(Loggers_data) <- Loggers_data[,"ID"]

# Keep only the columns we want, dropping almost all except Light_Log, Light_Rel_Ind Temp_Log,  SST_Satellite_mean, and Temp_Variability
Loggers_data <- Loggers_data[,c(4,5,6,7,12,16)]

class(Loggers_data) <- "numeric"

# Unnecessary because later you create a database of all environmental data together
# write.csv (Loggers_data,  "Data/Loggers_data.csv")


#### Adding extra Bleaching data 
# and combine dataframe with benthic data. 
Extra_bleaching <- read.csv("Data/Environment/Bleaching_Info.csv", sep=";")

# Add the thirty photoquadrats per site and depth. You need the same dimensions as benthic_substrate_data and resp_data
Extra_bleaching <- dplyr::arrange(merge(Extra_bleaching,list(Quadrat =1:30)),Site, Depth)


# Set rownames
Extra_bleaching$ID<- with(Extra_bleaching, paste0(Depth, sep = "_",  Site, sep = "_",Quadrat))

# Set class of categories
cols.num <- c("Depth","Years_Since_Last_bleaching","DHW_Last_Bleaching_year","Bleaching_events_since_2015", "Sum_DHWs_since_2015")
Extra_bleaching[cols.num] <- sapply(Extra_bleaching[cols.num],as.numeric)

Extra_bleaching <- as.matrix (Extra_bleaching)
row.names(Extra_bleaching) <- Extra_bleaching[,"ID"]

# Keep only the columns we want Years_Since_Last_bleaching,DHW_Last_Bleaching_year,Bleaching_events_since_2015, Sum_DHWs_since_2015
# First info data:
Bleaching_data <- Extra_bleaching[,c(4,5,6,7)]

class(Info_data) <- "numeric"


# Unnecessary because later you create a database of all environmental data together
# write.csv (Bleaching_data,  "Data/Bleaching_data.csv")




# Create final environmental predictor data combining dataframes of all environment data with benthic substrate data
env_data <- cbind (Info_data,benthic_substrate_data, Loggers_data,Extra_Env_Info_data,Bleaching_data) # Missing Extra_env_data because character!

write.csv (env_data,  "Data/env_data.csv")


# Data is ready. Go to Script of Species_Distribution_Model or to Script Visual_PCA_Environment_Scleractinian


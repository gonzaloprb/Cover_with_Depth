

# Departing question: 
# How is the relationship of coral cover with depth?
# What environment and substrate make high coral cover possible (> than expected) at lower mesophotic depths?

# Use a methodology similar to Cinner et al. 2016 

# Necessary to run a model across depths and identify the outliers of unexpected high coral cover and low coral cover


# The initial idea was to model these outliers only. However, the big variability caused not conversion (after a lot of testing)
# The environmental data and depth did not manage to explain the deviations (outliers) with the null and full model!
# Instead we have modeled coral cover according to depth and according to the interaction  between  environmental variables with depth
# These models are in Script: "Bayesian_models.R"

# THE IDEA NOW IS TO UNDERSTAND WHAT ENVIRONMENTAL CONDITIONS FAVOUR CORAL COVER RATHER THAN MODELLING THE OUTLIERS. 
# These data is standardised, and transformed to apply a binomial distribution

#### This script generates Fig 1 b, Sup. Fig. 2 and the databases for the Bayesian modelling

# Please go straight to the script "Bayesian_models.R"



rm (list = ls())

# Install.packages
library(tidyverse); require (plyr); require (reshape2); library (data.table); require  (stats); require (matrixStats); library (stringr)

# Plot packages
library (patchwork); library (scales); library(RColorBrewer);  library(randomcoloR); library(viridisLite); 
library (cowplot); library(ggridges)

# Model plots 
library (car); library (glmm); library (glmnet); library (glmmTMB); library (lme4); 

# Correlation packages
library(ggcorrplot)





# Open the data

data <- read.csv(file = "Data/rndpts_genus_form.csv", header = T, dec = ".", sep = ";", row.names = 1)

# Necessary to change name of one island
data$Island <- gsub("Mangareva", "Gambier", data$Island)

# Create a database to keep all quadrats with only corals with coral genus
rndpts_df <- data
rndpts_df <- filter(rndpts_df, Category == "Scleractinian") 
rndpts_df <- rndpts_df %>% separate(Coral_Genus_Form,  c("Coral_genus", "Coral_form"), " ", extra = "merge")
rndpts_df <- aggregate (Cover ~ Unique_Image_ID + Date + Site + Archipelago + Island + Island_Site + Depth + Quadrat + Category +  Coral_genus, rndpts_df , sum)


# Prepare different databases, per genus, site, depth
rndpts_df <- rndpts_df %>% complete( Island,Island_Site,Depth,Quadrat,fill = list(Cover = 0))
genus_cover_site <- ddply(rndpts_df, ~ Archipelago + Island +Island_Site + Depth + Category + Coral_genus, function(x){c(Cover = sum(x$Cover)/30) })

coral_cover_site  <- ddply(genus_cover_site, ~ Island +Island_Site + Depth , function(x){c(Cover = sum(x$Cover), Sd = sd(x$Cover), se=sd(x$Cover) / sqrt(length(x$Cover))) })
coral_cover_site[is.na(coral_cover_site)] <- 0

coral_cover_Island_Site  <- coral_cover_site %>% unite(Island_Island_Site, Island, Island_Site, remove = T)
coral_cover_Island_Site[is.na(coral_cover_Island_Site)] <- 0

coral_cover_depth  <- ddply(coral_cover_Island_Site, ~  Depth , function(x){c(Cover_Depth = mean(x$Cover), Sd_Depth = sd(x$Cover), se_Depth=sd(x$Cover) / sqrt(length(x$Cover))) })


# Plot each site and means to see the effect of depth and outliers
coral_cover_Island_Site <- merge (coral_cover_Island_Site,coral_cover_depth)
# Remake depth as a numeric variable
coral_cover_Island_Site$Depth <- as.numeric (as.character(coral_cover_Island_Site$Depth))

# Plot with standard deviation, loess to have a good impression of the outliers, spatial differences and general pattern! 

ggplot(coral_cover_Island_Site, aes(x=Depth, y=Cover)) + 
  geom_point(aes (colour = Island_Island_Site),shape = 21, size = 1.5) + 
  geom_smooth(span = 0.5, method = "loess", size = 2, colour = "grey") +
  geom_errorbar(aes(ymin = Cover_Depth - se_Depth, ymax = Cover_Depth + se_Depth), color="black", size=1, width=2) +
  geom_line(aes(y=Cover_Depth + (Sd_Depth)), size=1, linetype="dotted",alpha = 0.8, col = "blue") +
  geom_line(aes(y=Cover_Depth - (Sd_Depth)), size=1, linetype="dotted",alpha = 0.8, col = "blue") +
  geom_point(aes(y=Cover_Depth), shape=21, fill="white", size=4) +  
  scale_x_continuous(name ="Depth (m)", limits=c(5,122), breaks = c(6,20,40,60,90,120)) +
  scale_y_continuous(name ="Coral cover (%)", limits=c(-10,100), breaks = c(0,20,40,60,80,100)) +
  theme_bw()  + ylab ("Coral cover (%)") + xlab ("Depth (m)") +
  theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                          axis.text = element_text(size=10, colour="black"),
                          axis.title = element_text(size=11, face="bold", colour="black")) # Ignore warnings!

# This graph should be made with the expected distribution of a model and not from the mean and deviations only

#### 
# Coral cover profiles with depth according to sites. 
### (It represents to Fig. 1 b) ###

# Unite Island and Island_Site
coral_cover_site <- coral_cover_site %>% unite(Island_Island_Site, Island, Island_Site, remove = T)

# Transform NA because of 0 % coral cover by 0 
coral_cover_site[is.na(coral_cover_site)] <- 0

# Complete to have all sites although 0% coral cover
coral_cover_site <- coral_cover_site %>% complete( Depth,Island_Island_Site,fill = list(Cover = 0, Sd=0,Cover_se=0))

# Changes Island_Island_Site names to be consistent upon other plots
coral_cover_site$Island_Island_Site <- gsub('Moorea_1', 'Moorea S1', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Moorea_2', 'Moorea S2', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Tahiti_1', 'Tahiti S1', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Tahiti_2', 'Tahiti S2', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Bora_1', 'Bora S1', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Bora_2', 'Bora S2', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Tikehau_1', 'Tikehau S1', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Tikehau_2', 'Tikehau S2', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Rangiroa_1', 'Rangiroa S1', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Rangiroa_2', 'Rangiroa S2', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Raroia_1', 'Raroia S1', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Raroia_2', 'Raroia S2', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Makatea_1', 'Makatea S1', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Makatea_2', 'Makatea S2', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Gambier_1', 'Gambier S1', coral_cover_site$Island_Island_Site)
coral_cover_site$Island_Island_Site <- gsub('Gambier_2', 'Gambier S2', coral_cover_site$Island_Island_Site)

# Se the order of sites
coral_cover_site$Island_Island_Site <- factor(coral_cover_site$Island_Island_Site, levels = c("Moorea S1","Moorea S2","Tahiti S1","Tahiti S2","Bora S1","Bora S2","Tikehau S1","Tikehau S2","Rangiroa S1","Rangiroa S2","Raroia S1","Raroia S2","Makatea S1","Makatea S2","Gambier S1","Gambier S2"))

# To display with different colors according to categories
coral_cover_site$col <- cut(coral_cover_site$Cover, c(0, 25, 50, 100))

Cover_with_depth <- ggplot (coral_cover_site, aes (x = Depth, y = Cover, color = col)) + 
  geom_point (size = 5, alpha = 0.8) + geom_line (color = "black") + 
  geom_errorbar(aes(ymin = Cover - se, ymax = Cover + se), color="black", size=0.5, width=2.5) +
  facet_wrap(~Island_Island_Site, ncol = 2) +
  scale_x_continuous(name ="Depth", breaks = c(0,20,40,60,90,120)) +
  scale_y_continuous(name ="Cover (%)", limits=c(-5,100), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=8, colour="black"),
                                        axis.title = element_text(size=12, face="bold", colour="black"), 
                                        strip.text = element_text(size=10),
                                        legend.title = element_blank(), legend.position = "bottom") 
Cover_with_depth
ggsave ( "Outputs_R_Figures/Cover_vs_depth_persite.pdf", Cover_with_depth,width = 6, height = 8)

# This figure represents Fig. 1 b
# The arrowheads displaying 'outliers/hotspots' were added a posteriori with AI according to the Bayesian models 





# This variability between sites explains why is necessary the modelling. 
# After some mixed models tests, we decided to run Bayesian models (brms)

# The rest of this script is to prepare the databases for the modelling! 

# The data preparation is for the final chosen model. 
# Please consider that most other combinations of models (e.g., formulas, families, combinations of environmental data) were tested before choosing the final model
# We did that from the simplest model Cover~Depth to progressively adding extra variables
# Site as random variable!
# Adding all other environemtal variables


# The modeling is in the Script: "Bayesian_models.R"


# Necessary to have all the quadrats 

data_bayes <- ddply(rndpts_df, ~  Island +Island_Site + Depth + Quadrat , function(x){c(Cover = sum(x$Cover)) })
data_bayes  <- data_bayes %>% unite(Island_Island_Site, Island, Island_Site, remove = T)
data_bayes$Depth <- as.numeric (data_bayes$Depth)

# Necessary to have the environmental data for the full model 
pred <- read.csv(file = "Data/env_data.csv", header = T, dec = ".", sep = ",", row.names = 1)

# Set env_data to have the same formatting as data_Bayes
pred$ID <- rownames (pred)
# Extract island_Site
pred$Island_Island_Site <- gsub("_[^_]*$|^[^_]*_","",pred$ID,perl=T)
# Extract Quadrat
pred$Quadrat <- sub('.*\\_', '', pred$ID)

# Change the names for Island_Island_Sites
pred$Island_Island_Site <- gsub("GAMGAM1", "Gambier_1", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("GAMGAM2", "Gambier_2", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("SOCBOR1", "Bora_1", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("SOCBOR2", "Bora_2", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("SOCMOO1", "Moorea_1", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("SOCMOO2", "Moorea_2", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("SOCTAH1", "Tahiti_1", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("SOCTAH2", "Tahiti_2", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("TUAMAK1", "Makatea_1", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("TUAMAK2", "Makatea_2", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("TUARAN1", "Rangiroa_1", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("TUARAN2", "Rangiroa_2", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("TUARAR1", "Raroia_1", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("TUARAR2", "Raroia_2", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("TUATIK1", "Tikehau_1", pred$Island_Island_Site)
pred$Island_Island_Site <- gsub("TUATIK2", "Tikehau_2", pred$Island_Island_Site)


# Just in case, verify and complete both database
data_bayes <- data_bayes %>% complete( Island_Island_Site,Depth, Quadrat,fill = list(Cover = 0))
# Both databases have the same dimensions

# Merge dataframes!
Data_Bayes_environment <- merge (data_bayes, pred)
# str(Data_Bayes_environment)
Data_Bayes_environment$Depth <- as.numeric (Data_Bayes_environment$Depth)

# Since there are cells with NA, they have to be replaced by 0
Data_Bayes_environment[is.na(Data_Bayes_environment)] <- 0

# This database of coral data and environmental data data is created, yet:

##### 1st ##### - Check collinearity 
# Only keep numeric and complete variables

keep_columns <- c("Depth","Latitude","Longitude",  "Light_Rel_Ind","Temp_Variability", "Fixed.substrate","Sediment.substrate", "Rubble","Generic_richness","Years_Since_Last_bleaching", "DHW_Last_Bleaching_year","Bleaching_events_since_2015","Sum_DHWs_since_2015","SST_Satellite_mean")
num_env_data <- Data_Bayes_environment[,c(keep_columns)]
num_env_data[keep_columns] <- sapply(num_env_data[keep_columns],as.numeric)

# Compute a correlation matrix
corr <- round(cor(num_env_data), 2)
# Visualize the correlation matrix
# method = "square" (default)
ggcorrplot(corr)

# Compute a matrix of correlation p-values
p_mat <- cor_pmat(num_env_data)
# Check correlations between Depth and light
cor.test(num_env_data$Depth, num_env_data$Light_Rel_Ind)
# Depth and light are correlated - Problem of colinearity. Take into account for the future model

cor(select(num_env_data, Depth,Latitude,Longitude,Light_Rel_Ind,Temp_Variability, Fixed.substrate,Sediment.substrate,Rubble,Generic_richness,Years_Since_Last_bleaching, DHW_Last_Bleaching_year,Bleaching_events_since_2015,Sum_DHWs_since_2015,SST_Satellite_mean))

# There are not strong correlations except of Light and Depth; and SST temp and Latitude. 
# Yet, these last variables that were unique for the site (the same along the reef slope) were finally not considered in our model


#### Quick check of correlation of cover vs richness 

# We aggregate quadrats and make only a mean, sd and se. 
# The generic richness is already aggregated by all quadrats upon the Island_Island_Site and depth
Cover_vs_richness <- ddply(Data_Bayes_environment, ~ Island_Island_Site + Depth, function(x){c(Cover = mean(x$Cover), Sd = sd(x$Cover), Cover_se=sd(x$Cover) / sqrt(length(x$Cover)),
                                                                                     Generic_richness = mean (x$Generic_richness))}) # Scleractinian cover per site/depth. Mean, sd and se among the 30 quadrats


cor.test (Cover_vs_richness$Cover, Cover_vs_richness$Generic_richness,method ="pearson")

# Pearson correlation
corfun<-function(x, y) {
  corr=(cor.test(x, y,
                 alternative="two.sided", method="pearson"))}

ddply(Cover_vs_richness, .(Depth), summarise,z=corfun(Cover,Generic_richness)$statistic,
      pval=corfun(Cover,Generic_richness)$p.value,
      tau.est=corfun(Cover,Generic_richness)$estimate,
      alt=corfun(Cover,Generic_richness)$alternative
) 

# Visual plot
ggplot(Cover_vs_richness, aes(x=as.factor (Depth), y=Cover)) + 
  geom_point(shape = 20, size = 2) + 
  geom_errorbar(aes(ymin = Cover - Cover_se, ymax = Cover + Cover_se), color="black", size=0.5, width=0.5) +
  geom_point(data = Cover_vs_richness, aes(x = as.factor (Depth), y = Generic_richness*2), size = 2, shape = 8, color = "red") +
  facet_wrap(~Island_Island_Site) +
  scale_x_discrete(name ="Depth") +
  scale_y_continuous("Coral Cover (%)", sec.axis = sec_axis(~.*0.5, name = "Generic richness (n)")) +  theme_classic() + 
  theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),axis.text = element_text(size=10, colour="black"),
        axis.title = element_text(size=11, face="bold", colour= "black"), 
        axis.text.y.right = element_text(color = "red"),axis.title.y.right = element_text( color = "red"),
        legend.position = "none")






### Keep going making database ready for the modelling 

  # Null model with intercepts and depths only 
  # Full model with all environmental variables

# Necessary to create a factor Depth and numerical depth
Data_Bayes_environment$Depth <- as.factor (Data_Bayes_environment$Depth)

# Create depth as numeric variable also 
Data_Bayes_environment$Depth_num <- as.numeric (as.character(Data_Bayes_environment$Depth))

Data_Bayes_environment$Generic_richness <- as.numeric (as.character(Data_Bayes_environment$Generic_richness))
Data_Bayes_environment$Years_Since_Last_bleaching <- as.numeric (as.character(Data_Bayes_environment$Years_Since_Last_bleaching))

# There is one site with NA that has to be replaced by 0
Data_Bayes_environment[is.na(Data_Bayes_environment)] <- 0




######## Some extra necessary decisions for the model here ###########
####################################################################

# To be able to compare between environmental numerical variables, we standardised data
# We standardised extracting the mean and dividing by the standard deviation. We applied this to all numerical env. predictors

Data_Bayes_Standarized <- Data_Bayes_environment

# Standardise all the numerical variables you are finally using
Data_Bayes_Standarized$Light_Log <- (Data_Bayes_Standarized$Light_Log - mean(Data_Bayes_Standarized$Light_Log)) / sd(Data_Bayes_Standarized$Light_Log)
Data_Bayes_Standarized$Light_Rel_Ind <- (Data_Bayes_Standarized$Light_Rel_Ind - mean(Data_Bayes_Standarized$Light_Rel_Ind)) / sd(Data_Bayes_Standarized$Light_Rel_Ind)
Data_Bayes_Standarized$Temp_Hoboo <- (Data_Bayes_Standarized$Temp_Hoboo - mean(Data_Bayes_Standarized$Temp_Hoboo)) / sd(Data_Bayes_Standarized$Temp_Hoboo)
Data_Bayes_Standarized$Temp_Variability <- (Data_Bayes_Standarized$Temp_Variability - mean(Data_Bayes_Standarized$Temp_Variability)) / sd(Data_Bayes_Standarized$Temp_Variability)
Data_Bayes_Standarized$Temp_Rel_Ind <- (Data_Bayes_Standarized$Temp_Rel_Ind - mean(Data_Bayes_Standarized$Temp_Rel_Ind)) / sd(Data_Bayes_Standarized$Temp_Rel_Ind)
Data_Bayes_Standarized$SST_Satellite_mean <- (Data_Bayes_Standarized$SST_Satellite_mean - mean(Data_Bayes_Standarized$SST_Satellite_mean)) / sd(Data_Bayes_Standarized$SST_Satellite_mean)
Data_Bayes_Standarized$Sum_DHWs_since_2015 <- (Data_Bayes_Standarized$Sum_DHWs_since_2015 - mean(Data_Bayes_Standarized$Sum_DHWs_since_2015)) / sd(Data_Bayes_Standarized$Sum_DHWs_since_2015)
Data_Bayes_Standarized$Generic_richness <- (Data_Bayes_Standarized$Generic_richness - mean(Data_Bayes_Standarized$Generic_richness)) / sd(Data_Bayes_Standarized$Generic_richness)
Data_Bayes_Standarized$Temp_Rel_Ind <- (Data_Bayes_Standarized$Temp_Rel_Ind - mean(Data_Bayes_Standarized$Temp_Rel_Ind)) / sd(Data_Bayes_Standarized$Temp_Rel_Ind)
# Make sure all environmental numerical variables are standardised


# We found the best Bayesian model was with binomial distribution families
# Therefore, we transform the cover variable into a binomial distribution. Number of points, where I have incidence | number of points

# Info: 
# Column Inc_points are points falling on a coral (success)
# Column Non_Inc_points are points not falling on a coral (fail)
# Column TotPoints are the total number of points analysed

Data_Bayes_Standarized$TotPoints <- 75
Data_Bayes_Standarized$Inc_points <- (Data_Bayes_Standarized$Cover * Data_Bayes_Standarized$TotPoints) / 100
# Round the points
Data_Bayes_Standarized$Inc_points <-round(Data_Bayes_Standarized$Inc_points,0)
Data_Bayes_Standarized$Non_Inc_points <-abs (Data_Bayes_Standarized$Inc_points - Data_Bayes_Standarized$TotPoints)


# Save the standardised with the binomial distribution database for the Bayesian modelling (brms)
# The same database is used for the null and full bayesian modelling
write.csv (Data_Bayes_Standarized,  "Data/Data_Bayes_Standarized.csv")

# Bayesian modelling and figures Sup. Fig. 1 and Fig. 2 are created in the script "Bayesian_models.R"



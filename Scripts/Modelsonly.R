# rm (list = ls())


# Install.packages
library (dplyr); library(tidyverse); require (plyr); require (reshape2);
library (data.table); library(RColorBrewer);  library (tidyr);  library (ggplot2); 
require  (stats); require (matrixStats); 
library (car);  library (glmm); library (glmnet); library (glmmTMB); library (lme4); library(randomcoloR)
library (cowplot); library (patchwork); library (scales);library(viridisLite) ;  library(fishualize);
library(ggridges); library (stringr)

# Open the data 

library('brms');library('rstan');library('parallel');library('rstanarm')

coral_cover_Island_Site <- read.csv(file = "Data/coral_cover_Island_Site.csv", header = T, dec = ".", sep = ",", row.names = 1) 
Data_Bayes3_Stand_full <- read.csv(file = "Data/Data_Bayes3_Stand_full_2.csv", header = T, dec = ".", sep = ",", row.names = 1)


## Working with Binomial distribution - These are the proportions of points falling on corals / total number of points
# Info: 
# Column Inc_points are points falling on a coral (success)
# Column Non_Inc_points are points not falling on a coral (fail)
# Column TotPoints are the total number of points analysed

### Null model ####


Binomial_Model_null <- brms::brm (Inc_points | trials(TotPoints) ~ 1 + Depth_num + (1 + Depth_num | Island_Island_Site),
                                  data = Data_Bayes3_Stand_full, family = binomial(),  # prior = my_priors,
                                  control = list(adapt_delta = 0.95, max_treedepth = 13),
                                  iter = 4000, warmup = 1000, chains = 2, cores = 2,backend = "cmdstanr", threads = 20) 
save(Binomial_Model_null, file="Data/Converged_models/Binomial_Model_null.RData")
load("Data/Converged_models/Binomial_Model_null.RData") 

Bin_Model_null <- Binomial_Model_null 

plot(Bin_Model_null)
pp_check(Bin_Model_null, type = "scatter_avg") # Structured data, could be better
bayes_R2(Bin_Model_null) # 
summary (Bin_Model_null)

me_null <- conditional_effects(Bin_Model_null, nsamples = 1000, probs = c(0.05, 0.95), spaghetti = F) # Default is 0.95
plot(me_null, ask = FALSE, points = F) # Probability scale!


# From posterior predict - to get to the plot of the outliers. It changes from old model 
# Create the reference dataframe - newdata (Here for all depths)
Depth_num <- unique (Bin_Model_null$data$Depth_num)
Island_Island_Site <- unique (Bin_Model_null$data$Island_Island_Site)
Inc_points <- unique (Bin_Model_null$data$Inc_points)
TotPoints <- unique (Bin_Model_null$data$TotPoints)
#
ref_data <- crossing(Depth_num, Island_Island_Site, TotPoints)

# Forest plots with the posteriors
fitted_values <- posterior_epred(Bin_Model_null, newdata = ref_data, re_formula = 'Inc_points | trials(TotPoints) ~ Depth_num + (Depth_num | Island_Island_Site)')

# Number of rows equals to ref_data and number of dimensions is equal to (Island_Island_Site*Depths)
# str (fitted_values)
# dim (fitted_values)

# Necessary to traspose
fitted_values <- t(fitted_values)

# Create combination of ref_data
ref_data_fitted <- cbind (ref_data [c(1,2)],fitted_values)

# Multiple columns of predictions into a single one. 
ref_data_fitted <- melt (ref_data_fitted, id.vars = c ("Depth_num", "Island_Island_Site"), na.rm = F, measure.vars = c(3:6002), value.name = c("Posterior_Prob"))
colnames (ref_data_fitted) [4] <- "Posterior_Prob_" 
ref_data_fitted$Depth = factor (ref_data_fitted$Depth, levels = c ("120", "90","60", "40","20", "6"))
colours <- brewer.pal(6,"GnBu")

# To Get outliers from Posterior distribution
ref_data_fitted_Mean_Se <- ddply(ref_data_fitted, ~  Depth , function(x){c(Post_Depth_Mean = mean(x$Posterior_Prob), Post_Depth_Sd = sd(x$Posterior_Prob)) })

Data_Bayes3_Bayesian <- merge (coral_cover_Island_Site, ref_data_fitted_Mean_Se)

Data_Bayes3_Bayesian$Island <- sub("\\_.*", "", Data_Bayes3_Bayesian$Island_Island_Site)
Data_Bayes3_Bayesian$Site <- sub("^[^_]*_", "", Data_Bayes3_Bayesian$Island_Island_Site)

# Extract the number of points from cover
Data_Bayes3_Bayesian$TotPoints <- 75
Data_Bayes3_Bayesian$Inc_points <- (Data_Bayes3_Bayesian$Cover * Data_Bayes3_Bayesian$TotPoints) / 100
# Round the points
Data_Bayes3_Bayesian$Inc_points <-round(Data_Bayes3_Bayesian$Inc_points,0)
Data_Bayes3_Bayesian$Non_Inc_points <-abs (Data_Bayes3_Bayesian$Inc_points - Data_Bayes3_Bayesian$TotPoints)

# Plot from binomial proportion tranforming raw data to binomial proportions
# ggplot(Data_Bayes3_Bayesian, aes(x=Depth, y=Cover, colour = Island)) +
#   geom_point(aes(y=Inc_points, fill = Island, colour = Island, shape = Site), size=2.5) +
#   geom_ribbon(aes (ymin = Post_Depth_Mean - Post_Depth_Sd, ymax = Post_Depth_Mean + Post_Depth_Sd), alpha = 0.3, linetype=3,colour="grey23",size = 1,fill="grey21") +
#   #geom_point(aes(y=Post_Depth_Mean), shape=21, fill="white", size=4) + 
#   geom_line(aes(y=Post_Depth_Mean), colour="grey13", size=2) +
#   scale_y_continuous(name ="Exp. Post. Pred. Distribution of points falling on a coral out of 75", limits=c(-3,75), breaks = c(0,20,40,60,75)) +
#   scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
#   theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
#                                         axis.text = element_text(size=10, colour="black"),
#                                         axis.title = element_text(size=11, face="bold", colour="black"))

# Test, transform the post depth mean expected from points to coral cover
Data_Bayes3_Bayesian$Post_Depth_Mean_Cover <- (Data_Bayes3_Bayesian$Post_Depth_Mean * 100) / Data_Bayes3_Bayesian$TotPoints
Data_Bayes3_Bayesian$Post_Depth_Sd_Cover <- (Data_Bayes3_Bayesian$Post_Depth_Sd * 100) / Data_Bayes3_Bayesian$TotPoints

SFig_1_Coral_cover_Depth_Hotspots <- ggplot(Data_Bayes3_Bayesian, aes(x=Depth, y=Cover, colour = Island)) +
  geom_point(aes(y=Cover, fill = Island, colour = Island, shape = Site), size=2.5) +
  geom_ribbon(aes (ymin = Post_Depth_Mean_Cover - Post_Depth_Sd_Cover, ymax = Post_Depth_Mean_Cover + Post_Depth_Sd_Cover), alpha = 0.3, linetype=3,colour="grey23",size = 1,fill="grey21") +
  #geom_point(aes(y=Post_Depth_Mean), shape=21, fill="white", size=4) + 
  geom_line(aes(y=Post_Depth_Mean_Cover), colour="grey13", size=2) +
  scale_y_continuous(name ="Exp. Post. Pred. Distribution of coral cover (%)", limits=c(-3,100), breaks = c(0,20,40,60,80,100)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"))
SFig_1_Coral_cover_Depth_Hotspots

ggsave ( "~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/SFig_1_Coral_cover_Depth_Hotspots.pdf", SFig_1_Coral_cover_Depth_Hotspots,width = 10, height = 6)


### Null model ####

##############################################################################3

### full model ####

### We have decided to remove light because collinearity with depth, also the temp Ind. rel because did not have effect with depth and Other substrate because it did not converge
# Binomial_Model_full <- brms::brm (Inc_points | trials(TotPoints) ~ 1 + Depth_num  +  Temp_Variability +
#                                                 Bathymetry_slope + Dominant.Substrate  +
#                                                 Temp_Variability:Depth_num + Bathymetry_slope:Depth_num +
#                                                 Dominant.Substrate:Depth_num + (1 + Depth_num | Island_Island_Site),
#                                               data = Data_Bayes3_Stand_full, family = binomial(),  # prior = my_priors,
#                                               control = list(adapt_delta = 0.95, max_treedepth = 13),
#                                               iter = 4000, warmup = 1000, chains = 2, cores = 2,backend = "cmdstanr",threads = 20) 
# save(Binomial_Model_full, file="Data/Converged_models/Binomial_Model_full.RData")
load("Data/Converged_models/Binomial_Model_full.RData")



Bin_Model_full <- Binomial_Model_full 

plot(Bin_Model_full) 
pp_check(Bin_Model_full, type = "scatter_avg") # Structured data, could be better
bayes_R2(Bin_Model_full) # 
summary (Bin_Model_full)

me_full <- conditional_effects(Bin_Model_full, nsamples = 1000, probs = c(0.25, 0.75), spaghetti = F) # Default is 0.95
plot(me_full, ask = FALSE, points = F) # Probability scale!
##### Second full model with interactions #### necessary to execute from server, in my MAC Takes 30 hours aprox!




# Plots to make fig 2 from full model with interactions
me_full <- conditional_effects(Bin_Model_full, nsamples = 1000, spaghetti = F, probs = c(0.25, 0.75))

# Depth alone
plot_depth <- plot(me_full, plot = FALSE)[["Depth_num"]] + # scale_y_continuous(limits = c(-5,80)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Depth (m)") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_depth 



# Temp var
plot_temp_var <- plot(me_full, plot = FALSE)[["Depth_num:Temp_Variability"]] + # scale_y_continuous(limits = c(-5,80)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Temperature variability (ºC)") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_temp_var


# Dominant substrate
plot_domsubs <- plot(me_full, plot = FALSE)[["Depth_num:Dominant.Substrate"]] + # scale_y_continuous(limits = c(-5,60)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Dom. subs. structure") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_domsubs


# Slope - factor to select the order on which they appear!
me_full$`Depth_num:Bathymetry_slope`$Bathymetry_slope =  factor(me_full$`Depth_num:Bathymetry_slope`$Bathymetry_slope, levels = c ("Steep","Moderate","Wall","Gentle"))
plot_slope <- plot(me_full, plot = FALSE)[["Depth_num:Bathymetry_slope"]] + # scale_y_continuous(limits = c(-5,60)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Bathymetry slope") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_slope


(Figure_2 = plot_depth +  plot_temp_var + plot_domsubs + plot_slope + plot_layout(guides = 'collect', ncol = 2)  & theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()))
ggsave( "~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/Figure_2_new.pdf", Figure_2,  width = 8, height = 8)


### full model ####


###############################################


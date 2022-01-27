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

Data_Bayes3 <- read.csv(file = "Data/Data_Bayes3.csv", header = T, dec = ".", sep = ",", row.names = 1) 


### The Null #(OLD)# model! 

(prior <- get_prior(Cover ~  Depth + (1 + Depth | Island_Island_Site),
                    data = Data_Bayes3, family = student()))
# Update with Student Distribution - and give priors for each depth!
my_priors <- set_prior("", class = "b", coef = "") +
  set_prior ("student_t(3, 22.7, 31.6)", class = "Intercept") +
  set_prior ("gamma(2, 0.1)", class = "nu") +
  set_prior("student_t(3, 0, 31.6)", class = "sigma")

# fit_cover_null_Student_7 <- brm(Cover ~  Depth + (1 + Depth | Island_Island_Site),
#                                 data = Data_Bayes3,
#                                 iter = 5000, family = student(),
#                                 control = list(adapt_delta = 0.99, max_treedepth = 35),
#                                 prior = my_priors, chains = 3, cores = 3)
## Save and load outputs of model
# save(fit_cover_null_Student_7, file="~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/Data_Scripts/Dataframes/Bayesian_Outputs/fit_cover_null_Student_7.RData")
load("Data/Converged_models/fit_cover_null_Student_7.RData") 

##

plot(fit_cover_null_Student_7) # traceplot and posterior distributions, add to online supp
pp_check(fit_cover_null_Student_7, type = "scatter_avg") # posterior_predictive check, add to online supp
bayes_R2(fit_cover_null_Student_7) # bayesian R2, add to your main figure and results section?
summary (fit_cover_null_Student_7)

### The Null model is finished!



#### The full #(old)# model  #### This is not good!

Data_Bayes3_Stand_full <- read.csv(file = "Data/Data_Bayes3_Stand_full_2.csv", header = T, dec = ".", sep = ",", row.names = 1)


# Here the environmental data of the model are standarised
# We decided to keep the full model, without Generic richness and adding the interaction of environmental variables we kepth with depth
# The model is to understand what env. conditions favour coral cover rather than modelling the outliers!

(prior <- get_prior(Cover ~   Depth_num:Light_Rel_Ind  +  Temp_Rel_Ind:Depth_num + Temp_Variability:Depth_num + Bathymetry_slope:Depth_num + Dominant.Substrate:Depth_num + Dominant.benthic.non_coral:Depth_num + (1 + Depth_num | Island_Island_Site),
                    data = Data_Bayes3_Stand_full, family = gaussian()))

my_priors <- set_prior("", class = "b", coef = "") +
  set_prior ("student_t(3, 22.7, 31.6)", class = "Intercept") +
  set_prior("student_t(3, 0, 31.6)", class = "sigma")

# fit_cover_full_Stand_Depht_num_6 <- brms::brm (Cover ~   Depth_num:Light_Rel_Ind  +  Temp_Rel_Ind:Depth_num + Temp_Variability:Depth_num + Bathymetry_slope:Depth_num + Dominant.Substrate:Depth_num + Dominant.benthic.non_coral:Depth_num + (1 + Depth_num | Island_Island_Site),
#                                                 data = Data_Bayes3_Stand_full, prior = my_priors, family = gaussian(),
#                                                 control = list(adapt_delta = 0.98, max_treedepth = 17),
#                                                 iter = 6000, warmup = 1000, chains = 2, cores = 2, backend = "cmdstanr",threads = 20)
# save(fit_cover_full_Stand_Depht_num_6, file="~/sauvegarde/Bayesian/fit_cover_full_Stand_Depht_num_6.RData")
load("Data/Converged_models/fit_cover_full_Stand_Depht_num_6.RData") 
##
plot(fit_cover_full_Stand_Depht_num_6) # traceplot and posterior distributions, add to online supp
pp_check(fit_cover_full_Stand_Depht_num_6, type = "scatter_avg") # posterior_predictive check, add to online supp
bayes_R2(fit_cover_full_Stand_Depht_num_6) # 
summary (fit_cover_full_Stand_Depht_num_6)

me_full <- conditional_effects(fit_cover_full_Stand_Depht_num_6, nsamples = 1000, probs = c(0.25, 0.75), spaghetti = F) # Default is 0.95
plot(me_full, ask = FALSE, points = F) # Probability scale!

#### The full model (old) ####

############## Change to binomial distribution ##############################
#First the full model in case we keep the null old model

Data_Bayes3_Stand_full <- read.csv(file = "Data/Data_Bayes3_Stand_full_2.csv", header = T, dec = ".", sep = ",", row.names = 1)

# Info: 
# Column Inc_points are points falling on a coral (success)
# Column Non_Inc_points are points not falling on a coral (fail)
# Column TotPoints are the total number of points analysed


# 2nd test
# Bin_Model_full_2 <- brm (Inc_points | trials(TotPoints) ~ Depth_num + Light_Rel_Ind + Temp_Rel_Ind + Temp_Variability + Bathymetry_slope + 
#                                Dominant.Substrate + Dominant.benthic.non_coral + Depth_num:Light_Rel_Ind  + Temp_Rel_Ind:Depth_num + 
#                                Temp_Variability:Depth_num + Bathymetry_slope:Depth_num +
#                                Dominant.Substrate:Depth_num + Dominant.benthic.non_coral:Depth_num + (1 + Depth_num | Island_Island_Site),
#                              data = Data_Bayes3_Stand_full, family = binomial(),  # prior = my_priors,
#                              control = list(adapt_delta = 0.95, max_treedepth = 13),
#                              iter = 6000, warmup = 2000, chains = 2, cores = 2) # ,backend = "cmdstanr",threads = 20 / brms::
# save(Bin_Model_full_2, file="~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/Data_Scripts/Dataframes/Bayesian_Outputs/Bayesian_Tests/Bin_Model_full_2.RData")
load("Data/Converged_models/Bin_Model_full_2.RData")


plot(Bin_Model_full_2) 
pp_check(Bin_Model_full_2, type = "scatter_avg") # Structured data, could be better
bayes_R2(Bin_Model_full_2) # 
summary (Bin_Model_full_2)

me_full <- conditional_effects(Bin_Model_full_2, nsamples = 1000, probs = c(0.25, 0.75), spaghetti = F) # Default is 0.95
plot(me_full, ask = FALSE, points = F) # Probability scale!



###### The Null model test with binomial distribution ###### 
# Bin_Model_null <- brm (Inc_points | trials(TotPoints) ~ Depth_num + (0 + Depth_num | Island_Island_Site),
#                          data = Data_Bayes3_Stand_full, family = binomial(),  # prior = my_priors,
#                          control = list(adapt_delta = 0.95, max_treedepth = 13),
#                          iter = 1000, warmup = 500, chains = 2, cores = 2) # ,backend = "cmdstanr",threads = 20 / brms::
# save(Bin_Model_null, file="Data/Converged_models/Bin_Model_null.RData")
load("Data/Converged_models/Bin_Model_null.RData")

# Without the (0 + )
# Bin_Model_null2 <- brm (Inc_points | trials(TotPoints) ~ Depth_num + (Depth_num | Island_Island_Site),
#                           data = Data_Bayes3_Stand_full, family = binomial(),  # prior = my_priors,
#                           control = list(adapt_delta = 0.95, max_treedepth = 13),
#                           iter = 4000, warmup = 1000, chains = 2, cores = 2) # ,backend = "cmdstanr",threads = 20 / brms::
# save(Bin_Model_null2, file="Data/Converged_models/Bin_Model_null2.RData")
load("Data/Converged_models/Bin_Model_null2.RData") # Better this one without the 0
Bin_Model_null <- Bin_Model_null2 

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


ggplot(Data_Bayes3_Bayesian, aes(x=Depth, y=Cover, colour = Island)) +
  geom_point(aes(y=Inc_points, fill = Island, colour = Island, shape = Site), size=2.5) +
  geom_ribbon(aes (ymin = Post_Depth_Mean - Post_Depth_Sd, ymax = Post_Depth_Mean + Post_Depth_Sd), alpha = 0.3, linetype=3,colour="grey23",size = 1,fill="grey21") +
  #geom_point(aes(y=Post_Depth_Mean), shape=21, fill="white", size=4) + 
  geom_line(aes(y=Post_Depth_Mean), colour="grey13", size=2) +
  scale_y_continuous(name ="Exp. Post. Pred. Distribution of points falling on a coral out of 75", limits=c(-3,75), breaks = c(0,20,40,60,75)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"))

# Test, transform the post depth mean expected from points to coral
Data_Bayes3_Bayesian$Post_Depth_Mean_Cover <- (Data_Bayes3_Bayesian$Post_Depth_Mean * 100) / Data_Bayes3_Bayesian$TotPoints
Data_Bayes3_Bayesian$Post_Depth_Sd_Cover <- (Data_Bayes3_Bayesian$Post_Depth_Sd * 100) / Data_Bayes3_Bayesian$TotPoints

ggplot(Data_Bayes3_Bayesian, aes(x=Depth, y=Cover, colour = Island)) +
  geom_point(aes(y=Cover, fill = Island, colour = Island, shape = Site), size=2.5) +
  geom_ribbon(aes (ymin = Post_Depth_Mean_Cover - Post_Depth_Sd_Cover, ymax = Post_Depth_Mean_Cover + Post_Depth_Sd_Cover), alpha = 0.3, linetype=3,colour="grey23",size = 1,fill="grey21") +
  #geom_point(aes(y=Post_Depth_Mean), shape=21, fill="white", size=4) + 
  geom_line(aes(y=Post_Depth_Mean_Cover), colour="grey13", size=2) +
  scale_y_continuous(name ="Exp. Post. Pred. Distribution of coral cover (%)", limits=c(-5,100), breaks = c(0,20,40,60,80)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"))

# Outliers change a bit from the previous model... 

###### The Null model test with binomial distribution ###### 

##### Second full model with interactions #### necessary to execute from server, in my MAC Takes 30 hours aprox!
# Bin_Model_full_3 <- brms::brm (Inc_points | trials(TotPoints) ~ Depth_num + Light_Rel_Ind + Temp_Rel_Ind + Temp_Variability + Bathymetry_slope + 
#                                  Dominant.Substrate + Dominant.benthic.non_coral + Depth_num:Light_Rel_Ind  + Temp_Rel_Ind:Depth_num + 
#                                  Temp_Variability:Depth_num + Bathymetry_slope:Depth_num +
#                                  Dominant.Substrate:Depth_num + Dominant.benthic.non_coral:Depth_num + (0 + Depth_num | Island_Island_Site),
#                                data = Data_Bayes3_Stand_full, family = binomial(),  # prior = my_priors,
#                                control = list(adapt_delta = 0.95, max_treedepth = 13),
#                                iter = 2000, warmup = 1000, chains = 10, cores = 10) # ,backend = "cmdstanr",threads = 20 / brms::
# save(Bin_Model_full_3, file="~/Gonzalo_CoralCover/Data/Bin_Model_full_3.RData")
load("Data/Converged_models/Bin_Model_full_3.RData")



# Without the (0 + )
# Bin_Model_full_4 <- brms::brm (Inc_points | trials(TotPoints) ~ Depth_num + Light_Rel_Ind + Temp_Rel_Ind + Temp_Variability + Bathymetry_slope + 
#                                  Dominant.Substrate + Dominant.benthic.non_coral + Depth_num:Light_Rel_Ind  + Temp_Rel_Ind:Depth_num + 
#                                  Temp_Variability:Depth_num + Bathymetry_slope:Depth_num +
#                                  Dominant.Substrate:Depth_num + Dominant.benthic.non_coral:Depth_num + (Depth_num | Island_Island_Site),
#                                data = Data_Bayes3_Stand_full, family = binomial(),  # prior = my_priors,
#                                control = list(adapt_delta = 0.95, max_treedepth = 13),
#                                iter = 4000, warmup = 1000, chains = 2, cores = 10) # ,backend = "cmdstanr",threads = 20 / brms::
# save(Bin_Model_full_4, file="~/Gonzalo_CoralCover/Data/Bin_Model_full_4.RData")
load("Data/Converged_models/Bin_Model_full_4.RData")
Bin_Model_full_3 <- Bin_Model_full_4 

# Without the (0 + ) and without the dominant benthic non-coral
# Bin_Model_full_5 <- brms::brm (Inc_points | trials(TotPoints) ~ Depth_num + Light_Rel_Ind + Temp_Rel_Ind + Temp_Variability + Bathymetry_slope + 
#                                  Dominant.Substrate +  Depth_num:Light_Rel_Ind  + Temp_Rel_Ind:Depth_num + 
#                                  Temp_Variability:Depth_num + Bathymetry_slope:Depth_num +
#                                  Dominant.Substrate:Depth_num +  (Depth_num | Island_Island_Site),
#                                data = Data_Bayes3_Stand_full, family = binomial(),  # prior = my_priors,
#                                control = list(adapt_delta = 0.95, max_treedepth = 13),
#                                iter = 4000, warmup = 1000, chains = 2, cores = 10) # ,backend = "cmdstanr",threads = 20 / brms::
# save(Bin_Model_full_5, file="~/Gonzalo_CoralCover/Data/Bin_Model_full_5.RData")
load("Data/Converged_models/Bin_Model_full_5.RData")


Bin_Model_full_3 <- Bin_Model_full_4 
Bin_Model_full_3 <- Bin_Model_full_5 


plot(Bin_Model_full_3) 
pp_check(Bin_Model_full_3, type = "scatter_avg") # Structured data, could be better
bayes_R2(Bin_Model_full_3) # 
summary (Bin_Model_full_3)

me_full <- conditional_effects(Bin_Model_full_3, nsamples = 1000, probs = c(0.25, 0.75), spaghetti = F) # Default is 0.95
plot(me_full, ask = FALSE, points = F) # Probability scale!
##### Second full model with interactions #### necessary to execute from server, in my MAC Takes 30 hours aprox!




# Plots to make fig 2 from full model with interactions
me_full <- conditional_effects(Bin_Model_full_3, nsamples = 200, spaghetti = F, probs = c(0.33, 0.66))

# Light rel
plot_light <- plot(me_full, plot = FALSE)[["Depth_num:Light_Rel_Ind"]] + # scale_y_continuous(limits = c(-5,80)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Light index (μmol m-2 s-1)") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_light # When Rel light increases, coral cover increase


# Temp var
plot_Temp <- plot(me_full, plot = FALSE)[["Depth_num:Temp_Variability"]] + # scale_y_continuous(limits = c(-5,80)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Temp variability (ºC)") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_Temp 

plot_Temp_rel <- plot(me_full, plot = FALSE)[["Depth_num:Temp_Rel_Ind"]] + # scale_y_continuous(limits = c(-5,80)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Temp relative Index") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_Temp_rel


# Slope - factor to select the order on which they appear!
me_full$`Depth_num:Bathymetry_slope`$Bathymetry_slope =  factor(me_full$`Depth_num:Bathymetry_slope`$Bathymetry_slope, levels = c ("Steep","Moderate","Wall","Gentle"))
plot_slope <- plot(me_full, plot = FALSE)[["Depth_num:Bathymetry_slope"]] + # scale_y_continuous(limits = c(-5,60)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Bathymetry slope") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_slope

# Dominant substrate
plot_domsubs <- plot(me_full, plot = FALSE)[["Depth_num:Dominant.Substrate"]] + # scale_y_continuous(limits = c(-5,60)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Dom. subs. structure") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_domsubs

# Non-benthic substrate - I would skip these guys!
plot_non_bent <- plot(me_full, plot = FALSE)[["Depth_num:Dominant.benthic.non_coral"]] + # scale_y_continuous(limits = c(-5,60)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Dom. benthic (non corals)") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_non_bent


(Figure_2 = plot_light +  plot_Temp + plot_domsubs + plot_slope + plot_layout(guides = 'collect', ncol = 2)  & theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()))

# Depth increases the likelihood of coral cover...

plot_depth <- plot(me_full, plot = FALSE)[["Depth_num"]] + # scale_y_continuous(limits = c(-5,80)) +
  scale_x_continuous(name ="Depth (m)",limits=c(5,123), breaks = c(6,20,40,60,90,120)) +
  labs(y = "Likelihood of coral cover (%)", x = "Depth (m)", title = "Depth (m)") + theme_classic() +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5))
plot_depth # When Rel light increases, coral cover increase



###############################################



#### Tests for models with beta distribution!
# Necessary to make cover percentatge between 0 and 1
Data_Bayes3_Stand_full$Per_Cover <- Data_Bayes3_Stand_full$Cover / 100

# The model displays an error: Second shape parameter is 0, but must be > 0! 
# Because the model does not converge with 0 I applied a "solution", something necessary to fix
Data_Bayes3_Stand_full$Per_Cover [Data_Bayes3_Stand_full$Per_Cover==0] <- .0001
# It rejects all 0, perhaps need to transform to 0.000001?
# https://discourse.mc-stan.org/t/beta-regression-help-with-error-evaluating-the-log-probability-at-the-initial-value/14751/4

(prior <- get_prior(Per_Cover ~ Depth_num + Light_Rel_Ind + Temp_Rel_Ind + Temp_Variability + Bathymetry_slope + 
                      Dominant.Substrate + Dominant.benthic.non_coral + Depth_num:Light_Rel_Ind  + Temp_Rel_Ind:Depth_num + 
                      Temp_Variability:Depth_num + Bathymetry_slope:Depth_num +
                      Dominant.Substrate:Depth_num + Dominant.benthic.non_coral:Depth_num + (1 + Depth_num | Island_Island_Site),
                    data = Data_Bayes3_Stand_full, family = zero_one_inflated_beta("logit")))

my_priors <- set_prior("", class = "b", coef = "Depth_num") +
  set_prior ("beta(1, 1)", class = "coi") +
  set_prior ("lkj(1)", class = "cor",group = "Island_Island_Site") +
  set_prior ("student_t(3, 0, 2.5)", class = "Intercept") +
  set_prior("gamma(0.01, 0.01)", class = "phi") +
  set_prior ("student_t(3, 0, 2.5)", class = "sd", coef = "Depth_num",group = "Island_Island_Site") +
  set_prior ("student_t(3, 0, 2.5)", class = "sd", coef = "Intercept",group = "Island_Island_Site") +
  set_prior("beta(1, 1)", class = "zoi")


Beta_Model_full <- brm(Per_Cover ~ Depth_num + Light_Rel_Ind + Temp_Rel_Ind + Temp_Variability + Bathymetry_slope + 
                         Dominant.Substrate + Dominant.benthic.non_coral + Depth_num:Light_Rel_Ind  + Temp_Rel_Ind:Depth_num + 
                         Temp_Variability:Depth_num + Bathymetry_slope:Depth_num +
                         Dominant.Substrate:Depth_num + Dominant.benthic.non_coral:Depth_num + (1 + Depth_num | Island_Island_Site),
                       data = Data_Bayes3_Stand_full,
                       family = zero_one_inflated_beta("logit"),iter = 5000,
                       control = list(adapt_delta = 0.99, max_treedepth = 14),
                       prior = my_priors, chains = 3, cores = 3)
# I had to increase adapt_delta, max_treedepth and iter. Even increasing more, forcing to converge with adapt delta and max tree depth!
save(Beta_Model_full, file="~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/Data_Scripts/Dataframes/Bayesian_Outputs/Bayesian_Tests/Beta_Model_full.RData")
load("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/Data_Scripts/Dataframes/Bayesian_Outputs/Bayesian_Tests/Beta_Model_full.RData") 

plot(Beta_Model_full) 
pp_check(Beta_Model_full, type = "scatter_avg") # Data starts to look better
bayes_R2(Beta_Model_full) # Improving but still, Horrible!!!!
summary (Beta_Model_full)

me_full <- conditional_effects(Beta_Model_full, nsamples = 200, spaghetti = F) # Default is 0.95
plot(me_full, ask = FALSE, points = F) # Probability scale!




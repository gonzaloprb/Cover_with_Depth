


##########################################################################

# Script to check up the correlations and principal component analysis between environmental predictors and living benthos categories
# This is supplementary to also understand the modeling categories of the Species_Distribution_Model

library(ggcorrplot) 

# Environmental clustering and environmental PCA


resp <- read.csv(file = "Data/resp_data_living_benthos.csv", header = T, dec = ".", sep = ",", row.names = 1)
pred <- read.csv(file = "Data/env_data.csv", header = T, dec = ".", sep = ",", row.names = 1)


# PCA - select data
PCA_env_data <- select (pred, Depth, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind, Generic_richness)

PCA_resp_data <- select (resp, Scleractinian_branching, Scleractinian_encrusting, Scleractinian_massive, Scleractinian_laminar, Scleractinian_solitary)

# Sum Scleractinian total cover of PCA_resp_data
PCA_resp_data$Scleractinian_total <- as.numeric(apply(PCA_resp_data[,1:5], 1, sum))

# PCA numeric data combining both
PCA_data <- cbind (PCA_resp_data, PCA_env_data)
# str (PCA_data)

# PCA
res.pca <- PCA(PCA_data, quanti.sup = 1:6)

barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
summary(res.pca)

plot(res.pca,choix="ind",habillage=6)
dimdesc(res.pca, axes = 1:2)
plotellipses(res.pca,6)


# With another type of PCA
library(ade4); library (factoextra)

# Unfortunately asks for deleting NA
PCA_data <- na.omit(PCA_data)

pca <- dudi.pca(PCA_data, scale = T)   # scale = T (mean/stand.dev) because data is not on the same scale units
# In my case, I need to choose 6 axis to be at least at 80%

inertia.dudi(pca,col = TRUE)    # Information of variables 

# 1st Decomposition of total inertia
# With just Ax1 we have 28.31% of the total information
# With Ax1 and Ax2 we have 46.20% of the total information

# 2nd Column absolute contributions (%):
# Represent the contribution of each variable to each axe
# Depth contributes 29% to Axis 1

# 3rd Signed column relative contributions (cos^2) (depends of length and angle)
# Quality representation
# Depth 90.83 a good representation quality (because is close to 100) on axis 1
# Scleractinian total -91.4190 has a good representation quality on axis 2. 

# In summary, you need to consider as well the angle!


# Check correlation
pca$co

# Correlation of variables with axis
# Depth 0.95, Temp_Variability 0.86 are very good correlated with Axis1
# Scleractinian_total -0.95 is well correlated with axis2


# Now Graph
s.corcircle(pca$co, xax=1, yax=2, box = FALSE)

# Graphic interpretation: 
# There are three variables that are well represented and well correlated with the Axe1.
# The other variable is well represented and negative correlated with the Axe2

#######
# If we have had three graphs
# s.corcircle(pca$co, xax=1, yax=3)
#######

# Representation of individual (individually)
inertia.dudi(pca, row = TRUE)

# Coordinate of individuals
pca$li


# Inertia and contributions

# Look at graphic of contributions
contribution <- inertia.dudi(pca, row = T)

# Parallel plots
plot(contribution$row.abs)

# s.label(contribution$row.abs)

# Representation of individuals
# s.label (pca$li, clab = 0, xax = 1, yax =2, sub = "axe1 vs axe2")     # clab=1 gives labels
# s.label (pca$li, clab = 0, xax = 1, yax =3, sub = "axe1 vs axe3")      # With three axes


# Now we are going to overlap the species and the variables to have all the information
gcol <- c("wheat", "aquamarine2","deepskyblue","blue","navyblue","black")

PCA_data$Depth <- as.factor (PCA_data$Depth)
s.class (pca$li, fac = PCA_data [,7], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (4.5*pca$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = FALSE) # Add the variables
# 5*pca$co multiplicate the arrows for 5


########## PCA with categorical data also
PCA_env_data_cat <- select (pred, Bathymetry_slope, Dominant.Substrate, Dominant.benthic.non_coral)


PCA_data_cat <- cbind (PCA_resp_data, PCA_env_data, PCA_env_data_cat)
# str (PCA_data)

# PCA
res.pca <- PCA(PCA_data_cat, quanti.sup = 1:6, quali.sup = 12:14)

barplot(res.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(res.pca$eig))
summary(res.pca)

plot(res.pca,choix="ind",habillage=6)
dimdesc(res.pca, axes = 1:2)
plotellipses(res.pca,6)






####### Morphologies per depth
# Working from View (data)
# Very important to complete to consider 0 before doing the mean
data <- data %>% complete( Island,Island_Site,Depth,Quadrat,Category,fill = list(Cover = 0))

Total_cover_data <- ddply(data, ~ Island + Island_Site + Depth + Category, function(x){c(Cover = mean(x$Cover), Sd = sd(x$Cover), Cover_se=sd(x$Cover) / sqrt(length(x$Cover)))}) # Cover of categories per site/depth. Mean, sd and se among the 30 quadrats

Total_cover_data  <- Total_cover_data %>% unite(Island_Island_Site, Island, Island_Site, remove = T)

keep <-  c ("Scleractinian_branching","Scleractinian_massive","Scleractinian_encrusting","Scleractinian_solitary","Scleractinian_laminar")

Total_cover_data_coral_forms <- subset(Total_cover_data,Category %in% keep)


# Measure cover and standard error for the site and deth of overall coral cover. 
Total_cover_data_se <- subset(data,Category %in% keep)
keep <-  c ("Scleractinian_branching","Scleractinian_massive","Scleractinian_encrusting","Scleractinian_solitary","Scleractinian_laminar")

# Summ all forms by quadrats
Total_cover_data_se <- ddply(Total_cover_data_se, ~ Island +Island_Site + Depth + Quadrat , function(x){c(Cover = sum(x$Cover)) })
# make the mean of the 30 quadrats
Total_cover_data_se <- ddply(Total_cover_data_se, ~ Island + Island_Site + Depth , function(x){c(Cover_total = mean(x$Cover), Cover_total_se=sd(x$Cover) / sqrt(length(x$Cover)))}) # Cover of categories per site/depth. Mean, sd and se among the 30 quadrats
# Unite island and site
Total_cover_data_se  <- Total_cover_data_se %>% unite(Island_Island_Site, Island, Island_Site, remove = T)

# Merge the two dataframes
Total_cover_data_coral_forms <- merge (Total_cover_data_coral_forms,Total_cover_data_se, by = c("Island_Island_Site", "Depth"))

# Make same format of Fig. 1 B to put together 

# Changes names
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Moorea_1', 'Moorea S1', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Moorea_2', 'Moorea S2', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Tahiti_1', 'Tahiti S1', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Tahiti_2', 'Tahiti S2', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Bora_1', 'Bora S1', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Bora_2', 'Bora S2', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Tikehau_1', 'Tikehau S1', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Tikehau_2', 'Tikehau S2', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Rangiroa_1', 'Rangiroa S1', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Rangiroa_2', 'Rangiroa S2', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Raroia_1', 'Raroia S1', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Raroia_2', 'Raroia S2', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Makatea_1', 'Makatea S1', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Makatea_2', 'Makatea S2', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Mangareva_1', 'Gambier S1', Total_cover_data_coral_forms$Island_Island_Site)
Total_cover_data_coral_forms$Island_Island_Site <- gsub('Mangareva_2', 'Gambier S2', Total_cover_data_coral_forms$Island_Island_Site)

Total_cover_data_coral_forms$Island_Island_Site <- factor(Total_cover_data_coral_forms$Island_Island_Site, levels = c("Moorea S1","Moorea S2","Tahiti S1","Tahiti S2","Bora S1","Bora S2","Tikehau S1","Tikehau S2","Rangiroa S1","Rangiroa S2","Raroia S1","Raroia S2","Makatea S1","Makatea S2","Gambier S1","Gambier S2"))

# Change name of forms
Total_cover_data_coral_forms$Category <-  gsub('Scleractinian_branching', 'Branching', Total_cover_data_coral_forms$Category)
Total_cover_data_coral_forms$Category <-  gsub('Scleractinian_massive', 'Massive', Total_cover_data_coral_forms$Category)
Total_cover_data_coral_forms$Category <-  gsub('Scleractinian_encrusting', 'Encrusting', Total_cover_data_coral_forms$Category)
Total_cover_data_coral_forms$Category <-  gsub('Scleractinian_solitary', 'Solitary', Total_cover_data_coral_forms$Category)
Total_cover_data_coral_forms$Category <-  gsub('Scleractinian_laminar', 'Laminar', Total_cover_data_coral_forms$Category)



# Stack area plot
Total_cover_data_coral_forms$Category = factor(Total_cover_data_coral_forms$Category,levels = c ("Branching","Massive","Encrusting","Solitary","Laminar"))

colours <-brewer.pal(5, "Spectral")

Cover_form_vs_depth <- ggplot(Total_cover_data_coral_forms, aes(x=Depth, y=Cover)) +
  geom_bar(aes(fill=factor(Category)), stat="identity") + 
  scale_fill_manual(values= colours) +
  facet_wrap(~Island_Island_Site,ncol = 4) + 
  scale_x_continuous(name ="Depth (m)", breaks = c(6,20,40,60,90,120)) +
  scale_y_continuous(name ="Cover by morphology (%)", limits=c(-5,100), breaks = c(0,20,40,60,80)) +
  theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                          axis.text = element_text(size=8, colour="black"),
                          axis.title = element_text(size=12, face="bold", colour="black"), 
                          strip.text = element_text(size=10),
                          legend.title = element_blank(), legend.position = "bottom") 
Cover_form_vs_depth
# ggsave ( "~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/Cover_form_vs_depth.pdf", Cover_form_vs_depth,width = 8, height = 8)





# Add the error_bar
ggplot(Total_cover_data_coral_forms, aes(x=Depth, y=Cover, ymin=Cover-Cover_se, ymax=Cover+Cover_se, fill=Category)) +
  geom_bar(position=position_dodge(width = 15), aes(y=Cover), stat="identity", width = 15) +
  geom_errorbar (position=position_dodge(width=15), colour="black") +
  scale_fill_manual(values= colours) +
  facet_wrap(~Island_Island_Site,ncol = 8) + 
  scale_x_continuous(name ="Depth (m)", breaks = c(6,20,40,60,90,120)) +
  scale_y_continuous(name ="Cover by morphology", limits=c(0,100), breaks = c(0,20,40,60,80,100)) +
  theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                          axis.text = element_text(size=10, colour="black"),
                          axis.title = element_text(size=11, face="bold", colour="black"), legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 5, title = NULL))


# Add the error_bar but only on the top of total coral cover

Cover_form_vs_depth_se <-  ggplot(Total_cover_data_coral_forms, aes(x=Depth, y=Cover)) +
  geom_bar(aes(fill=factor(Category)), stat="identity") + 
  geom_errorbar(aes(ymin = Cover_total - Cover_total_se, ymax = Cover_total + Cover_total_se), color="black", size=0.5, width=1) +
  scale_fill_manual(values= colours) +
  facet_wrap(~Island_Island_Site,ncol = 4) + 
  scale_x_continuous(name ="Depth (m)", breaks = c(6,20,40,60,90,120)) +
  scale_y_continuous(name ="Cover by morphology (%)", limits=c(-5,100), breaks = c(0,20,40,60,80)) +
  theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                          axis.text = element_text(size=8, colour="black"),
                          axis.title = element_text(size=12, face="bold", colour="black"), 
                          strip.text = element_text(size=10),
                          legend.title = element_blank(), legend.position = "bottom") 
Cover_form_vs_depth_se
# ggsave ( "~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/Cover_form_vs_depth_se.pdf", Cover_form_vs_depth_se,width = 8, height = 8)


Cover_form_per_depth_se <- ggplot(Total_cover_data_coral_forms, aes(x=Island_Island_Site, y=Cover)) +
  geom_bar(aes(fill=factor(Category)), stat="identity", width = 0.6) +  facet_grid (rows = vars(Depth)) +
  geom_errorbar(aes(ymin = Cover_total - Cover_total_se, ymax = Cover_total + Cover_total_se), color="black", size=0.3, width=0.2) +
  scale_fill_manual(values= colours) +
  scale_y_continuous(position = "left",breaks = c(0,20,40,60,80))+ scale_x_discrete ()+ ylab ("Percentage of cover(%)") + xlab ("") + 
  ggtitle("")+
  theme_classic() + theme(plot.title = element_text(hjust=0.5, size=16, face="bold"),
                          axis.text.y = element_text(size=14, colour="black"),
                          axis.text.x = element_text(size=12, colour="black", angle = 90),
                          strip.text.y = element_text(size=16, colour="black"),
                          strip.text.x = element_text(angle = 0),
                          strip.text.y.left = element_text(angle = 0),
                          axis.title = element_text(size=14, face="bold", colour="black"), legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 5, title = NULL))
Cover_form_per_depth_se
ggsave ( "~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/Cover_form_per_depth_se.pdf", Cover_form_per_depth_se,width = 8, height = 8)




##### Plot in relative ( do not run because it ruins what comes after) #####
coral_data_main_Total <- aggregate(Cover ~  Island_Island_Site + Depth,  Total_cover_data_coral_forms, sum)
Total_cover_data_coral_forms <- merge (Total_cover_data_coral_forms,coral_data_main_Total, by=c("Island_Island_Site","Depth"))
names (Total_cover_data_coral_forms) <- c("Island_Island_Site","Depth", "Category","Category_cover","Category_cover_sd","Category_cover_se", "Total_cover")
# Calculation of Relative contribution to Total coral cover
Total_cover_data_coral_forms$Relative <- (Total_cover_data_coral_forms$Category_cover/ Total_cover_data_coral_forms$Total_cover)*100

# Stack plot
ggplot(Total_cover_data_coral_forms, aes(x=Depth, y=Relative, fill=Category)) +
  scale_fill_manual(values= colours) +
  geom_area(alpha=1 , size=0.5, colour="black") + facet_wrap(~Island_Island_Site,ncol = 8) + 
  scale_x_reverse(lim=c(120,0), breaks = c(6,20,40,60,90,120)) +    coord_flip() + 
  ggtitle ("Relative percentatge of generic composition") + xlab ("Depth (m)") + ylab ("") + labs(fill='Coral genera', size = 17) +
  theme_classic() + theme(axis.title = element_text(size=17),
                          plot.title = element_text(size=20),
                          axis.text.y =  element_text(size=15),
                          axis.text.x =  element_blank(),
                          axis.ticks.x=element_blank(),
                          strip.text.x =  element_text(size = 15,colour = "black", angle = 0),
                          strip.text.y = element_text(size = 6),strip.text.y.left = element_text(angle=0, size = 15),
                          strip.background = element_rect(fill="grey"),
                          legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 8, title.hjust = 0.4))


ggplot(Total_cover_data_coral_forms, aes(x=Island_Island_Site, y=Relative)) +
  geom_bar(aes(fill=factor(Category)), stat="identity", width = 0.3) +  facet_grid (rows = vars(Depth), switch = "both") +
  scale_fill_manual(values= colours) +
  scale_y_continuous(position = "right",breaks = c(0,50,100))+ scale_x_discrete ()+ ylab ("Percentatge (%)") + xlab ("Islands") + 
  ggtitle("Relative community composition")+
  theme_classic() + theme(plot.title = element_text(hjust=0.5, size=16, face="bold"),
                          axis.text.y = element_text(size=14, colour="black"),
                          axis.text.x = element_text(size=12, colour="black"),
                          strip.text.y = element_text(size=16, colour="black"),
                          axis.title = element_text(size=14, face="bold", colour="black"), legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 14, title = NULL))
##### Plot in relative ( do not run because it ruins what comes after) #####




# Plots of scleractinian and coral morphologies vs env data. 

# Work from Total_cover_data_coral_forms and Extra_environ

Extra_environ <- read.csv("~/Documents/AAASea_Science/AAA_PhD_Thesis/Environment_Pooled_Together/Extra_Information/Bathymetry_Coord_info.csv", sep=";")
# Remove the MArquises and Australes
remove <-  c ("MARFAT1", "MARFAT2", "MARHIV1", "MARHIV2", "MARTAH1", "MARTAH2", "AUSRAI1", "AUSRAI2")
Extra_environ <- subset(Extra_environ,!Site %in% remove)


unique (Extra_environ$Site)
# Set same site names
Extra_environ$Site <- gsub('SOCMOO1', 'Moorea S1', Extra_environ$Site)
Extra_environ$Site <- gsub('SOCMOO2', 'Moorea S2', Extra_environ$Site)
Extra_environ$Site <- gsub('SOCTAH1', 'Tahiti S1', Extra_environ$Site)
Extra_environ$Site <- gsub('SOCTAH2', 'Tahiti S2', Extra_environ$Site)
Extra_environ$Site <- gsub('SOCBOR1', 'Bora S1', Extra_environ$Site)
Extra_environ$Site <- gsub('SOCBOR2', 'Bora S2', Extra_environ$Site)
Extra_environ$Site <- gsub('TUATIK1', 'Tikehau S1', Extra_environ$Site)
Extra_environ$Site <- gsub('TUATIK2', 'Tikehau S2', Extra_environ$Site)
Extra_environ$Site <- gsub('TUARAN1', 'Rangiroa S1', Extra_environ$Site)
Extra_environ$Site <- gsub('TUARAN2', 'Rangiroa S2', Extra_environ$Site)
Extra_environ$Site <- gsub('TUARAR1', 'Raroia S1', Extra_environ$Site)
Extra_environ$Site <- gsub('TUARAR2', 'Raroia S2', Extra_environ$Site)
Extra_environ$Site <- gsub('TUAMAK1', 'Makatea S1', Extra_environ$Site)
Extra_environ$Site <- gsub('TUAMAK2', 'Makatea S2', Extra_environ$Site)
Extra_environ$Site <- gsub('GAMGAM1', 'Gambier S1', Extra_environ$Site)
Extra_environ$Site <- gsub('GAMGAM2', 'Gambier S2', Extra_environ$Site)


colnames(Total_cover_data_coral_forms)[1] <- "Site"
unique (Total_cover_data_coral_forms$Site)

Total_cover_data_coral_forms$Site <- gsub('Moorea_1', 'Moorea S1', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Moorea_2', 'Moorea S2', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Tahiti_1', 'Tahiti S1', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Tahiti_2', 'Tahiti S2', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Bora_1', 'Bora S1', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Bora_2', 'Bora S2', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Tikehau_1', 'Tikehau S1', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Tikehau_2', 'Tikehau S2', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Rangiroa_1', 'Rangiroa S1', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Rangiroa_2', 'Rangiroa S2', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Raroia_1', 'Raroia S1', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Raroia_2', 'Raroia S2', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Makatea_1', 'Makatea S1', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Makatea_2', 'Makatea S2', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Mangareva_1', 'Gambier S1', Total_cover_data_coral_forms$Site)
Total_cover_data_coral_forms$Site <- gsub('Mangareva_2', 'Gambier S2', Total_cover_data_coral_forms$Site)





Total_cover_data_coral_forms_environment <- merge (Total_cover_data_coral_forms,Extra_environ, by = c("Site", "Depth"))


Total_cover_data_coral_forms_environment <- select (Total_cover_data_coral_forms_environment, Site, Depth, Category, Cover, Cover_se, Cover_total, Cover_total_se, Bathymetry_slope, Dominant.Substrate, Dominant.benthic.non_coral, Generic_richness)

colnames (Total_cover_data_coral_forms_environment) <- c("Site", "Depth", "Category", "Category_cover", "Category_cover_se", "Total_cover", "Total_cover_se", "Bathymetry_slope", "Dominant.Substrate", "Dominant.benthic.non_coral", "Generic_richness")

# Make single plots of Total scleractinian cover in function of slope, substrate etc. 

# slope
ggplot(Total_cover_data_coral_forms_environment, aes(x = Bathymetry_slope, y = Total_cover)) + 
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.alpha = 0.1) + 
  geom_point(aes(x = Bathymetry_slope, y = Total_cover, colour = as.factor (Site), fill = as.factor (Site))) +  
  facet_wrap(~Depth, ncol = 3) +
  xlab ("Slope") + ylab ("Coral cover (%)") + 
  theme( axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold"))+
  theme_bw() +  theme(legend.position="bottom") 


ggplot(Total_cover_data_coral_forms_environment, aes(x = Bathymetry_slope, y = Total_cover)) + 
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.alpha = 0.1) + 
  geom_point(aes(x = Bathymetry_slope, y = Total_cover, colour = as.factor (Depth), fill = as.factor (Depth))) +  
  facet_wrap(~Site, ncol = 8) +
  xlab ("Slope") + ylab ("Coral cover (%)") + 
  theme( axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold"))+
  theme_bw() +  theme(legend.position="bottom") 



ggplot(Total_cover_data_coral_forms_environment, aes(x = Bathymetry_slope, y = Total_cover, colour = as.factor (Depth), fill = as.factor (Depth))) +
  geom_point()+  
  facet_wrap(~Site, ncol = 4) +   xlab ("Slope") + ylab ("Coral cover (%)")+
  theme( axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold"))+
  theme_bw() +  theme(legend.position="bottom")

# Susbtrate
ggplot(Total_cover_data_coral_forms_environment, aes(x = Dominant.Substrate, y = Total_cover)) + 
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.alpha = 0.1) + 
  geom_point(aes(x = Dominant.Substrate, y = Total_cover, colour = as.factor (Site), fill = as.factor (Site))) +  
  facet_wrap(~Depth, ncol = 3) +
  xlab ("Dominant.Substrate") + ylab ("Coral cover (%)") + 
  theme( axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold"))+
  theme_bw() +  theme(legend.position="bottom") 


ggplot(Total_cover_data_coral_forms_environment, aes(x = Dominant.Substrate, y = Total_cover, colour = as.factor (Depth), fill = as.factor (Depth))) +
  geom_point()+  
  facet_wrap(~Site, ncol = 4) +   xlab ("Substrate") + ylab ("Coral cover (%)")+
  theme( axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold"))+
  theme_bw() +  theme(legend.position="bottom")


# Dominant benthic non coral
ggplot(Total_cover_data_coral_forms_environment, aes(x = Dominant.benthic.non_coral, y = Total_cover)) + 
  geom_boxplot(fill = "white", colour = "#3366FF", outlier.alpha = 0.1) + 
  #  geom_point(aes(x = Dominant.benthic.non_coral, y = Total_cover, colour = as.factor (Site), fill = as.factor (Site))) +  
  facet_wrap(~Depth, ncol = 3) +
  xlab ("Dominant.Substrate non coral") + ylab ("Coral cover (%)") + 
  theme( axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold"))+
  theme_bw() +  theme(legend.position="bottom") 




# PCA of the different sites according to total cover and the ennvironment
Loggers_data_site <- read.csv("~/Documents/AAASea_Science/AAA_PhD_Thesis/Environment_Pooled_Together/Env_data/Loggers_data_site.csv", sep=",")

Loggers_data_site <- select (Loggers_data_site, Site, Depth, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind)

unique (Loggers_data_site$Site)
# Set same site names
Loggers_data_site$Site <- gsub('SOCMOO1', 'Moorea S1', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('SOCMOO2', 'Moorea S2', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('SOCTAH1', 'Tahiti S1', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('SOCTAH2', 'Tahiti S2', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('SOCBOR1', 'Bora S1', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('SOCBOR2', 'Bora S2', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('TUATIK1', 'Tikehau S1', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('TUATIK2', 'Tikehau S2', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('TUARAN1', 'Rangiroa S1', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('TUARAN2', 'Rangiroa S2', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('TUARAR1', 'Raroia S1', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('TUARAR2', 'Raroia S2', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('TUAMAK1', 'Makatea S1', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('TUAMAK2', 'Makatea S2', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('GAMGAM1', 'Gambier S1', Loggers_data_site$Site)
Loggers_data_site$Site <- gsub('GAMGAM2', 'Gambier S2', Loggers_data_site$Site)


# Make all the data together
Total_cover_sites_environment_PCA <- merge (Total_cover_data_coral_forms_environment,Loggers_data_site, by = c("Site", "Depth"))

# Select only total coral cover and delete duplicated rows
Total_cover_sites_environment_PCA <- select (Total_cover_sites_environment_PCA, Site, Depth, Total_cover, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind)

# Set the rownames and put in format to PCA deleting duplicated
Total_cover_sites_environment_PCA$ID<- with(Total_cover_sites_environment_PCA, paste0(Depth, sep = "_",  Site))
Total_cover_sites_environment_PCA <- Total_cover_sites_environment_PCA[!duplicated(Total_cover_sites_environment_PCA), ]
rownames(Total_cover_sites_environment_PCA) <- Total_cover_sites_environment_PCA[,"ID"]
Total_cover_sites_environment_PCA_num <-  select (Total_cover_sites_environment_PCA, Total_cover, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind)
# Remove NA
Total_cover_sites_environment_PCA_num <- na.omit(Total_cover_sites_environment_PCA_num)


# Make the PCA plot
pca_site <- dudi.pca(Total_cover_sites_environment_PCA_num, scale = T)   # scale = T (mean/stand.dev) because data is not on the same scale units
# Select 2
inertia.dudi(pca_site,col = TRUE)    # Information of variables 
pca_site$co

# Correlation of variables with axis
# Temp Variability, Total cover, and light rel well correlated with axis 1
# Temp rel more correlated with axis 2

# Now Graph
s.corcircle(pca_site$co, xax=1, yax=2, box = T)

# Graphic interpretation: 
# Light relative and total cover well correlated

# Representation of individual (individually)
inertia.dudi(pca_site, row = TRUE)

# Coordinate of individuals
pca_site$li

# Inertia and contributions
# Look at graphic of contributions
contribution <- inertia.dudi(pca_site, row = T)

# Parallel plots
plot(contribution$row.abs)
# s.label(contribution$row.abs)

# Representation of individuals
s.label (pca_site$li, clab = 0, xax = 1, yax =2, sub = "axe1 vs axe2")     # clab=1 gives labels
# s.label (pca$li, clab = 0, xax = 1, yax =3, sub = "axe1 vs axe3")      # With three axes

# Set colors of different depths and eventually labels of sites
# Extract depths from the rownames
Total_cover_sites_environment_PCA_num$Depth = sapply(strsplit(rownames(Total_cover_sites_environment_PCA_num), "_"), function(x) x[1])

# Set colors
# unique(Total_cover_sites_environment_PCA_num$Depth)

gcol <- c("black",  "aquamarine2","deepskyblue","wheat","blue","navyblue")
Total_cover_sites_environment_PCA_num$Depth <- as.factor (Total_cover_sites_environment_PCA_num$Depth)

# Plot
s.class (pca_site$li, fac = Total_cover_sites_environment_PCA_num [,5], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2*pca_site$co, xax = 1, yax = 2, clab = 1.5,  add.plot = T, boxes = FALSE) # Add the variables
# 5*pca$co multiplicate the arrows for 5


groups <- as.factor(Total_cover_sites_environment_PCA_num$Depth)
s.class(pca_site$li,
        fac = groups,  # color by groups
        col = c("black",  "aquamarine2","deepskyblue","wheat","blue","navyblue"), 
        add.plot = TRUE,         # Add onto the scatter plot
        cstar = 0,               # Remove stars
        cellipse = 0             # Remove ellipses
)



# Biplot
Total_cover_sites_environment_PCA_num$Depth = factor(Total_cover_sites_environment_PCA_num$Depth,levels = c ("6","20","40","60","90","120"))

fviz_pca_biplot(pca_site, repel = TRUE, habillage=Total_cover_sites_environment_PCA_num$Depth,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  


# Make the Biplot with slope, substrate etc.
# Extract slopes ()

Total_cover_sites_environment_PCA_num$Site = sapply(strsplit(rownames(Total_cover_sites_environment_PCA_num), "_"), function(x) x[2])

Environment_PCA <- merge (Total_cover_data_coral_forms_environment,Loggers_data_site, by = c("Site", "Depth"))
Environment_PCA <- select (Environment_PCA, Site, Depth,Bathymetry_slope, Dominant.Substrate, Dominant.benthic.non_coral)

# Combine into a new dataframe
Total_cover_sites_environment_PCA_num_norownames <- merge (Total_cover_sites_environment_PCA_num,Environment_PCA,  by = c("Site", "Depth"))
# Necessary to re-delete duplicated
Total_cover_sites_environment_PCA_num_norownames <- Total_cover_sites_environment_PCA_num_norownames[!duplicated(Total_cover_sites_environment_PCA_num_norownames), ]



# Slope biplot
fviz_pca_biplot(pca_site, repel = TRUE, habillage=Total_cover_sites_environment_PCA_num_norownames$Bathymetry_slope,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  

# substrate biplot
fviz_pca_biplot(pca_site, repel = TRUE, habillage=Total_cover_sites_environment_PCA_num_norownames$Dominant.Substrate,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  



# Make the PCA biplot but without Total cover / only environment

pca_site2 <- dudi.pca(Total_cover_sites_environment_PCA_num[,c(2,3,4)], scale = T)   # scale = T (mean/stand.dev) because data is not on the same scale units
# Select 2
inertia.dudi(pca_site2,col = TRUE)    # Information of variables 
pca_site2$co

# Biplot
Total_cover_sites_environment_PCA_num_norownames$Depth = factor(Total_cover_sites_environment_PCA_num_norownames$Depth,levels = c ("6","20","40","60","90","120"))

fviz_pca_biplot(pca_site2, labelsize = 2, geom = "point", repel = T, habillage=Total_cover_sites_environment_PCA_num_norownames$Depth,
                addEllipses=F, ellipse.level=0.95,palette = c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black"),
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12))

pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/PCA_Biplot_env_Depths.pdf", 15, 8)
fviz_pca_biplot(pca_site2, labelsize = 2,geom = "text", repel = T, habillage=Total_cover_sites_environment_PCA_num_norownames$Depth,
                addEllipses=F, ellipse.level=0.95,palette = c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black"),
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12), 
            legend.position = "bottom")
dev.off()





# Slope biplot
fviz_pca_biplot(pca_site2, labelsize = 2, geom = "text",repel = T, habillage=Total_cover_sites_environment_PCA_num_norownames$Bathymetry_slope,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  + theme(text = element_text(size = 12),
           axis.title = element_text(size = 12),
           axis.text = element_text(size = 12))
pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/PCA_Biplot_env_Slope.pdf", 15, 8)
fviz_pca_biplot(pca_site2, labelsize = 2, geom = "text",repel = T, habillage=Total_cover_sites_environment_PCA_num_norownames$Bathymetry_slope,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  + theme(text = element_text(size = 12),
           axis.title = element_text(size = 12),
           axis.text = element_text(size = 12), 
           legend.position = "bottom")
dev.off()



# substrate biplot
fviz_pca_biplot(pca_site2, labelsize = 2, geom = "text",repel = T, habillage=Total_cover_sites_environment_PCA_num_norownames$Dominant.Substrate,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12))

pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/PCA_Biplot_env_Substrate.pdf", 15, 8)
fviz_pca_biplot(pca_site2, labelsize = 2, geom = "text",repel = T, habillage=Total_cover_sites_environment_PCA_num_norownames$Dominant.Substrate,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12), 
            legend.position = "bottom")
dev.off()


# Make the graph of the ellipses for the PCA of only environment

# Set colors of different depths and eventually labels of sites
# Set colors
# unique(Total_cover_sites_environment_PCA_num$Depth)
gcol <- c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black")
Total_cover_sites_environment_PCA_num$Depth <- as.factor (Total_cover_sites_environment_PCA_num$Depth)

# Plot
s.class (pca_site2$li, fac = Total_cover_sites_environment_PCA_num [,5], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2*pca_site2$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = FALSE) # Add the variables
# 5*pca$co multiplicate the arrows for 5

groups <- as.factor(Total_cover_sites_environment_PCA_num$Depth)
s.class(pca_site2$li,
        fac = groups,  # color by groups
        col = c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black"), 
        add.plot = TRUE,         # Add onto the scatter plot
        cstar = 0,               # Remove stars
        cellipse = 0             # Remove ellipses
)

# Plot (column 10 is the extracted depth!)

# Save this plot "Sclass_Sarrow_Abiotic_Env.pdf"
pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/Sclass_Sarrow_Abiotic_Env.pdf", 10, 8)
s.class (pca_site2$li, fac = Total_cover_sites_environment_PCA_num [,5], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2.25*pca_site2$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = F) # Add the variables
dev.off()





### Make the same  with the different coral morphologies and coral cover, all at the same time!
Total_cover_forms_sites_environment_PCA <- merge (Total_cover_data_coral_forms_environment,Loggers_data_site, by = c("Site", "Depth"))

# Select all columns that you want 
Total_cover_forms_sites_environment_PCA <- select (Total_cover_forms_sites_environment_PCA, Site, Depth, Category, Category_cover, Total_cover, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind)


# Add columns for the different forms also 
Total_cover_forms_sites_environment_PCA = melt(Total_cover_forms_sites_environment_PCA, id=c("Site", "Depth", "Total_cover", "Light_Rel_Ind", "Temp_Variability", "Temp_Rel_Ind", "Category"), measure.vars="Category_cover", na.rm=FALSE)

Total_cover_forms_sites_environment_PCA = dcast(Total_cover_forms_sites_environment_PCA, Site + Depth + Total_cover +Light_Rel_Ind + Temp_Variability + Temp_Rel_Ind ~ Category, mean, add.missing = T)


# Set the rownames and put in format to PCA deleting duplicated
Total_cover_forms_sites_environment_PCA$ID<- with(Total_cover_forms_sites_environment_PCA, paste0(Depth, sep = "_",  Site))
Total_cover_forms_sites_environment_PCA <- Total_cover_forms_sites_environment_PCA[!duplicated(Total_cover_forms_sites_environment_PCA), ]
rownames(Total_cover_forms_sites_environment_PCA) <- Total_cover_forms_sites_environment_PCA[,"ID"]
# Keep all coral forms, total cover, etc. (This what you will need to change after)
Total_cover_forms_sites_environment_PCA_num <-  select (Total_cover_forms_sites_environment_PCA, Total_cover, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind, Branching,Massive,Encrusting,Solitary,Laminar)
# Remove NA
Total_cover_forms_sites_environment_PCA_num <- na.omit(Total_cover_forms_sites_environment_PCA_num)


# Make the PCA plot
pca_site <- dudi.pca(Total_cover_forms_sites_environment_PCA_num, scale = T)   
# Select variables = 2
inertia.dudi(pca_site,col = TRUE)    
pca_site$co

# Correlation of variables with axis

# First graph of variables!
s.corcircle(pca_site$co, xax=1, yax=2, box = T)
# Save this PDF "Cocircle_Coralforms_Total_Num_Env.pdf"
# pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/Cocircle_Coralforms_Total_Num_Env.pdf", 15, 15)
s.corcircle(pca_site$co, xax=1, yax=2, box = T)
# dev.off()
# Save this PDF "Cocircle_Coralforms_Total_Num_Env.pdf"

# Graphic interpretation: 
# Light and branching well correlated! 
# Total cover is correlated with all coral forms, especially with encrusting!

# Representation of individual (individually)
inertia.dudi(pca_site, row = TRUE)
# Coordinate of individuals
pca_site$li

# Inertia and contributions
contribution <- inertia.dudi(pca_site, row = T)
# Parallel plots (we cannot see anything, so do not run!)
# plot(contribution$row.abs)
# s.label(contribution$row.abs)

# Representation of individuals
s.label (pca_site$li, clab = 0, xax = 1, yax =2, sub = "axe1 vs axe2")     # clab=1 gives labels
# s.label (pca$li, clab = 0, xax = 1, yax =3, sub = "axe1 vs axe3")      # With three axes

# Set colors of different depths and eventually labels of sites
# Extract depths from the rownames
Total_cover_forms_sites_environment_PCA_num$Depth = sapply(strsplit(rownames(Total_cover_forms_sites_environment_PCA_num), "_"), function(x) x[1])

# Set colors
unique(Total_cover_forms_sites_environment_PCA_num$Depth)
gcol <- c("black",  "aquamarine2","deepskyblue","wheat","blue","navyblue")
Total_cover_forms_sites_environment_PCA_num$Depth <- as.factor (Total_cover_forms_sites_environment_PCA_num$Depth)

# Plot (column 10 is the extracted depth!)
s.class (pca_site$li, fac = Total_cover_forms_sites_environment_PCA_num [,10], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2.5*pca_site$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = F) # Add the variables

# Save this plot "Sclass_Sarrow_Coralforms_Total_Num_Env.pdf"
# pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/Sclass_Sarrow_Coralforms_Total_Num_Env.pdf", 10, 8)
s.class (pca_site$li, fac = Total_cover_forms_sites_environment_PCA_num [,10], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2.5*pca_site$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = F) # Add the variables
# dev.off()

# 5*pca$co multiplicate the arrows for 5
# This plot starts to look very interesting!

groups <- as.factor(Total_cover_forms_sites_environment_PCA_num$Depth)
s.class(pca_site$li,
        fac = groups,  # color by groups
        col = c("black",  "aquamarine2","deepskyblue","wheat","blue","navyblue"), 
        add.plot = TRUE,         # Add onto the scatter plot
        cstar = 0,               # Remove stars
        cellipse = 0             # Remove ellipses
)



# Biplot
Total_cover_forms_sites_environment_PCA_num$Depth = factor(Total_cover_forms_sites_environment_PCA_num$Depth,levels = c ("6","20","40","60","90","120"))




fviz_pca_biplot(pca_site, labelsize = 2, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num$Depth,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  + theme(text = element_text(size = 10),
           axis.title = element_text(size = 10),
           axis.text = element_text(size = 10))

# label = "none"
# label = "var"



# Save
# pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/PCA_Biplot_Depths.pdf", 15, 8)
fviz_pca_biplot(pca_site, labelsize = 2, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num$Depth,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  + theme(text = element_text(size = 12),
           axis.title = element_text(size = 12),
           axis.text = element_text(size = 12))
# dev.off()

# Make the Biplot with slope, substrate etc.
# Extract slopes and other environmental data

Total_cover_forms_sites_environment_PCA_num$Site = sapply(strsplit(rownames(Total_cover_forms_sites_environment_PCA_num), "_"), function(x) x[2])

Environment_PCA <- merge (Total_cover_data_coral_forms_environment,Loggers_data_site, by = c("Site", "Depth"))
Environment_PCA <- select (Environment_PCA, Site, Depth,Bathymetry_slope, Dominant.Substrate, Dominant.benthic.non_coral)

# Combine into a new dataframe
Total_cover_forms_sites_environment_PCA_num_norownames <- merge (Total_cover_forms_sites_environment_PCA_num,Environment_PCA,  by = c("Site", "Depth"))
# Necessary to re-delete duplicated
Total_cover_forms_sites_environment_PCA_num_norownames <- Total_cover_forms_sites_environment_PCA_num_norownames[!duplicated(Total_cover_forms_sites_environment_PCA_num_norownames), ]



# Slope biplot
fviz_pca_biplot(pca_site, labelsize = 2, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Bathymetry_slope,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
) + theme(text = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12))



# Save
# pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/PCA_Biplot_Slope.pdf", 15, 8)
fviz_pca_biplot(pca_site, labelsize = 2, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Bathymetry_slope,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
) + theme(text = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12))
# dev.off()

# substrate biplot
fviz_pca_biplot(pca_site, labelsize = 2, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Dominant.Substrate,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  + theme(text = element_text(size = 12),
           axis.title = element_text(size = 12),
           axis.text = element_text(size = 12))

# Save
# pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/PCA_Biplot_Substrate.pdf", 15, 8)
fviz_pca_biplot(pca_site, labelsize = 2, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Dominant.Substrate,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  + theme(text = element_text(size = 12),
           axis.title = element_text(size = 12),
           axis.text = element_text(size = 12))
# dev.off()



# Make the PCA biplot but without Total cover and environment (only coral morphologies ) - NEW REQUEST
# Make the same but with the different coral morphologies (not total coral cover) 
pca_morpho <- dudi.pca(Total_cover_forms_sites_environment_PCA_num[,c(5,6,7,8,9)], scale = T)   # scale = T (mean/stand.dev) because data is not on the same scale units
# Select 2
inertia.dudi(pca_morpho,col = TRUE)    # Information of variables 
pca_morpho$co

# Extract depth from rownames
Total_cover_forms_sites_environment_PCA_num$Depth = sapply(strsplit(rownames(Total_cover_forms_sites_environment_PCA_num), "_"), function(x) x[1])

# Biplot
Total_cover_forms_sites_environment_PCA_num$Depth = factor(Total_cover_forms_sites_environment_PCA_num$Depth,levels = c ("6","20","40","60","90","120"))

fviz_pca_biplot(pca_morpho, repel = T, labelsize = 2,geom = "text", habillage=Total_cover_forms_sites_environment_PCA_num$Depth,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12))

pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/PCA_Biplot_morpho_Depths.pdf", 15, 8)
fviz_pca_biplot(pca_morpho, repel = T, labelsize = 2,geom = "text", habillage=Total_cover_forms_sites_environment_PCA_num$Depth,
                addEllipses=F, ellipse.level=0.95,palette = c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black"),
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            legend.position = "bottom")
dev.off()





Environment_PCA <- merge (Total_cover_data_coral_forms_environment,Loggers_data_site, by = c("Site", "Depth"))
Environment_PCA <- select (Environment_PCA, Site, Depth,Bathymetry_slope, Dominant.Substrate, Dominant.benthic.non_coral)

# Combine into a new dataframe
Total_cover_forms_sites_environment_PCA_num_norownames <- merge (Total_cover_forms_sites_environment_PCA_num,Environment_PCA,  by = c("Site", "Depth"))
# Necessary to re-delete duplicated
Total_cover_forms_sites_environment_PCA_num_norownames <- Total_cover_forms_sites_environment_PCA_num_norownames[!duplicated(Total_cover_forms_sites_environment_PCA_num_norownames), ]



# Slope biplot
fviz_pca_biplot(pca_morpho, repel = F, labelsize =2, geom = "text",habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Bathymetry_slope,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12))

pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/PCA_Biplot_morpho_Slopes.pdf", 15, 8)
fviz_pca_biplot(pca_morpho, repel = T, labelsize =2, geom = "text",habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Bathymetry_slope,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            legend.position = "bottom")
dev.off()



# substrate biplot
fviz_pca_biplot(pca_morpho, repel = F, labelsize =2, geom = "text",habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Dominant.Substrate,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12))

pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/PCA_Biplot_morpho_Substrate.pdf", 15, 8)
fviz_pca_biplot(pca_morpho, repel = T, labelsize =2, geom = "text",habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Dominant.Substrate,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            legend.position = "bottom")
dev.off()



# Set colors of different depths and eventually labels of sites
gcol <- c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black")
Total_cover_forms_sites_environment_PCA_num_norownames$Depth <- as.factor (Total_cover_forms_sites_environment_PCA_num_norownames$Depth)

# Plot
s.class (pca_morpho$li, fac = Total_cover_forms_sites_environment_PCA_num_norownames [,2], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2*pca_morpho$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = FALSE) # Add the variables
# 5*pca$co multiplicate the arrows for 5

groups <- as.factor(Total_cover_forms_sites_environment_PCA_num_norownames$Depth)
s.class(pca_morpho$li,
        fac = groups,  # color by groups
        col = c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black"), 
        add.plot = TRUE,         # Add onto the scatter plot
        cstar = 0,               # Remove stars
        cellipse = 0             # Remove ellipses
)

# Plot (column 10 is the extracted depth!)

# Save this plot "Sclass_Sarrow_Abiotic_Env.pdf"
pdf("~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/Sclass_Sarrow_Biotic_Forms.pdf", 10, 8)
s.class (pca_morpho$li, fac = Total_cover_forms_sites_environment_PCA_num_norownames [,2], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2.25*pca_morpho$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = FALSE) # Add the variables
dev.off()





#Running PERMANOVA to check dispersion of morphologies - You are keeping this section! IMPORTANT
cover_st2.dist <- vegdist(Total_cover_forms_sites_environment_PCA_num [,c(5:9)], method="euclidean")

#Create factors
# cover_st2.env <- as.data.frame(row.names(cover_st2)) ##### had to replace this line by below because otherwise there was an issue
cover_st2 <- as.data.frame(row.names(Total_cover_forms_sites_environment_PCA_num))
colnames(cover_st2)[1] <- "ID"
# Extract depths
cover_st2$Depth <- sub("\\_.*", "", cover_st2$ID)
# Extract Island 
cover_st2$Island_Site <- sapply(strsplit(cover_st2$ID, "_"), function(x) x[2])

# MEasure dispersions
disp_morpho_Depth <- betadisper(cover_st2.dist, group=cover_st2$Depth)  
permutest(disp_morpho_Depth)











### Make the same but only with laminar corals and environment (and nothing else)

pca_site_laminar <- dudi.pca(Total_cover_forms_sites_environment_PCA_num[,c(2,3,4,9)], scale = T)   # scale = T (mean/stand.dev) because data is not on the same scale units
# Select 2
inertia.dudi(pca_site_laminar,col = TRUE)    # Information of variables 
pca_site_laminar$co

# Biplot
Total_cover_forms_sites_environment_PCA_num_norownames$Depth = factor(Total_cover_forms_sites_environment_PCA_num_norownames$Depth,levels = c ("6","20","40","60","90","120"))

fviz_pca_biplot(pca_site_laminar, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Depth,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  




# Slope biplot
fviz_pca_biplot(pca_site_laminar, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Bathymetry_slope,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  

# substrate biplot
fviz_pca_biplot(pca_site_laminar, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Dominant.Substrate,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  



########3 Quick test with generic richness!  probably not keepint it ##########3

Total_cover_forms_sites_environment_PCA <- merge (Total_cover_data_coral_forms_environment,Loggers_data_site, by = c("Site", "Depth"))
# Add also richness
Total_cover_forms_sites_environment_PCA <- select (Total_cover_forms_sites_environment_PCA, Site, Depth, Category, Category_cover, Total_cover, Generic_richness, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind)
# Add columns for the different forms also 
Total_cover_forms_sites_environment_PCA = melt(Total_cover_forms_sites_environment_PCA, id=c("Site", "Depth", "Total_cover", "Generic_richness","Light_Rel_Ind", "Temp_Variability", "Temp_Rel_Ind", "Category"), measure.vars="Category_cover", na.rm=FALSE)
Total_cover_forms_sites_environment_PCA = dcast(Total_cover_forms_sites_environment_PCA, Site + Depth + Total_cover + Generic_richness + Light_Rel_Ind + Temp_Variability + Temp_Rel_Ind ~ Category, mean, add.missing = T)
# Set the rownames and put in format to PCA deleting duplicated
Total_cover_forms_sites_environment_PCA$ID<- with(Total_cover_forms_sites_environment_PCA, paste0(Depth, sep = "_",  Site))
Total_cover_forms_sites_environment_PCA <- Total_cover_forms_sites_environment_PCA[!duplicated(Total_cover_forms_sites_environment_PCA), ]
rownames(Total_cover_forms_sites_environment_PCA) <- Total_cover_forms_sites_environment_PCA[,"ID"]
# Keep all coral forms, total cover, etc. (This what you will need to change after)
Total_cover_forms_sites_environment_PCA_num <-  select (Total_cover_forms_sites_environment_PCA, Total_cover, Generic_richness,Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind, Branching,Massive,Encrusting,Solitary,Laminar)
# Remove NA
Total_cover_forms_sites_environment_PCA_num <- na.omit(Total_cover_forms_sites_environment_PCA_num)
# Make the PCA plot
pca_site <- dudi.pca(Total_cover_forms_sites_environment_PCA_num, scale = T)   
# Extract depths from the rownames
Total_cover_forms_sites_environment_PCA_num$Depth = sapply(strsplit(rownames(Total_cover_forms_sites_environment_PCA_num), "_"), function(x) x[1])
# Set colors
unique(Total_cover_forms_sites_environment_PCA_num$Depth)
gcol <- c("black",  "aquamarine2","deepskyblue","wheat","blue","navyblue")
Total_cover_forms_sites_environment_PCA_num$Depth <- as.factor (Total_cover_forms_sites_environment_PCA_num$Depth)
Total_cover_forms_sites_environment_PCA_num$Site = sapply(strsplit(rownames(Total_cover_forms_sites_environment_PCA_num), "_"), function(x) x[2])
# Add the environmental data
Environment_PCA <- merge (Total_cover_data_coral_forms_environment,Loggers_data_site, by = c("Site", "Depth"))
Environment_PCA <- select (Environment_PCA, Site, Depth,Bathymetry_slope, Dominant.Substrate, Dominant.benthic.non_coral)
# Combine into a new dataframe
Total_cover_forms_sites_environment_PCA_num_norownames <- merge (Total_cover_forms_sites_environment_PCA_num,Environment_PCA,  by = c("Site", "Depth"))
# Necessary to re-delete duplicated
Total_cover_forms_sites_environment_PCA_num_norownames <- Total_cover_forms_sites_environment_PCA_num_norownames[!duplicated(Total_cover_forms_sites_environment_PCA_num_norownames), ]
# MEasure PCA
pca_site_richness <- dudi.pca(Total_cover_forms_sites_environment_PCA_num[,c(1,2,3,4,5,6,7,8,9,10)], scale = T)   # scale = T (mean/stand.dev) because data is not on the same scale units
# Select 2
inertia.dudi(pca_site_richness,col = TRUE)    # Information of variables 
pca_site_richness$co
# Biplot
Total_cover_forms_sites_environment_PCA_num_norownames$Depth = factor(Total_cover_forms_sites_environment_PCA_num_norownames$Depth,levels = c ("6","20","40","60","90","120"))
fviz_pca_biplot(pca_site_richness, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Depth,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  
# Only total coral cover according to richness
pca_site_richness2 <- dudi.pca(Total_cover_forms_sites_environment_PCA_num[,c(1,2)], scale = T)   # scale = T (mean/stand.dev) because data is not on the same scale units
Total_cover_forms_sites_environment_PCA_num_norownames$Depth = factor(Total_cover_forms_sites_environment_PCA_num_norownames$Depth,levels = c ("6","20","40","60","90","120"))
fviz_pca_biplot(pca_site_richness2, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Depth,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  
# Only laminar coral cover according to richness
pca_site_richness3 <- dudi.pca(Total_cover_forms_sites_environment_PCA_num[,c(2,10)], scale = T)   # scale = T (mean/stand.dev) because data is not on the same scale units
Total_cover_forms_sites_environment_PCA_num_norownames$Depth = factor(Total_cover_forms_sites_environment_PCA_num_norownames$Depth,levels = c ("6","20","40","60","90","120"))
fviz_pca_biplot(pca_site_richness3, repel = TRUE, habillage=Total_cover_forms_sites_environment_PCA_num_norownames$Depth,
                addEllipses=F, ellipse.level=0.95,
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)  


########3 Quick test with generic richness!  probably not keepint it ##########3








### Different kind of PCA but looking at the coral cover

res.pca_site <- PCA(Total_cover_sites_environment_PCA_num, quanti.sup = 1, quali.sup = 5)

barplot(res.pca_site$eig[,2],main="Eigenvalues",names.arg=1:nrow(res.pca_site$eig))
summary(res.pca_site)

plot(res.pca_site,choix="ind",habillage=6)
dimdesc(res.pca_site, axes = 1:2)
plotellipses(res.pca_site,6)







# All the different benthic groups, suggested by Ale and Pim

####### Benthic groups per depth

allbenthosdata <- read.csv(file = "~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/Species_distribution_model/R_Data_Codes/allbenthosdata.csv", header = T, dec = ".", sep = ",", row.names = 1)


# Working from View (data)
# Very important to complete to consider 0 before doing the mean
allbenthosdata <- allbenthosdata %>% complete( Island,Island_Site,Depth,Quadrat,Category,fill = list(Cover = 0))

# Tranform forms into Scleractinian
Benthos_data <- allbenthosdata

Benthos_data$Category <- gsub('Scleractinian_branching', 'Scleractinian', Benthos_data$Category)
Benthos_data$Category <- gsub('Scleractinian_massive', 'Scleractinian', Benthos_data$Category)
Benthos_data$Category <- gsub('Scleractinian_encrusting', 'Scleractinian', Benthos_data$Category)
Benthos_data$Category <- gsub('Scleractinian_solitary', 'Scleractinian', Benthos_data$Category)
Benthos_data$Category <- gsub('Scleractinian_laminar', 'Scleractinian', Benthos_data$Category)


Benthos_data <- ddply(Benthos_data, ~ Island + Island_Site + Depth + Quadrat +  Category, function(x){c(Cover = sum(x$Cover))}) # Sum all Scleractinian forms 


Benthos_data <- ddply(Benthos_data, ~ Island + Island_Site + Depth + Category, function(x){c(Cover = mean(x$Cover), Sd = sd(x$Cover), Cover_se=sd(x$Cover) / sqrt(length(x$Cover)))}) # Cover of categories per site/depth. Mean, sd and se among the 30 quadrats

Benthos_data2 <- ddply(Benthos_data, ~ Depth + Category, function(x){c(Cover = mean(x$Cover), Cover_se=sd(x$Cover) / sqrt(length(x$Cover)))}) # Cover of categories per site/depth. Mean, sd and se among the 30 quadrats

Benthos_data  <- Benthos_data %>% unite(Island_Island_Site, Island, Island_Site, remove = T)

# Change names to match!
Benthos_data$Category <- gsub('Black_coral', 'Antipatharia', Benthos_data$Category)

keep_benthos <-  c ("Sponges","Gorgonia","Antipatharia","Scleractinian","CCA", "Fleshy macroalgae", "Hydroids", "Crustose algae", "Halimeda", "Fixed substrate","Sediment substrate","Rubble" )

Benthos_data <- subset(Benthos_data,Category %in% keep_benthos)


# Changes names
Benthos_data$Island_Island_Site <- gsub('Moorea_1', 'Moorea S1', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Moorea_2', 'Moorea S2', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Tahiti_1', 'Tahiti S1', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Tahiti_2', 'Tahiti S2', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Bora_1', 'Bora S1', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Bora_2', 'Bora S2', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Tikehau_1', 'Tikehau S1', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Tikehau_2', 'Tikehau S2', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Rangiroa_1', 'Rangiroa S1', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Rangiroa_2', 'Rangiroa S2', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Raroia_1', 'Raroia S1', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Raroia_2', 'Raroia S2', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Makatea_1', 'Makatea S1', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Makatea_2', 'Makatea S2', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Mangareva_1', 'Gambier S1', Benthos_data$Island_Island_Site)
Benthos_data$Island_Island_Site <- gsub('Mangareva_2', 'Gambier S2', Benthos_data$Island_Island_Site)

Benthos_data$Island_Island_Site <- factor(Benthos_data$Island_Island_Site, levels = c("Moorea S1","Moorea S2","Tahiti S1","Tahiti S2","Bora S1","Bora S2","Tikehau S1","Tikehau S2","Rangiroa S1","Rangiroa S2","Raroia S1","Raroia S2","Makatea S1","Makatea S2","Gambier S1","Gambier S2"))



# Stack area plot
Benthos_data$Category = factor(Benthos_data$Category,levels = c ("Scleractinian","Halimeda","Fleshy macroalgae","CCA", "Crustose algae","Hydroids","Antipatharia","Gorgonia","Sponges","Fixed substrate","Sediment substrate","Rubble" ))



# colours <-brewer.pal(12, "Spectral")
# colours <- brewer.pal(n = 12, name = "Set1")
colours <- distinctColorPalette(12)
# colours <- c("#FF7F00","#377EB8", "#4DAF4A", "#E41A1C","#984EA3", "#999999", "#A65628",  "#FFFF33","#F781BF") # Edited from "Set1"

Benthos_vs_depth <- ggplot(Benthos_data, aes(x=Depth, y=Cover)) +
  geom_bar(aes(fill=factor(Category)), stat="identity") + 
  scale_fill_manual(values= colours) +
  facet_wrap(~Island_Island_Site,ncol = 4) + 
  scale_x_continuous(name ="Depth (m)", breaks = c(6,20,40,60,90,120)) +
  scale_y_continuous(name ="Benthos cover (%)", limits=c(-5,100), breaks = c(0,20,40,60,80)) +
  theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                          axis.text = element_text(size=8, colour="black"),
                          axis.title = element_text(size=12, face="bold", colour="black"), 
                          strip.text = element_text(size=10),
                          legend.title = element_blank(), legend.position = "bottom") 
Benthos_vs_depth

Benthos_data_Total <- aggregate(Cover ~  Island_Island_Site + Depth,  Benthos_data, sum)
Benthos_data <- merge (Benthos_data,Benthos_data_Total, by=c("Island_Island_Site","Depth"))
names (Benthos_data) <- c("Island_Island_Site","Depth", "Category","Category_cover","Category_cover_sd","Category_cover_se", "Total_cover")
# Calculation of Relative contribution of benthos to total
Benthos_data$Relative <- (Benthos_data$Category_cover/ Benthos_data$Total_cover)*100

# Stack plot
ggplot(Benthos_data, aes(x=Depth, y=Relative, fill=Category)) +
  scale_fill_manual(values= colours) +
  geom_area(alpha=1 , size=0.5, colour="black") + facet_wrap(~Island_Island_Site,ncol = 8) + 
  scale_x_reverse(lim=c(120,0), breaks = c(6,20,40,60,90,120)) +    coord_flip() + 
  ggtitle ("Relative percentatge of benthic cover") + xlab ("Depth (m)") + ylab ("") + labs(fill='Benthic categories', size = 17) +
  theme_classic() + theme(axis.title = element_text(size=17),
                          plot.title = element_text(size=20),
                          axis.text.y =  element_text(size=15),
                          axis.text.x =  element_blank(),
                          axis.ticks.x=element_blank(),
                          strip.text.x =  element_text(size = 15,colour = "black", angle = 0),
                          strip.text.y = element_text(size = 6),strip.text.y.left = element_text(angle=0, size = 15),
                          strip.background = element_rect(fill="grey"),
                          legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 8, title.hjust = 0.4))

colours <- distinctColorPalette(12)
colours <- c("#D6A46F","#7DB1D5","#7EE46A","#D6A0C5","#DADE53","#DC6DCE","#8584DA","#A446E2","#E45D64","gray65","gray80","gray50")

Benthic_cover_stack_rel <- ggplot(Benthos_data, aes(x=Island_Island_Site, y=Relative)) +
  geom_bar(aes(fill=factor(Category)), stat="identity", width = 0.6) +  facet_grid (rows = vars(Depth)) +
  scale_fill_manual(values= colours) +
  scale_y_continuous(position = "left",breaks = c(0,25,50,75,100))+ scale_x_discrete ()+ ylab ("Percentatge (%)") + xlab ("") + 
  ggtitle("")+
  theme_classic() + theme(plot.title = element_text(hjust=0.5, size=16, face="bold"),
                          axis.text.y = element_text(size=14, colour="black"),
                          axis.text.x = element_text(size=12, colour="black", angle = 90),
                          strip.text.x = element_text(angle = 0),
                          strip.text.y = element_text(size=16, colour="black"),
                          axis.title = element_text(size=14, face="bold", colour="black"), legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 14, title = NULL))
Benthic_cover_stack_rel
ggsave ( "~/Documents/AAASea_Science/AAA_PhD_Thesis/Photoquadrats/PhD_Cover_Depth/Benthic_cover_stack_rel.pdf", Benthic_cover_stack_rel,width = 14, height = 14)




##### Plot in relative ( do not run because it ruins what comes after) #####
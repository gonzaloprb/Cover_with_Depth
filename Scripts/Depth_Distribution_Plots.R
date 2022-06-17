library (dplyr); library(tidyverse); require (plyr); require (reshape2);  library (RColorBrewer); library (randomcoloR)

rm (list = ls ())

# This Scripts creates Fig. 3 and Sup. Fig. 5
#############################################


###Plot of morphologies per depth 

# Working from file "Living_Benthic_Categories_Data.csv"
data <- read.csv(file = "Data/Living_Benthic_Categories_Data.csv", header = T, dec = ".", sep = ",", row.names = 1) 

# Very important to complete to consider 0 before doing the mean
data <- data %>% complete( Island,Island_Site,Depth,Quadrat,Category,fill = list(Cover = 0))

Total_cover_data <- ddply(data, ~ Island + Island_Site + Depth + Category, function(x){c(Cover = mean(x$Cover), Sd = sd(x$Cover), Cover_se=sd(x$Cover) / sqrt(length(x$Cover)))}) # Cover of categories per site/depth. Mean, sd and se among the 30 quadrats
Total_cover_data  <- Total_cover_data %>% unite(Island_Island_Site, Island, Island_Site, remove = T)

# Keep only coral morphologies
keep <-  c ("Scleractinian_branching","Scleractinian_massive","Scleractinian_encrusting","Scleractinian_solitary","Scleractinian_laminar")
Total_cover_data_coral_forms <- subset(Total_cover_data,Category %in% keep)


# Measure cover and standard error for the site and deth of overall coral cover. 
Total_cover_data_se <- subset(data,Category %in% keep)

# Summ all forms by quadrats
Total_cover_data_se <- ddply(Total_cover_data_se, ~ Island +Island_Site + Depth + Quadrat , function(x){c(Cover = sum(x$Cover)) })
# make the mean of the 30 quadrats
Total_cover_data_se <- ddply(Total_cover_data_se, ~ Island + Island_Site + Depth , function(x){c(Cover_total = mean(x$Cover), Cover_total_se=sd(x$Cover) / sqrt(length(x$Cover)))}) # Cover of categories per site/depth. Mean, sd and se among the 30 quadrats
# Unite island and site
Total_cover_data_se  <- Total_cover_data_se %>% unite(Island_Island_Site, Island, Island_Site, remove = T)

# Merge the two dataframes
Total_cover_data_coral_forms <- merge (Total_cover_data_coral_forms,Total_cover_data_se, by = c("Island_Island_Site", "Depth"))


# Changes names to make format of Fig. 1 B to put together 
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

# Facet wrap representation
ggplot(Total_cover_data_coral_forms, aes(x=Depth, y=Cover)) +
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


# Add the error_bar and split by forms
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

ggplot(Total_cover_data_coral_forms, aes(x=Depth, y=Cover)) +
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

# 
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
ggsave ( "Outputs_R_Figures/Cover_form_per_depth_se.pdf", Cover_form_per_depth_se,width = 8, height = 8)

# This figure corresponds to Fig 3

# Save dataframe: 
write.csv (Total_cover_data_coral_forms,  "Data/Total_cover_data_coral_forms.csv")



### Plot of all benthic groups per depths / Scleractinia cover instead of morphologies

allbenthosdata <- read.csv(file = "Data/allbenthosdata.csv", header = T, dec = ".", sep = ",", row.names = 1)

# Complete to consider 0 before doing the mean
allbenthosdata <- allbenthosdata %>% complete( Island,Island_Site,Depth,Quadrat,Category,fill = list(Cover = 0))

# Transform forms into Scleractinian
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

# Change names to match with species distribution model plots
Benthos_data$Category <- gsub('Black_coral', 'Antipatharia', Benthos_data$Category)

keep_benthos <-  c ("Sponges","Gorgonia","Antipatharia","Scleractinian","CCA", "Fleshy macroalgae", "Hydroids", "Crustose algae", "Halimeda", "Fixed substrate","Sediment substrate","Rubble" )

Benthos_data <- subset(Benthos_data,Category %in% keep_benthos)


# Changes names to make format of Fig. 1 B
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


# Automatic set of colours to avoid changes 
colours <- c("#D6A46F","#7DB1D5","#7EE46A","#D6A0C5","#DADE53","#DC6DCE","#8584DA","#A446E2","#E45D64","gray65","gray80","gray50")


ggplot(Benthos_data, aes(x=Depth, y=Cover)) +
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
ggsave ( "Outputs_R_Figures/Benthic_cover_stack_rel.pdf", Benthic_cover_stack_rel,width = 14, height = 14)
# This figure corresponds to Supplementary figure 5







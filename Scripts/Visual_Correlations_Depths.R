

rm (list = ls())

# This script generates Sup. Fig. 2

# Correlation plots between the different depths

# Standard packages
library(tidyverse); require (plyr); require (reshape2); library (data.table); require  (stats); require (matrixStats);library (stringr)

# Plot packages
library (patchwork); library (scales); library(RColorBrewer);  library(randomcoloR); library(viridisLite); 
library (cowplot); library(ggridges)

# Correlation packages
library (corrplot); library("Hmisc")


# Open database
data <- read.csv(file = "Data/rndpts_genus_form.csv", header = T, dec = ".", sep = ";", row.names = 1)

# Necessary data transformations
data$Island <- gsub("Mangareva", "Gambier", data$Island)

# Create a database to keep all quadrats with only corals / fix the genus/form issue, aggregate
rndpts_df <- data
rndpts_df <- filter(rndpts_df, Category == "Scleractinian") 
rndpts_df <- rndpts_df %>% separate(Coral_Genus_Form,  c("Coral_genus", "Coral_form"), " ", extra = "merge")
rndpts_df <- aggregate (Cover ~ Unique_Image_ID + Date + Site + Archipelago + Island + Island_Site + Depth + Quadrat + Category +  Coral_genus, rndpts_df , sum)
# This data is ready

# These databases are necessary to be created
rndpts_df <- rndpts_df %>% complete( Island,Island_Site,Depth,Quadrat,fill = list(Cover = 0))
genus_cover_site <- ddply(rndpts_df, ~ Archipelago ~ Island +Island_Site + Depth + Category + Coral_genus, function(x){c(Cover = sum(x$Cover)/30) })

coral_cover_site  <- ddply(genus_cover_site, ~ Island +Island_Site + Depth , function(x){c(Cover = sum(x$Cover), Sd = sd(x$Cover), se=sd(x$Cover) / sqrt(length(x$Cover))) })
coral_cover_site[is.na(coral_cover_site)] <- 0

coral_cover_Island_Site  <- coral_cover_site %>% unite(Island_Island_Site, Island, Island_Site, remove = T)
coral_cover_Island_Site[is.na(coral_cover_Island_Site)] <- 0


# Make correlations between individual depths 
# View (coral_cover_Island_Site)
Cor_Depths <- coral_cover_Island_Site 

Cor_Depths <- Cor_Depths %>% complete( Depth,Island_Island_Site,fill = list(Cover = 0))

# Select the different depths
Shallow_six <- subset (Cor_Depths, Depth == 6)
Shallow_twenty <- subset (Cor_Depths, Depth == 20)
# Upper 40
Upper_forty <- subset (Cor_Depths, Depth == 40)
# Upper 60
Upper_sixty <- subset (Cor_Depths, Depth == 60)
# Upper 90
Lower_ninety <- subset (Cor_Depths, Depth == 90)
# Upper 120
Lower_hundredtwenty <- subset (Cor_Depths, Depth == 120)



# Compare all depths between all the rest (test)

all_vs_all <- Reduce(function(x,y) merge(x,y,by="Island_Island_Site",all=TRUE) ,list(Shallow_six,Shallow_twenty,Upper_forty,Upper_sixty, Lower_ninety, Lower_hundredtwenty))

colnames (all_vs_all) <- c("Island_Island_Site",  "Depth.Six","Cover.Six", "Cover_sd.Six","se.Six", 
                           "Depth.Twenty","Cover.Twenty", "Cover_sd.Twenty","se.Twenty",
                           "Depth.Forty","Cover.Forty", "Cover_sd.Forty","se.Forty",
                           "Depth.Sixty","Cover.Sixty", "Cover_sd.Sixty","se.Sixty",
                           "Depth.Ninety","Cover.Ninety", "Cover_sd.Ninety","se.Ninety",
                           "Depth.Hundredtwenty","Cover.Hundredtwenty", "Cover_sd.Hundredtwenty","se.Hundredtwenty")

# 6 m
cor.test(all_vs_all$Cover.Six, all_vs_all$Cover.Twenty, method ="pearson")
cor.test(all_vs_all$Cover.Six, all_vs_all$Cover.Forty, method ="pearson")
cor.test(all_vs_all$Cover.Six, all_vs_all$Cover.Sixty, method ="pearson")
cor.test(all_vs_all$Cover.Six, all_vs_all$Cover.Ninety, method ="pearson")
cor.test(all_vs_all$Cover.Six, all_vs_all$Cover.Hundredtwenty, method ="pearson")

# 20 m
cor.test(all_vs_all$Cover.Twenty, all_vs_all$Cover.Six, method ="pearson")
cor.test(all_vs_all$Cover.Twenty, all_vs_all$Cover.Forty, method ="pearson")
cor.test(all_vs_all$Cover.Twenty, all_vs_all$Cover.Sixty, method ="pearson")
cor.test(all_vs_all$Cover.Twenty, all_vs_all$Cover.Ninety, method ="pearson")
cor.test(all_vs_all$Cover.Twenty, all_vs_all$Cover.Hundredtwenty, method ="pearson")

# 40 m
cor.test(all_vs_all$Cover.Forty, all_vs_all$Cover.Six, method ="pearson")
cor.test(all_vs_all$Cover.Forty, all_vs_all$Cover.Twenty, method ="pearson")
cor.test(all_vs_all$Cover.Forty, all_vs_all$Cover.Sixty, method ="pearson")
cor.test(all_vs_all$Cover.Forty, all_vs_all$Cover.Ninety, method ="pearson")
cor.test(all_vs_all$Cover.Forty, all_vs_all$Cover.Hundredtwenty, method ="pearson")

# 60 m
cor.test(all_vs_all$Cover.Sixty, all_vs_all$Cover.Six, method ="pearson")
cor.test(all_vs_all$Cover.Sixty, all_vs_all$Cover.Twenty, method ="pearson")
cor.test(all_vs_all$Cover.Sixty, all_vs_all$Cover.Forty, method ="pearson")
cor.test(all_vs_all$Cover.Sixty, all_vs_all$Cover.Ninety, method ="pearson")
cor.test(all_vs_all$Cover.Sixty, all_vs_all$Cover.Hundredtwenty, method ="pearson")

# 90 m
cor.test(all_vs_all$Cover.Ninety, all_vs_all$Cover.Six, method ="pearson")
cor.test(all_vs_all$Cover.Ninety, all_vs_all$Cover.Twenty, method ="pearson")
cor.test(all_vs_all$Cover.Ninety, all_vs_all$Cover.Forty, method ="pearson")
cor.test(all_vs_all$Cover.Ninety, all_vs_all$Cover.Sixty, method ="pearson")
cor.test(all_vs_all$Cover.Ninety, all_vs_all$Cover.Hundredtwenty, method ="pearson")

# 120 m
cor.test(all_vs_all$Cover.Hundredtwenty, all_vs_all$Cover.Six, method ="pearson")
cor.test(all_vs_all$Cover.Hundredtwenty, all_vs_all$Cover.Twenty, method ="pearson")
cor.test(all_vs_all$Cover.Hundredtwenty, all_vs_all$Cover.Forty, method ="pearson")
cor.test(all_vs_all$Cover.Hundredtwenty, all_vs_all$Cover.Sixty, method ="pearson")
cor.test(all_vs_all$Cover.Hundredtwenty, all_vs_all$Cover.Ninety, method ="pearson")

median (all_vs_all$Cover.Six)
median (all_vs_all$Cover.Twenty)
median (all_vs_all$Cover.Forty)
median (all_vs_all$Cover.Sixty)
median (all_vs_all$Cover.Ninety)
median (all_vs_all$Cover.Hundredtwenty)


# Now, let's represent this in a plot
# Six m vs all the rest of meters
Six_vs_twenty <- ggplot(all_vs_all, aes(Cover.Six,Cover.Twenty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Six-se.Six, xmax=Cover.Six+se.Six), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Twenty-se.Twenty, ymax=Cover.Twenty+se.Twenty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="6 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="20 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

Six_vs_forty <- ggplot(all_vs_all, aes(Cover.Six,Cover.Forty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Six-se.Six, xmax=Cover.Six+se.Six), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Forty-se.Forty, ymax=Cover.Forty+se.Forty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="6 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="40 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

Six_vs_sixty <- ggplot(all_vs_all, aes(Cover.Six,Cover.Sixty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Six-se.Six, xmax=Cover.Six+se.Six), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Sixty-se.Sixty, ymax=Cover.Sixty+se.Sixty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="6 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="60 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

Six_vs_ninety <- ggplot(all_vs_all, aes(Cover.Six,Cover.Ninety, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Six-se.Six, xmax=Cover.Six+se.Six), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Ninety-se.Ninety, ymax=Cover.Ninety+se.Ninety), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="6 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="90 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

Six_vs_hundredtwenty <- ggplot(all_vs_all, aes(Cover.Six,Cover.Hundredtwenty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Six-se.Six, xmax=Cover.Six+se.Six), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Hundredtwenty-se.Hundredtwenty, ymax=Cover.Hundredtwenty+se.Hundredtwenty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="6 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="120 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

(Figure_six = Six_vs_twenty + Six_vs_forty + Six_vs_sixty + Six_vs_ninety + Six_vs_hundredtwenty + plot_layout(guides = 'collect', ncol = 5)  & theme(legend.position='right'))
ggsave ( "Outputs_R_Figures/Correlations_six.pdf", Figure_six,width = 16, height = 4)


# Twenty m
Twenty_vs_forty <- ggplot(all_vs_all, aes(Cover.Twenty,Cover.Forty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Twenty-se.Twenty, xmax=Cover.Twenty+se.Twenty), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Forty-se.Forty, ymax=Cover.Forty+se.Forty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="20 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="40 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

Twenty_vs_sixty <- ggplot(all_vs_all, aes(Cover.Twenty,Cover.Sixty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Twenty-se.Twenty, xmax=Cover.Twenty+se.Twenty), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Sixty-se.Sixty, ymax=Cover.Sixty+se.Sixty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="20 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="60 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 


Twenty_vs_ninety <- ggplot(all_vs_all, aes(Cover.Twenty,Cover.Ninety, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Twenty-se.Twenty, xmax=Cover.Twenty+se.Twenty), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Ninety-se.Ninety, ymax=Cover.Ninety+se.Ninety), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="20 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="90 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

Twenty_vs_hundredtwenty <- ggplot(all_vs_all, aes(Cover.Twenty,Cover.Hundredtwenty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Twenty-se.Twenty, xmax=Cover.Twenty+se.Twenty), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Hundredtwenty-se.Hundredtwenty, ymax=Cover.Hundredtwenty+se.Hundredtwenty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="20 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="120 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

(Figure_twenty = Six_vs_twenty + Twenty_vs_forty + Twenty_vs_sixty + Twenty_vs_ninety + Twenty_vs_hundredtwenty + plot_layout(guides = 'collect', ncol = 5)  & theme(legend.position='right'))
ggsave ( "Outputs_R_Figures/Correlations_twenty.pdf", Figure_twenty,width = 16, height = 4)

# Forty m 
Forty_vs_sixty <- ggplot(all_vs_all, aes(Cover.Forty,Cover.Sixty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Forty-se.Forty, xmax=Cover.Forty+se.Forty), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Sixty-se.Sixty, ymax=Cover.Sixty+se.Sixty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="40 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="60 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

Forty_vs_ninety <- ggplot(all_vs_all, aes(Cover.Forty,Cover.Ninety, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Forty-se.Forty, xmax=Cover.Forty+se.Forty), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Ninety-se.Ninety, ymax=Cover.Ninety+se.Ninety), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="40 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="90 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

Forty_vs_hundredtwenty <- ggplot(all_vs_all, aes(Cover.Forty,Cover.Hundredtwenty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Forty-se.Forty, xmax=Cover.Forty+se.Forty), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Hundredtwenty-se.Hundredtwenty, ymax=Cover.Hundredtwenty+se.Hundredtwenty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="40 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="120 m cover (%)", limits=c(-5,95), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

(Figure_forty = Six_vs_forty + Twenty_vs_forty + Forty_vs_sixty + Forty_vs_ninety + Forty_vs_hundredtwenty + plot_layout(guides = 'collect', ncol = 5)  & theme(legend.position='right'))
ggsave ( "Outputs_R_Figures/Correlations_forty.pdf", Figure_forty,width = 16, height = 4)

# Sixty m
Sixty_vs_ninety <- ggplot(all_vs_all, aes(Cover.Sixty,Cover.Ninety, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Sixty-se.Sixty, xmax=Cover.Sixty+se.Sixty), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Ninety-se.Ninety, ymax=Cover.Ninety+se.Ninety), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="60 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="90 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

Sixty_vs_hundredtwenty <- ggplot(all_vs_all, aes(Cover.Sixty,Cover.Hundredtwenty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Sixty-se.Sixty, xmax=Cover.Sixty+se.Sixty), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Hundredtwenty-se.Hundredtwenty, ymax=Cover.Hundredtwenty+se.Hundredtwenty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="60 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="120 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

(Figure_sixty = Six_vs_sixty + Twenty_vs_sixty + Forty_vs_sixty + Sixty_vs_ninety + Sixty_vs_hundredtwenty + plot_layout(guides = 'collect', ncol = 5)  & theme(legend.position='right'))
ggsave ( "Outputs_R_Figures/Correlations_sixty.pdf", Figure_sixty,width = 16, height = 4)

# Ninety m
Ninety_vs_hundredtwenty <- ggplot(all_vs_all, aes(Cover.Ninety,Cover.Hundredtwenty, colour = Island_Island_Site)) + 
  geom_point(size = 3,) +
  geom_errorbar(aes(xmin=Cover.Ninety-se.Ninety, xmax=Cover.Ninety+se.Ninety), width=.2,position=position_dodge(0.05)) +
  geom_errorbar(aes(ymin=Cover.Hundredtwenty-se.Hundredtwenty, ymax=Cover.Hundredtwenty+se.Hundredtwenty), width=.2,position=position_dodge(0.05)) +
  geom_segment(aes(x = 0, y = 33, xend = 66, yend = 33),linetype="dashed", color = "gray20", size=0.6) +
  geom_segment(aes(x = 33, y = 0, xend = 33, yend = 66),linetype="dashed", color = "gray20", size=0.6) +
  scale_x_continuous(name ="90 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  scale_y_continuous(name ="120 m cover (%)", limits=c(-5,87), breaks = c(0,20,40,60,80)) +
  theme_bw()  + theme_classic() + theme(plot.title = element_text(hjust=0.5, size=12, face="bold"),
                                        axis.text = element_text(size=10, colour="black"),
                                        axis.title = element_text(size=11, face="bold", colour="black"), 
                                        legend.title = element_blank()) 

(Figure_ninety = Six_vs_ninety + Twenty_vs_ninety + Forty_vs_ninety + Sixty_vs_ninety + Ninety_vs_hundredtwenty + plot_layout(guides = 'collect', ncol = 5)  & theme(legend.position='right'))
ggsave ( "Outputs_R_Figures/Correlations_ninety.pdf", Figure_ninety,width = 16, height = 4)



# Add a matrix of correlations between all depths 
correlation_all <- select (all_vs_all, Cover.Six, Cover.Twenty, Cover.Forty, Cover.Sixty, Cover.Ninety,Cover.Hundredtwenty)
colnames (correlation_all) <- c("6 m", "20 m", "40 m", "60 m", "90 m", "120 m")

Cor_Mat <- cor (correlation_all)

Cor_plot <- corrplot(Cor_Mat,  type="lower", 
                     col=brewer.pal(n=8, name="RdYlBu"))

pdf("Outputs_R_Figures/Cor_plot.pdf", 5, 5)
Cor_plot <- corrplot(Cor_Mat,  type="lower", 
                     col=brewer.pal(n=8, name="RdYlBu"))
dev.off()
# It corresponds to Sup. Fig. 2 b

rcorr(as.matrix(correlation_all))
# Add the p-value
res <- cor.mtest(correlation_all, conf.level = 0.95)

corrplot(Cor_Mat, p.mat = res$p, sig.level = 0.05,number.digits = 6, insig = "p-value", type="lower", 
         col=brewer.pal(n=8, name="RdYlBu"))

# Save plot and work from AI, it represents Sup. Fig. 2 with all correlations. 
# The mirroring was done with AI

# We also made some test only including Fixed hard substrate. We considered, it was more interesting to keep all sites and depths for the correlations











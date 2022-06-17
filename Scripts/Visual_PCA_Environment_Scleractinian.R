

# This script generates Supplementary Figures 4 and 5
# It also generates the betadisper of the different coral morphologies according to depths
##########################################################################
rm (list = ls ())
library (dplyr); library(tidyverse); require (plyr); library(ggcorrplot); library(ade4); library (factoextra); library (vegan)

# Script of principal component analysis between environmental predictors and scleractinian morphologies

# Environmental clustering and PCA
# Morphology clustering and PCA


# PCA of the different sites and depths according to numeric environmental categories and scleractinian morphologies

# Open the most completed database with coral cover, morpho cover and environment
Total_cover_data_coral_forms_environment <- read.csv("Data/Total_cover_morpho_environment.csv", sep=",", row.names = 1)

# Select only the variables we want to keep. Numerical env. data and morphologies
Total_cover_data_coral_forms_environment <- select (Total_cover_data_coral_forms_environment, Site, Depth, Category, Cover, Cover_se, Cover_total, Cover_total_se, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind)
colnames (Total_cover_data_coral_forms_environment) <- c("Site", "Depth", "Morphology", "Morphology_cover", "Morphology_cover_se", "Total_cover", "Total_cover_se", "Light_Rel_Ind", "Temp_Variability", "Temp_Rel_Ind")


### First, environmental data 
# Select only environmental data and total coral cover
Total_Environment_PCA <- select (Total_cover_data_coral_forms_environment, Site, Depth, Total_cover, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind)

# Set the rownames and put in format to PCA deleting duplicated
Total_Environment_PCA$ID<- with(Total_Environment_PCA, paste0(Depth, sep = "_",  Site))
Total_Environment_PCA <- Total_Environment_PCA[!duplicated(Total_Environment_PCA), ]
rownames(Total_Environment_PCA) <- Total_Environment_PCA[,"ID"]
Environment_PCA_num <-  select (Total_Environment_PCA,Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind)
# Remove NA
Environment_PCA_num <- na.omit(Environment_PCA_num)


# Set colors of different depths and eventually labels of sites
# Extract depths from the rownames
Environment_PCA_num$Depth = sapply(strsplit(rownames(Environment_PCA_num), "_"), function(x) x[1])

# Extract sites ()
Environment_PCA_num$Site = sapply(strsplit(rownames(Environment_PCA_num), "_"), function(x) x[2])

# Set colors
gcol <- c("black",  "aquamarine2","deepskyblue","wheat","blue","navyblue")
Environment_PCA_num$Depth <- as.factor (Environment_PCA_num$Depth)


Environment_PCA_num_norownames <- select (Total_cover_data_coral_forms_environment, Site, Depth, Total_cover, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind)
Environment_PCA_num_norownames <- Environment_PCA_num_norownames[!duplicated(Environment_PCA_num_norownames), ]
Environment_PCA_num_norownames <- na.omit(Environment_PCA_num_norownames)



# Make the PCA and biplot with only the environment

pca_environment <- dudi.pca(Environment_PCA_num[,c(1,2,3)], scale = T, nf = 2)   # scale = T (mean/stand.dev) because data is not on the same scale units
# Select 2
inertia.dudi(pca_environment,col = TRUE)    # Information of variables 
pca_environment$co

# Necessary to have one biplot for information of axis
Environment_PCA_num_norownames$Depth = factor(Environment_PCA_num_norownames$Depth,levels = c ("6","20","40","60","90","120"))

fviz_pca_biplot(pca_environment, labelsize = 2, geom = "point", repel = T, habillage=Environment_PCA_num_norownames$Depth,
                addEllipses=F, ellipse.level=0.95,palette = c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black"),
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12))


# Make the graph of the ellipses for the PCA of only environment
# Plot
s.class (pca_environment$li, fac = Environment_PCA_num [,4], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2*pca_environment$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = FALSE) # Add the variables
# 5*pca$co multiplicate the arrows for 5

# Save this plot "Sclass_Sarrow_Abiotic_Env.pdf"
pdf("Outputs_R_Figures/Sclass_Sarrow_Abiotic_Env.pdf", 10, 8)
s.class (pca_environment$li, fac = Environment_PCA_num [,4], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2.25*pca_environment$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = F) # Add the variables
dev.off()

# This correspond to Sup. Fig. 3


####################################################################
### Second, morphology data 
### Make the same only with the different coral morphologies (no environment)

# Select all columns that you want 
Total_morpho_PCA <- select (Total_cover_data_coral_forms_environment, Site, Depth, Morphology, Morphology_cover, Total_cover, Light_Rel_Ind, Temp_Variability, Temp_Rel_Ind)


# Add columns for the different forms also 
Total_morpho_PCA = melt(Total_morpho_PCA, id=c("Site", "Depth", "Morphology"), measure.vars="Morphology_cover", na.rm=FALSE)

Total_morpho_PCA = dcast(Total_morpho_PCA, Site + Depth  ~ Morphology, mean, add.missing = T)


# Set the rownames and put in format to PCA deleting duplicated
Total_morpho_PCA$ID<- with(Total_morpho_PCA, paste0(Depth, sep = "_",  Site))
Total_morpho_PCA <- Total_morpho_PCA[!duplicated(Total_morpho_PCA), ]
rownames(Total_morpho_PCA) <- Total_morpho_PCA[,"ID"]
# Keep all coral forms, total cover, etc. (This what you will need to change after)
Morphology_PCA_num <-  select (Total_morpho_PCA, Branching,Massive,Encrusting,Solitary,Laminar)


# Extract depth from rownames
Morphology_PCA_num$Depth = sapply(strsplit(rownames(Morphology_PCA_num), "_"), function(x) x[1])
# Extract sites from rownames
Morphology_PCA_num$Site = sapply(strsplit(rownames(Morphology_PCA_num), "_"), function(x) x[2])


Morphology_PCA_num_norownames <- select (Total_morpho_PCA, Site, Depth, Branching, Encrusting, Laminar, Massive, Solitary)
Morphology_PCA_num_norownames <- Morphology_PCA_num_norownames[!duplicated(Morphology_PCA_num_norownames), ]
Morphology_PCA_num_norownames <- na.omit(Morphology_PCA_num_norownames)


# Make the PCA biplot with only coral morphologies (no environment, no total cover)
pca_morpho <- dudi.pca(Morphology_PCA_num[,c(1,2,3,4,5)], scale = T)   # scale = T (mean/stand.dev) because data is not on the same scale units
# Select 2
inertia.dudi(pca_morpho,col = TRUE)    # Information of dimensions
pca_morpho$co


# Necessary to have the biplot to see information of the dimensions axis
Morphology_PCA_num$Depth = factor(Morphology_PCA_num$Depth,levels = c ("6","20","40","60","90","120"))

fviz_pca_biplot(pca_morpho, repel = T, labelsize = 3,geom = "point", habillage=Morphology_PCA_num$Depth,
                addEllipses=F, ellipse.level=0.95,palette = c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black"),
                col.var = "black", # Variables color
                col.ind = "black", # Individuals color
)   + theme(text = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12))


# Set colors of different depths and eventually labels of sites
Morphology_PCA_num_norownames$Depth = factor(Morphology_PCA_num_norownames$Depth,levels = c ("6","20","40","60","90","120"))
Morphology_PCA_num_norownames$Depth <- as.factor (Morphology_PCA_num_norownames$Depth)
gcol <- c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black")


# Plot
s.class (pca_morpho$li, fac = Morphology_PCA_num_norownames [,2], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2*pca_morpho$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = FALSE) # Add the variables
# 5*pca$co multiplicate the arrows for 5

groups <- as.factor(Morphology_PCA_num_norownames$Depth)
s.class(pca_morpho$li,
        fac = groups,  # color by groups
        col = c("wheat",  "aquamarine2","deepskyblue","blue","navyblue","black"), 
        add.plot = TRUE,         # Add onto the scatter plot
        cstar = 0,               # Remove stars
        cellipse = 0             # Remove ellipses
)



# Save this plot "Sclass_Sarrow_Abiotic_Env.pdf"
pdf("Outputs_R_Figures/Sclass_Sarrow_Biotic_Forms.pdf", 10, 8)
s.class (pca_morpho$li, fac = Morphology_PCA_num_norownames [,2], col = gcol, xax = 1, yax = 2)   # Individual label for depths!
s.arrow (2.25*pca_morpho$co, xax = 1, yax = 2, clab = 1,  add.plot = T, boxes = FALSE) # Add the variables
dev.off()

# This corresponds to Supplementary Figure 4













########################################################################################################3

#Running PERMANOVA to check dispersion of morphologies - You are keeping this section! IMPORTANT
cover_st2.dist <- vegdist(Morphology_PCA_num [,c(1:5)], method="euclidean")

#Create factors
# cover_st2.env <- as.data.frame(row.names(cover_st2)) ##### had to replace this line by below because otherwise there was an issue
cover_st2 <- as.data.frame(row.names(Morphology_PCA_num))
colnames(cover_st2)[1] <- "ID"
# Extract depths
cover_st2$Depth <- sub("\\_.*", "", cover_st2$ID)
# Extract Island 
cover_st2$Island_Site <- sapply(strsplit(cover_st2$ID, "_"), function(x) x[2])

# MEasure dispersions
disp_morpho_Depth <- betadisper(cover_st2.dist, group=cover_st2$Depth)  
permutest(disp_morpho_Depth)
print (disp_morpho_Depth)








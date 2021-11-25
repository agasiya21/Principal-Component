library(nnet)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(pca3d)
library(tidyverse)
library(psych)
library(easystats)



#################################################################################################
                                 #  Assumptions #
#################################################################################################

##Sample size: ideally, there should be 150+ cases and there should be ratio of at least five cases 
        #for each variable (Pallant, 2010)

##Correlations: there should be some correlation among the factors to be considered for PCA

##Linearity: it is assumed that the relationship between the variables are linearly related

##Outliers: PCA is sensitive to outliers; they should be removed.


###### 2. Load the data and extract only active individuals and variables:######
dst <- decathlon2
decathlon2.active <- decathlon2[1:23, 1:10]
decathlon2.active[, 1:6] %>% head()

### 3. Compute PCA
res.pca <- prcomp(decathlon2.active, scale = TRUE)

##### 4. Visualize eigenvalues (scree plot). 
#Show the percentage of variances explained by each principal component
fviz_eig(res.pca, addlabels = TRUE)

#### 5. Graph of individuals. 
#Individuals with a similar profile are grouped together.
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#### Graph of variables. 
### Positive correlated variables point to the same side of the plot. 
### Negative correlated variables point to opposite sides of the graph
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

### Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

### Access to the PCA results
# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

### Predict using PCA
### Supplementary individuals
## 1. Data: rows 24 to 27 and columns 1 to 10 [in decathlon2 data sets]. 
#The new data must contain columns (variables) with the same names and in the 
#same order as the active data used to compute PCA.

# Data for the supplementary individuals
ind.sup <- decathlon2[24:27, 1:10]
ind.sup[, 1:6]

### Predict the coordinates of new individuals data. Use the R base function predict():
ind.sup.coord <- predict(res.pca, newdata = ind.sup)
ind.sup.coord[, 1:4]

### Graph of individuals including the supplementary individuals:
# Plot of active individuals
p <- fviz_pca_ind(res.pca, repel = TRUE)
# Add supplementary individuals
fviz_add(p, ind.sup.coord, color ="blue")

### The predicted coordinates of individuals can be manually calculated as follow:
# Centering and scaling the supplementary individuals
ind.scaled <- scale(ind.sup, 
                    center = res.pca$center,
                    scale = res.pca$scale)
# Coordinates of the individividuals
coord_func <- function(ind, loadings){
  r <- loadings*ind
  apply(r, 2, sum)
}
pca.loadings <- res.pca$rotation
ind.sup.coord <- t(apply(ind.scaled, 1, coord_func, pca.loadings ))
ind.sup.coord[, 1:4]

##Supplementary variables
##Qualitative / categorical variables
groups <- as.factor(decathlon2$Competition[1:23])
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)

#===========================================================================================================
#===========================================================================================================
library(magrittr) # for pipe %>%
library(dplyr)   # everything else
# 1. Individual coordinates
res.ind <- get_pca_ind(res.pca)
# 2. Coordinate of groups
coord.groups <- res.ind$coord %>%
  as_data_frame() %>%
  select(Dim.1, Dim.2) %>%
  mutate(competition = groups) %>%
  group_by(competition) %>%
  summarise(
    Dim.1 = mean(Dim.1),
    Dim.2 = mean(Dim.2)
  )
coord.groups

## Quantitative variables
quanti.sup <- decathlon2[1:23, 11:12, drop = FALSE]
head(quanti.sup)
# Predict coordinates and compute cos2
quanti.coord <- cor(quanti.sup, res.pca$x)
quanti.cos2 <- quanti.coord^2
# Graph of variables including supplementary variables
p <- fviz_pca_var(res.pca)
fviz_add(p, quanti.coord, color ="blue", geom="arrow")


#################################################################################################
                       #  Theory behind PCA results  #
#################################################################################################

## PCA results for variables
#Here we’ll show how to calculate the PCA results for variables: coordinates, cos2 and contributions:
  
  #var.coord = loadings * the component standard deviations
  #var.cos2 = var.coord^2
  #var.contrib. The contribution of a variable to a given principal component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component)
# Helper function 
#::::::::::::::::::::::::::::::::::::::::
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
#::::::::::::::::::::::::::::::::::::::::
loadings <- res.pca$rotation
sdev <- res.pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:4])


# Compute Cos2
#::::::::::::::::::::::::::::::::::::::::
var.cos2 <- var.coord^2
head(var.cos2[, 1:4])


# Compute contributions
#::::::::::::::::::::::::::::::::::::::::
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
head(var.contrib[, 1:4])


###PCA results for individuals
ind.coord = res.pca$x
#Cos2 of individuals. Two steps:
  #Calculate the square distance between each individual and the PCA center of gravity: d2 = [(var1_ind_i - mean_var1)/sd_var1]^2 + …+ [(var10_ind_i - mean_var10)/sd_var10]^2 + …+..
#Calculate the cos2 as ind.coord^2/d2
#Contributions of individuals to the principal components: 
100 * (1 / number_of_individuals)*(ind.coord^2 / comp_sdev^2). 
#Note that the sum of all the contributions per column is 100

# Coordinates of individuals
#::::::::::::::::::::::::::::::::::
ind.coord <- res.pca$x
head(ind.coord[, 1:4])


# Cos2 of individuals
#:::::::::::::::::::::::::::::::::
# 1. square of the distance between an individual and the
# PCA center of gravity
center <- res.pca$center
scale<- res.pca$scale
getdistance <- function(ind_row, center, scale){
  return(sum(((ind_row-center)/scale)^2))
}
d2 <- apply(decathlon2.active,1,getdistance, center, scale)
# 2. Compute the cos2. The sum of each row is 1
cos2 <- function(ind.coord, d2){return(ind.coord^2/d2)}
ind.cos2 <- apply(ind.coord, 2, cos2, d2)
head(ind.cos2[, 1:4])


# Contributions of individuals
#:::::::::::::::::::::::::::::::
contrib <- function(ind.coord, comp.sdev, n.ind){
  100*(1/n.ind)*ind.coord^2/comp.sdev^2
}
ind.contrib <- t(apply(ind.coord, 1, contrib, 
                       res.pca$sdev, nrow(ind.coord)))
head(ind.contrib[, 1:4])


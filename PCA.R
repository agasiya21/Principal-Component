library(nnet)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(pca3d)
library(tidyverse)
library(psych)

### Assumptions:
  
#Sample size: ideally, there should be 150+ cases and there should be ratio of at least five cases for each variable (Pallant, 2010)

#Correlations: there should be some correlation among the factors to be considered for PCA

#Linearity: it is assumed that the relationship between the variables are linearly related

#Outliers: PCA is sensitive to outliers; they should be removed.


breast_data <- read.csv(
  "https://raw.githubusercontent.com/mariocastro73/ML2020-2021/master/datasets/breast-cancer-data.csv")
View(breast_data)
#str(breast_data)

########### REMOIVNG OF UNREQUIRED VARIABLES ##########
breast <- breast_data[, -c(1,2,33)]
scale <- breast
#str(breast)

########### SCALLING / STANDARDIZING ###############
scale <- breast %>% scale() #for the first 10 variables
pca_10 <- prcomp(scale)

scalle <- breast %>% scale() #for all the variables
pca_all <- prcomp(scalle)

########### The scores of each variable ###############
pca$x #selecting the scores
summary(pca_10)
summary(pca_all)

biplot(pca_10)
biplot(pca_all)

screeplot(pca_10)
screeplot(pca_all)

######### visualize ##### more nicer plots ###########
fviz_pca_biplot(pca_10, repel = T)
fviz_pca_biplot(pca_all, repel = T)

fviz_pca_var(pca_10, repel = T)
fviz_pca_var(pca_all, repel = T)

fviz_eig(pca_10, addlabels = TRUE, ylim = c(0, 100))
fviz_eig(pca_all, addlabels = TRUE, ylim = c(0, 100))

fviz_screeplot(pca_10,  addlabels = TRUE, ylim = c(0, 100))
fviz_screeplot(pca_all, addlabels = TRUE, ylim = c(0, 100))

############## 3d visual #############
pca3d(pca_all)
pca3d(pca_all, show.labels = T, fancy = T)

############## pca svd ###########
fpca <- PCA(scale, ncp = 10)
fpca

fpca$var$contrib # for the contributuion of each variable
fpca$eig # for eigenvalues
cumsum(fpca$eig[1:10]) # for the cummulative sum of the eigenvalues
plot(cumsum(fpca$eig[1:10]), main = "CumSum Plot", xlab = "Components")

#Biplot and scree plot 
fviz_pca(fpca)
fviz_pca_biplot(fpca, repel = T, col.ind = "contrib", col.var = "red")
fviz_screeplot(fpca, ncp = 10, addlabels = TRUE, ylim = c(0, 100))
fviz_screeplot(fpca, ncp = 10, geom = "line", addlabels = TRUE, ylim = c(0, 100))
fviz_screeplot(fpca, ncp = 10, geom = "bar", barfill = "red", addlabels = TRUE, ylim = c(0, 100))


############# CHECK REDUDNANCY BY CORRELATION PLOT #############
corrplot(cor(breast))
corrplot(cor(breast), diag = FALSE)



############   IMPORTANCE OF SCALING #############
#### Non scaled output ###########
prcomp <- prcomp(breast)
plot(prcomp, type = "l", main = "Plot of PCA")
fviz_screeplot(prcomp, type = "line", main = "Plot of PCA", addlabels = T)
fviz_pca_biplot(prcomp, scale = T)


#pca <- PCA(breast)
#fviz_contrib(pca, choice = "var", top = 20, axes = 2)


library(report)
model <- lm(Sepal.Length ~ Species, data = iris)
report(model)





## get packages installed
#packs = as.data.frame(installed.packages(.libPaths()[1]), stringsAsFactors = F)

## and now re-install install packages using install.packages()
#install.packages(packs$Package)

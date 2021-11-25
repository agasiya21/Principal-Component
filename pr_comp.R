library(nnet)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(pca3d)
library(tidyverse)
library(psych)
library(easystats)
library(magrittr)
library(survival)
library(ggbiplot)


cancer <- survival::cancer %>% drop_na() 
Factoshiny(cancer)
plot(cancer)
pca <- prcomp(cancer[,-5],
              center = TRUE,
              scale = TRUE)
plot(pca)
print(pca)
summary(pca)

fviz_pca_biplot(pca, repel = TRUE, col.var = "green")
fviz_screeplot(pca,geom = "line", addlabels = TRUE, ylim = c(0, 30))

can <- cancer %>%
  mutate(
    sex = case_when(sex == 1 ~ "Male", T ~ "Female"))
  
ggbiplot(pca, 
         obs.scale = 1, 
         var.scale = 1, 
         groups = can$sex, 
         ellipse = TRUE, 
         circle = TRUE, 
         ellipse.prob = 70) + 
  scale_color_discrete(name = '') + 
  theme(legend.direction = "horizonal",
        legend.position = "top")


































dset <- mtcars
head(dset, 10)
dset %>% 
  mutate(
    vs_new = case_when(vs == 0 ~ "no", vs == 1 ~ "yes"),
    mpg_new = case_when(mpg <=20.0 ~ "usual", mpg >20.0 & mpg <22.5 ~ "high", mpg > 22.5 ~ "extreame")
  ) %>%
  head(10)

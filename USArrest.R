library(nnet)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(pca3d)
library(tidyverse)
library(psych)
library(easystats)

usa <- USArrests
prcomp <- prcomp(usa, scale = T)
summary(prcomp)
fviz_screeplot(prcomp, type = "l", addlabels = T)
#abline(1,0, col = "green", lty = 2)
fviz_pca_biplot(prcomp)

principal(usa, nfactors = 3, rotate = "none")


library(magrittr)
library(tidyverse)

dset <- mtcars
head(dset, 10)

dset %>% 
  mutate(
    vs_new = case_when(vs == 0 ~ "no", vs == 1 ~ "yes"),
    mpg_new = case_when(mpg <=20.0 ~ "usual", mpg >20.0 & mpg <22.5 ~ "high", mpg > 22.5 ~ "extreame")
  ) %>%
  head(10)

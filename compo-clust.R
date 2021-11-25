#=================================
    #PRINCIPAL COMPONENT ANALYSIS
#=================================

# Iris Data
data <- read.csv(file.choose(), header=T)
str(data)
data$NSP <- factor(data$NSP)

# Partition Data
set.seed(111)
ind <- sample(2, nrow(data), 
              replace = TRUE, 
              prob = c(.8, .2))
training <- data[ind==1,]
testing <- data[ind==2,]

# Scatter Plots & Correlations
library(psych)
pairs.panels(training[,-22], 
             gap=0,
             bg=c("red","yellow","blue")[training$NSP],
             pch=21)

# PCA
pc <- prcomp(training[,-22],
             center = TRUE,
             scale. = TRUE)
attributes(pc)
print(pc)
summary(pc)
plot(pc, type = "lines")

# Orthogonality of PCs
pairs.panels(pc$x, 
             gap=0,
             bg=c("red","yellow","blue")[training$NSP],
             pch=21)
# Bi-Plot
library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
g <- ggbiplot(pc, 
              obs.scale = 1, 
              var.scale = 1, 
              groups = training$NSP, 
              ellipse = TRUE, 
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

# Prediction with Principal Components
trg <- predict(pc, training)
trg <- data.frame(trg, training[22])
tst <- predict(pc, testing)
tst <- data.frame(tst, testing[22])

# Multinomial Logistic regression with 1st two PCs
library(nnet)
trg$NSP<-relevel(trg$NSP, ref="1")
mymodel <- multinom(NSP~PC1+PC2+PC3+PC4+PC5, data=trg)
summary(mymodel)

# Misclassification error & Confusion matrix - training
p <- predict(mymodel, trg)
tab <- table(p, trg$NSP)
tab
1-sum(diag(tab))/sum(tab)

# Misclassification error & Confusion matrix - Testing
p1 <- predict(mymodel, tst)
tab1 <- table(p1, tst$NSP)
tab1
1-sum(diag(tab1))/sum(tab1)




#======================================
     # CLUSTER ANALYSIS
#======================================

# Cluster Analysis
mydata <- read.csv(file.choose(), header=T)
str(mydata)
head(mydata)
pairs(mydata[2:9])

# Scatter plot 
plot(mydata$Fuel_Cost~ mydata$Sales, data = mydata)
with(mydata,text(mydata$Fuel_Cost ~ mydata$Sales, labels=mydata$Company,pos=4))

# Normalize 
z <- mydata[,-c(1,1)]
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale=sds)

# Calculate distance matrix  
distance = dist(nor)

# Hierarchical agglomerative clustering  
mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$Company,main='Default from hclust')
plot(mydata.hclust,hang=-1)

# Hierarchical agglomerative clustering using "average" linkage 
mydata.hclust<-hclust(distance,method="average")
plot(mydata.hclust,hang=-1)

# Cluster membership
member = cutree(mydata.hclust,3)
table(member)

# Characterizing clusters 
aggregate(nor,list(member),mean)
aggregate(mydata[,-c(1,1)],list(member),mean)

# Silhouette Plot
library(cluster) 
plot(silhouette(cutree(mydata.hclust,3), distance)) 

# Scree Plot
wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-means clustering
set.seed(123)
kc<-kmeans(nor,4)
#paper data614

install.packages("dbscan")
install.packages("fpc")

library(datasets)
library(dplyr)
library(ggplot2)
library("dbscan")
library("fpc")
library(mclust)
library("Hmisc")
library(resampledata)

library(lmPerm)
library(coin)
library(mosaic)

library(gginference)


data(iris)

summary(iris)

names(iris) <- tolower(names(iris))

ggplot(iris, aes(sepal.length, petal.width, color = species), e) + geom_point(size=4)

iris[, 1:4]
#usage of k-means clustering 
set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

#confusion table
table(irisCluster$cluster, iris$species)

#irisCluster$cluster <- as.factor(irisCluster$cluster)
#ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$cluster)) + geom_point()

#usage of hirarchical clustering 
clusters <- hclust(dist(iris[, 3:4]))
clusters
plot(clusters)

#using cutree to keep three clusters 
clusterCut <- cutree(clusters, 3)

#confusion matrix 
table(clusterCut, iris$Species)

#plot of clusters 
ggplot(iris, aes(petal.length, petal.width, color = iris$species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green'))


#usage of hdbscan 
set.seed(123445)
cl <- hdbscan(iris[, 1:4], minPts = 20)
cl$cluster

#confusion matrix 
table( cl$cluster, iris$species)

#plot of clusters 
plot(iris[, 3:4], col=cl$cluster+1, pch=20)

print(cl$cluster_scores)

#plot of clusters with membership probability 
plot(iris[, 3:4], col=cl$cluster+1, pch=21)
colors <- mapply(function(col, i) adjustcolor(col, alpha.f = cl$membership_prob[i]), 
                 palette()[cl$cluster+1], seq_along(cl$cluster))
points(iris[, 3:4], col=colors, pch=20)

#Determing optimal value of e (eps)
iris_matrix <- as.matrix(iris[, 1:4])
#find the elbow in the plot 
kNNdistplot(iris_matrix, k=4)
abline(h=0.2, col="red")

#usage of dbscan 
set.seed(1234)
db = dbscan(iris_matrix, 0.2, 12)
db$cluster

#confusion matrix 
table( db$cluster, iris$species)

#plot of clusters 
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = db$cluster/2+0.01) + 
  scale_color_manual(values = c('black', 'red', 'green'))

hullplot(iris_matrix, db$cluster)

#stat of clustering 
cs = cluster.stats(dist(iris[, 1:4]), db$cluster)
cs
cs[c("within.cluster.ss","avg.silwidth")]

#usage of dbscan with fcp library 
set.seed(123)
db <- fpc::dbscan(iris[, 1:4], eps = 0.2, MinPts = 12)

table(iris$Species, db$cluster)
plot(db, iris[, 3:4], main = "DBSCAN", frame = FALSE)

#HDBSCAN
clusterer = hdbscan.HDBSCAN(min_cluster_size=3, gen_min_span_tree=True)
clusterer.fit(test_data)


#Gaussian mixture model clustering

mcl_model <- Mclust(iris[, 1:4], 3)
mcl_model


table( mcl_model$classification, iris$species)

plot(mcl_model, what = "classification", main = "Mclust Classification")
#plot of clusters 
mcl_model$classification
plot(iris[, 3:4], col=mcl_model$classification+1, pch=20)



############################################################################################

#one dim

x1 <- iris[,1]
x2 <- iris[,2]
x3 <- iris[,3]
x4 <- iris[,4]

#two dim
x12 <- iris[,c(1,2)]
x13 <- iris[,c(1,3)]
x14 <- iris[,c(1,4)]
x23 <- iris[,c(2,3)]
x24 <- iris[,c(2,4)]
x34 <- iris[,c(3,4)]

#three dim
x123 <- iris[,c(1,2,3)]
x234 <- iris[,c(2,3,4)]
x134 <- iris[,c(1,3,4)]
x124 <- iris[,c(1,2,4)]

################################################# kmeans 1 dim
#models
set.seed(123)
irisCluster <- kmeans(x4, 3, nstart = 20)
#confusion table
table(irisCluster$cluster, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 2
  else if (iris$species[i] == "setosa")
    species[i] <- 3
  else if (iris$species[i] == "virginica")
    species[i] <- 1
}
#species

sum(irisCluster$cluster == species) / 150 *100

#results kmeans 1 dim
#x1 => 66%
#x2 => 57%
#x3 => 94%
#x4 => 96%

df <- data.frame()
vec <- c(66,57,94,96)

#df_kmeans <- data.frame(Dimensions=c(1), Accuracy = mean(vec), sd = sd(vec), Alg="K-means")
df_kmeans <- data.frame(Dimensions=c(1), Accuracy = vec, Alg="K-means")

df_kmeans

################################################# kmeans 2 dim
#models
set.seed(123)
irisCluster <- kmeans(x34, 3, nstart = 20)
#confusion table
table(irisCluster$cluster, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 1
  else if (iris$species[i] == "setosa")
    species[i] <- 3
  else if (iris$species[i] == "virginica")
    species[i] <- 2
}
#species

sum(irisCluster$cluster == species) / 150 *100

#results kmeans 1 dim
#x12 => 82%
#x13 => 88%
#x14 => 82%
#x23 => 92%
#x24 => 92%
#x34 => 96%
vec <- c(82,88,82,92, 92, 96)

#df <- data.frame(Dimensions=c(2), Accuracy = mean(vec), sd = sd(vec), Alg="K-means")
df <- data.frame(Dimensions=c(2), Accuracy = vec, Alg="K-means")

df_kmeans <- rbind(df_kmeans, df)

df_kmeans
################################################# kmeans 3 dim
#models
set.seed(123)
irisCluster <- kmeans(x124, 3, nstart = 20)
#confusion table
table(irisCluster$cluster, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 3
  else if (iris$species[i] == "setosa")
    species[i] <- 1
  else if (iris$species[i] == "virginica")
    species[i] <- 2
}
#species

sum(irisCluster$cluster == species) / 150 *100

#results kmeans 1 dim
#x123 => 88%
#x234 => 95%
#x134 => 89%
#x124 => 82%
vec <- c(88,95,89,82)

#df <- data.frame(Dimensions=c(3), Accuracy = mean(vec), sd = sd(vec), Alg="K-means")
df <- data.frame(Dimensions=c(3), Accuracy = vec, Alg="K-means")

df_kmeans <- rbind(df_kmeans, df)

df_kmeans

#################################################### kmeans 4 dim

#models
set.seed(123)
irisCluster <- kmeans(iris[,1:4], 3, nstart = 20)
#confusion table
table(irisCluster$cluster, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 2
  else if (iris$species[i] == "setosa")
    species[i] <- 1
  else if (iris$species[i] == "virginica")
    species[i] <- 3
}
#species

sum(irisCluster$cluster == species) / 150 *100

#results kmeans 1 dim
#x1:4 => 89%

vec <- c(89)

#df <- data.frame(Dimensions=c(4), Accuracy = mean(vec), sd = 0, Alg="K-means")
df <- data.frame(Dimensions=c(4), Accuracy = vec, Alg="K-means")


df_kmeans <- rbind(df_kmeans, df)

df_kmeans

plot(df_kmeans$Dimensions , df_kmeans$Accuracy, type="n", cl=df_kmeans$Alg, xaxt='n')
axis(1, at = seq(1, 4, by = 1), las=1)
# new plot (adjusts Yrange automatically)
with (
  data = df_kmeans
  , expr = errbar(Dimensions, Accuracy, Accuracy+sd, Accuracy-sd, pch=1)
)


################################################# hirarchichal 1 dim
#models
set.seed(123)
#usage of hirarchical clustering 
clusters <- hclust(dist(x4))

#using cutree to keep three clusters 
clusterCut <- cutree(clusters, 3)

#confusion table
table(clusterCut, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 2
  else if (iris$species[i] == "setosa")
    species[i] <- 1
  else if (iris$species[i] == "virginica")
    species[i] <- 3
}
#species

sum(clusterCut == species) / 150 *100

#results hirarchical clustering  1 dim
#x1 => 67%
#x2 => 52%
#x3 => 83%
#x4 => 94%


df <- data.frame()
vec <- c(67,52,83,94)

#df_hi <- data.frame(Dimensions=c(1), Accuracy = mean(vec), sd = sd(vec), Alg="hirarchical")
df_hi <- data.frame(Dimensions=c(1), Accuracy = vec, Alg="hirarchical")

df_hi

################################################# hirarchichal 2 dim
#models
set.seed(123)
#usage of hirarchical clustering 
clusters <- hclust(dist(x34))

#using cutree to keep three clusters 
clusterCut <- cutree(clusters, 3)

#confusion table
table(clusterCut, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 3
  else if (iris$species[i] == "setosa")
    species[i] <- 1
  else if (iris$species[i] == "virginica")
    species[i] <- 2
}
#species

sum(clusterCut == species) / 150 *100
#results kmeans 1 dim
#x12 => 51%
#x13 => 84%
#x14 => 64%
#x23 => 87%
#x24 => 91%
#x34 => 86%

df <- data.frame()
vec <- c(51,84,64,87, 91, 86)

#df <- data.frame(Dimensions=c(2), Accuracy = mean(vec), sd = sd(vec), Alg="hirarchical")
df <- data.frame(Dimensions=c(2), Accuracy =vec, Alg="hirarchical")

df_hi <- rbind(df_hi, df)

################################################# hirarchichal 3 dim
#models
set.seed(123)
#usage of hirarchical clustering 
clusters <- hclust(dist(x124))

#using cutree to keep three clusters 
clusterCut <- cutree(clusters, 3)

#confusion table
table(clusterCut, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 2
  else if (iris$species[i] == "setosa")
    species[i] <- 1
  else if (iris$species[i] == "virginica")
    species[i] <- 3
}
#species

sum(clusterCut == species) / 150 *100
#results hirarchichal 3 dim
#x123 => 85%
#x234 => 89%
#x134 => 84%
#x124 => 60%

df <- data.frame()
vec <- c(85,89,84,60)

#df <- data.frame(Dimensions=c(3), Accuracy = mean(vec), sd = sd(vec), Alg="hirarchical")
df <- data.frame(Dimensions=c(3), Accuracy = vec, Alg="hirarchical")

df_hi <- rbind(df_hi, df)

df_hi

################################################# hirarchichal 4 dim
#models
set.seed(123)
#usage of hirarchical clustering 
clusters <- hclust(dist(iris[,1:4]))

#using cutree to keep three clusters 
clusterCut <- cutree(clusters, 3)

#confusion table
table(clusterCut, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 3
  else if (iris$species[i] == "setosa")
    species[i] <- 1
  else if (iris$species[i] == "virginica")
    species[i] <- 2
}
#species

sum(clusterCut == species) / 150 *100
#results hirarchichal 4 dim
#84%

vec <- c(84)

#df <- data.frame(Dimensions=c(4), Accuracy = mean(vec), sd = 0, Alg="hirarchical")
df <- data.frame(Dimensions=c(4), Accuracy = vec, Alg="hirarchical")


df_hi <- rbind(df_hi, df)

df_hi

plot(df_hi$Dimensions , df_hi$Accuracy, type="n",  xaxt='n')
axis(1, at = seq(1, 4, by = 1), las=1)
# new plot (adjusts Yrange automatically)
with (
  data = df_hi
  , expr = errbar(Dimensions, Accuracy, Accuracy+sd, Accuracy-sd, pch=1)
)
################################################# HDBSCAN 1 dim
#models
set.seed(123)
#usage of HDBSCAN clustering 
#cl <- hdbscan(iris[, 1:4], minPts = 20)
cl <- hdbscan(dist(x4), minPts =2)

#confusion table
table(cl$cluster, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 1
  else if (iris$species[i] == "setosa")
    species[i] <- 2
  else if (iris$species[i] == "virginica")
    species[i] <- 0
}

sum(cl$cluster == species) / 150 *100

#results hirarchical clustering  1 dim
#x1 => 54%
#x2 => 48%
#x3 => 68%
#x4 => 66%

df <- data.frame()
vec <- c(54,48,68,66)

#df_hd <- data.frame(Dimensions=c(1), Accuracy = mean(vec), sd = sd(vec), Alg="HDBSCAN")
df_hd <- data.frame(Dimensions=c(1), Accuracy = vec, Alg="HDBSCAN")


################################################# HDBSCAN 2 dim
#models
set.seed(123)
#usage of HDBSCAN clustering 
#cl <- hdbscan(iris[, 1:4], minPts = 20)
cl <- hdbscan(x34, minPts =20)

#confusion table
table(cl$cluster, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 1
  else if (iris$species[i] == "setosa")
    species[i] <- 2
  else if (iris$species[i] == "virginica")
    species[i] <- 0
}

sum(cl$cluster == species) / 150 *100
#results kmeans 2 dim
#x12 => 55%
#x13 => 69%
#x14 => 72%
#x23 => 66%
#x24 => 66%
#x34 => 67%

df <- data.frame()
vec <- c(55,69,72,66, 66, 67)

#df <- data.frame(Dimensions=c(2), Accuracy = mean(vec), sd = sd(vec), Alg="HDBSCAN")
df <- data.frame(Dimensions=c(2), Accuracy = vec, Alg="HDBSCAN")

df_hd <- rbind(df_hd, df)

df_hd
################################################# HDBSCAN 3 dim
#models
set.seed(123)
#usage of HDBSCAN clustering 
#cl <- hdbscan(iris[, 1:4], minPts = 20)
cl <- hdbscan(x134, minPts =20)

#confusion table
table(cl$cluster, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 1
  else if (iris$species[i] == "setosa")
    species[i] <- 2
  else if (iris$species[i] == "virginica")
    species[i] <- 0
}

sum(cl$cluster == species) / 150 *100

#results hirarchichal 3 dim
#x123 => 69%
#x234 => 66%
#x134 => 69%
#x124 => 68%

df <- data.frame()
vec <- c(69,66,69,68)

#df <- data.frame(Dimensions=c(3), Accuracy = mean(vec), sd = sd(vec), Alg="HDBSCAN")
df <- data.frame(Dimensions=c(3), Accuracy = vec, Alg="HDBSCAN")

df_hd <- rbind(df_hd, df)

df_hd
################################################# HDBSCAN 4 dim
#models
set.seed(123)
#usage of HDBSCAN clustering 
cl <- hdbscan(iris[, 1:4], minPts = 20)
#cl <- hdbscan(x134, minPts =20)

#confusion table
table(cl$cluster, iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 1
  else if (iris$species[i] == "setosa")
    species[i] <- 2
  else if (iris$species[i] == "virginica")
    species[i] <- 0
}

sum(cl$cluster == species) / 150 *100
#68%
vec <- c(68)

#df <- data.frame(Dimensions=c(4), Accuracy = mean(vec), sd = 0, Alg="HDBSCAN")
df <- data.frame(Dimensions=c(4), Accuracy = vec, Alg="HDBSCAN")


df_hd <- rbind(df_hd, df)

df_hd

plot(df_hd$Dimensions , df_hd$Accuracy, type="n",  xaxt='n')
axis(1, at = seq(1, 4, by = 1), las=1)
# new plot (adjusts Yrange automatically)
with (
  data = df_hd
  , expr = errbar(Dimensions, Accuracy, Accuracy+sd, Accuracy-sd, pch=1)
)
################################################# GMM 1 dim
#models
set.seed(123)
#usage of HDBSCAN clustering 
#cl <- hdbscan(iris[, 1:4], minPts = 20)

GMM_model <- Mclust(x4, 3)

#confusion table
table(GMM_model$classification , iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 2
  else if (iris$species[i] == "setosa")
    species[i] <- 1
  else if (iris$species[i] == "virginica")
    species[i] <- 3
}

sum(GMM_model$classification == species) / 150 *100

#results hirarchical clustering  1 dim
#x1 => 62%
#x2 => 48%
#x3 => 94%
#x4 => 95%

df <- data.frame()
vec <- c(62,48,94,95)

#df_gmm <- data.frame(Dimensions=c(1), Accuracy = mean(vec), sd = sd(vec), Alg="GMM")
df_gmm <- data.frame(Dimensions=c(1), Accuracy = vec, Alg="GMM")

################################################# GMM 2 dim
#models
set.seed(123)
#usage of HDBSCAN clustering 
#cl <- hdbscan(iris[, 1:4], minPts = 20)

GMM_model <- Mclust(x34, 3)

#confusion table
table(GMM_model$classification , iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 2
  else if (iris$species[i] == "setosa")
    species[i] <- 1
  else if (iris$species[i] == "virginica")
    species[i] <- 3
}

sum(GMM_model$classification == species) / 150 *100

#results kmeans 2 dim
#x12 => 71%
#x13 => 96%
#x14 => 68%
#x23 => 70%
#x24 => 91%
#x34 => 97%

df <- data.frame()
vec <- c(71,96,68,70, 91, 97)

#df <- data.frame(Dimensions=c(2), Accuracy = mean(vec), sd = sd(vec), Alg="GMM")
df <- data.frame(Dimensions=c(2), Accuracy = vec, Alg="GMM")

df_gmm <- rbind(df_gmm, df)

df_gmm

################################################# GMM 3 dim
#models
set.seed(123)
#usage of HDBSCAN clustering 
#cl <- hdbscan(iris[, 1:4], minPts = 20)

GMM_model <- Mclust(x124, 3)

#confusion table
table(GMM_model$classification , iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 2
  else if (iris$species[i] == "setosa")
    species[i] <- 1
  else if (iris$species[i] == "virginica")
    species[i] <- 3
}

sum(GMM_model$classification == species) / 150 *100

#results hirarchichal 3 dim
#x123 => 80%
#x234 => 88%
#x134 => 98%
#x124 => 92%

vec <- c(80,88,98, 92)

#df <- data.frame(Dimensions=c(3), Accuracy = mean(vec), sd = sd(vec), Alg="GMM")
df <- data.frame(Dimensions=c(3), Accuracy = vec, Alg="GMM")

df_gmm <- rbind(df_gmm, df)

df_gmm
################################################# GMM 4 dim
#models
set.seed(123)
#usage of HDBSCAN clustering 
#cl <- hdbscan(iris[, 1:4], minPts = 20)

GMM_model <- Mclust(iris[, 1:4], G=3)

#confusion table
table(GMM_model$classification , iris$species)

species <- numeric(length(iris$species))

for (i in 1:150){
  if (iris$species[i] == "versicolor")
    species[i] <- 2
  else if (iris$species[i] == "setosa")
    species[i] <- 1
  else if (iris$species[i] == "virginica")
    species[i] <- 3
}

sum(GMM_model$classification == species) / 150 *100
#96%

vec <- c(96)

#df <- data.frame(Dimensions=c(4), Accuracy = mean(vec), sd = 0, Alg="GMM")
df <- data.frame(Dimensions=c(4), Accuracy = vec, Alg="GMM")


df_gmm <- rbind(df_gmm, df)

df_gmm

plot(df_gmm$Dimensions , df_gmm$Accuracy, type="n",  xaxt='n')
axis(1, at = seq(1, 4, by = 1), las=1)
# new plot (adjusts Yrange automatically)
with (
  data = df_gmm
  , expr = errbar(Dimensions, Accuracy, Accuracy+sd, Accuracy-sd, pch=1)
)


#errors
#gmm  98
#kmeans 96
#hirarchical 94
#hdbscan 72

model_errors <- c(28, 6, 4, 2)

barplot(model_errors, main="Car Distribution", horiz=FALSE,
        names.arg=c("HDBSCAN", "Hirarchical", "k-means", "GMM"))


df_kmeans
df_hi
df_hd
#df_hd <- df_hd[-c(5),] 
df_gmm

df <- rbind(df_kmeans, df_hi, df_hd, df_gmm)
df
write.csv(df_kmeans, "clustering_dims_kmeans.csv")


boxplot(Accuracy~Dimensions,
        data=df,
        main="Different boxplots for eachdimension",
        xlab="Dimensions",
        ylab="Matching Accuracy",
        col="green",
        border="brown"
       
)

########################################################## hypothesis testing 
df
df1 <- df[df$Dimensions == 1,]
df2 <- df[df$Dimensions == 2,]
df3 <- df[df$Dimensions == 3,]
df4 <- df[df$Dimensions == 4,]

nrow(df2)

x1 <- sample(df1$Accuracy, size=nrow(df2), replace=TRUE)
x2 <- df2$Accuracy 
x3 <- sample(df3$Accuracy, size=nrow(df2), replace=TRUE)
x4 <- sample(df4$Accuracy, size=nrow(df2), replace=TRUE)

Dimensions <- rep(1,nrow(df2))
Accuracy <- x2
data1 <- data.frame(Accuracy, Dimensions)

Dimensions <- rep(4,nrow(df2))
Accuracy <- x3
data2 <- data.frame(Accuracy, Dimensions)
#data2

data <- rbind(data1, data2)

data$Dimensions <- factor(data$Dimensions)

#df12 <- rbind(df[df$Dimensions == 1,] , df[df$Dimensions == 2,])


t.test(Accuracy ~ Dimensions, data = data, paired = TRUE)

######################################    permutation 

data
favstats(~Accuracy | Dimensions, data=data)
diff <- favstats(~Accuracy | Dimensions, data=data)$mean[1] - favstats(~Accuracy | Dimensions, data=data)$mean[2]
diff
mean(x1)
mean(x2)
mean(x3)
mean(x4)

set.seed(1234)
diff <- mean(df3$Accuracy) - mean(df4$Accuracy)
Accuracy <- c(df3$Accuracy,df4$Accuracy)
Accuracy

M = (30001 - 1)  #subtract 1 from 3001 to do 3000
outcome2 = numeric(M)
for(i in 1:M)
{ index1 = sample(length(Accuracy), 10, replace=FALSE)
outcome2[i] = mean(Accuracy[index1]) - mean(Accuracy[-index1])
}

hist(outcome2, col='blue', ylim = c(0,5000), xlim=c(-20,20), xlab="Difference of mean(One Dimensions Accuracy)  - Mean(Four Dimensions Accuracy)", main="Outcome of Permutation Test")
abline(v=diff, col="red",lwd=4)
diff
1 - (sum(outcome2 <= diff))/(M)
#############################################################

##########################  t.test
tt <- t.test(Accuracy~Dimensions,data=data, conf.level = 0.81)
tt
ggttest(tt)
df1
x1
hist(x4-x1,col="gray",las=1,main="")
abline(v=obs,col="red")

##################################  lmp
summary(lmp(Accuracy~Dimensions,data=data))

##################################  coin
## default: asymptotic
oneway_test(Accuracy~Dimensions,data=data)

## exact distribution
oneway_test(Accuracy~Dimensions,data=data,distribution="exact")

## approximate (random-sampling/Monte Carlo)
oneway_test(Accuracy~Dimensions,data=data,distribution=approximate(nresample=9999))

#precision
precision_max <- max(df[df$Dimensions == 4,]$Accuracy)
precision_min <- min(df[df$Dimensions == 4,]$Accuracy)

range <- (precision_max- precision_min)

precision <- (100-range)

precision

#dimensions 1, 2, 3, 4
#precision 52%, 54%, 62%, 72%

pres <- c(52, 54, 62, 72)

barplot (pres,ylim = c(40,100), col="blue" )
###################################################################

library("mlbench")
data(Sonar)
ncol(Sonar)

library("modeldata")
data(mlc_churn, package = "modeldata")
ncol(mlc_churn)
View(mlc_churn)


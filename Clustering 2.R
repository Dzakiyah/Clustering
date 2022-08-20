library(cluster)
library(factoextra)
library(tidyverse)
library(mclust)

data<- read.csv("D:/data_cluster.csv", sep=";")
head(data)
str(data)
rowness(data) <- data$Kota.kab
dataclus <- data[,-1]

dataclus1 <- na.omit(dataclus)
head(dataclus1)
nrow(dataclus1)

#standarisasi data
datafix <- scale(dataclus) #standarisasi data
datafix
#agglomerative hirarchical clustering
# Dissimilarity matrix
d <- dist(datafix, method = "euclidean") #mendapatkan kedekatan titik
d
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" ) #metode bisa milih complete, single, average
win.graph()
plot(hc1, cex = 0.6, hang = -1)

# Cut tree into 4 groups
sub_grp <- cutree(hc1, k = 4) #jumlah k diganti sesuai pemotongan
data.frame(sub_grp)
# Number of members in each cluster
table(sub_grp)
dataclus %>%
  mutate(cluster = sub_grp) %>%
  head
#plot mendapatkan grup
win.graph()
plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 4, border = 2:5)

#gambar petanya
win.graph()
fviz_cluster(list(data = datafix, cluster = sub_grp))

dataclus %>%
  mutate(cluster = sub_grp) %>%
  group_by(cluster) %>%
  summarise_all("mean")

# single linkage
hc2 <- hclust(d, method = "single" ) 
win.graph()
plot(hc2, cex = 0.6, hang = -1)

# Cut tree into 4 groups
sub_grp1 <- cutree(hc2, k = 2)

# Number of members in each cluster
table(sub_grp1)
dataclus %>%
  mutate(cluster = sub_grp1) %>%
  head
#plot mendapatkan grup
win.graph()
plot(hc2, cex = 0.6)
rect.hclust(hc2, k = 2, border = 2:5)

win.graph()
fviz_cluster(list(data = datafix, cluster = sub_grp1))

new_data <- cbind(dataclus,sub_grp)
new_data


###====================K-Means===================================
library(dplyr)
#membentuk k means dengan k=3
km_fit = kmeans(datafix,centers = 3,iter.max = 300 )
km_fit
win.graph()
fviz_cluster(km_fit, data = datafix)

km_fit1 = kmeans(datafix,centers = 4,iter.max = 300 )
win.graph()
fviz_cluster(km_fit1, data = datafix)
new_data <- cbind(dataclus,data.frame(km_fit1$cluster))
new_data$km_fit1.cluster <- as.factor(new_data$km_fit1.cluster)

##########################ICD RATE dan Pseudo F-Statistic###################
icdrate = function(Data, nc, c)
{
  n = dim(Data)[1]
  p = dim(Data)[2]
  X = Data[,1:(p-1)]
  Group = Data[,p]
  p = dim(X)[2]
  Mean.X = matrix(ncol = p, nrow = (nc+1))
  for (i in 1:nc)
  {
    for (j in 1:p)
    {
      Mean.X[i,j] = mean(X[which(Group==i),j])
      Mean.X[(nc+1),j] = mean(X[,j])
    }
  }
  SST = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      SST[i,j] = (X[i,j] - Mean.X[(nc+1),j])^2
    }
  }
  SST = sum(sum(SST))
  SSE = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      for (k in 1:nc)
      {
        if (Group[i]==k)
        {
          SSE[i,j] = (X[i,j] - Mean.X[k,j])^2
        }
      }
    }
  }
  SSE = sum(sum(SSE))
  Rsq = (SST-SSE)/SST
  icdrate = 1-Rsq
  Pseudof = (Rsq/(c-1))/((icdrate)/(nc-c))
  ssb=SST-SSE
  list(SSW=SSE, SST=SST, SSB=ssb, Rsq=Rsq, icdrate=icdrate, pseudof=Pseudof)
}

icdrate(new_data,length(new_data),3)

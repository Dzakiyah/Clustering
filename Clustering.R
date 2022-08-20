library(cluster)    # Algoritma klastering
library(factoextra) # Algoritma klastering dan visualisasi
library(tidyverse)
library(dplyr)
library(fpc)
library(mvnormtest)

data=read.csv("D:/Data1_AD.csv", header=TRUE, sep=";")
head(data)
str(data)
rownames(data) <- data$Kecamatan
dataclus <- data[,-c(1,2)]
#check missing values
dataclus1 <- na.omit(dataclus)
head(dataclus1)
summary(dataclus1)
nrow(dataclus1)
datafix=dataclus1

#Pemilihan k optimum
fviz_nbclust(datafix, kmeans, method="wss") #Metode elbow, k optimum=garis antara 2 titik yang paling mendatar
fviz_nbclust(datafix, kmeans, method="silhouette") #Metode siluet, k optimum=titik yang ada garis titik2nya

d <- dist(datafix, method = "euclidean") #mendapatkan kedekatan titik

########### Complete Linkage ############
hc1 <- hclust(d, method = "complete" ) #metode bisa milih complete, single, average
win.graph()
plot(hc1, cex = 0.6, hang = -1)

# Cut tree into n groups
sub_grp <- cutree(hc1, k = 6) #jumlah k diganti sesuai pemotongan

# Number of members in each cluster
table(sub_grp)
dataclus %>%
  mutate(cluster = sub_grp) %>%
  head
#plot mendapatkan grup
win.graph()
plot(hc1, cex = 0.6)
rect.hclust(hc1, k = 6, border = 2:5)

#gambar petanya
win.graph()
fviz_cluster(list(data = datafix, cluster = sub_grp))

dataclus %>%
  mutate(cluster = sub_grp) %>%
  group_by(cluster) %>%
  summarise_all("mean")

new_data_CL <- cbind(dataclus,sub_grp)

#Silhouette score
databind_1=cbind(dataclus, Cluster=sub_grp)
jarak=as.matrix(d)
score_CL=mean(silhouette(databind_1$Cluster, dmatrix=jarak)[,3])

################### Single Linkage #########################
dataclus2 <- data[,-c(1,2)]
hc2 <- hclust(d, method = "single" ) #metode bisa milih complete, single, average
win.graph()
plot(hc2, cex = 0.6, hang = -1)

# Cut tree into n groups
sub_grp1 <- cutree(hc2, k = 6)

# Number of members in each cluster
table(sub_grp1)
dataclus2 %>%
  mutate(cluster = sub_grp1) %>%
  head
#plot mendapatkan grup
win.graph()
plot(hc2, cex = 0.6)
rect.hclust(hc2, k = 6, border = 2:5)

win.graph()
fviz_cluster(list(data = datafix, cluster = sub_grp1))

#Silhouette score
databind_2=cbind(dataclus2, Cluster=sub_grp1)
score_SL=mean(silhouette(databind_2$Cluster, dmatrix=jarak)[,3])

new_data_SL <- cbind(dataclus2,sub_grp1)

################### Average Linkage #########################
dataclus3 <- data[,-c(1,2)]
hc3 <- hclust(d, method = "average" ) #metode bisa milih complete, single, average
win.graph()
plot(hc3, cex = 0.6, hang = -1)

# Cut tree into n groups
sub_grp2 <- cutree(hc3, k = 7)

# Number of members in each cluster
table(sub_grp2)
dataclus3 %>%
  mutate(cluster = sub_grp2) %>%
  head
#plot mendapatkan grup
win.graph()
plot(hc3, cex = 0.6)
rect.hclust(hc3, k = 7, border = 2:5)

win.graph()
fviz_cluster(list(data = datafix, cluster = sub_grp2))

#Silhouette score
databind_3=cbind(dataclus3, Cluster=sub_grp2)
score_AL=mean(silhouette(databind_3$Cluster, dmatrix=jarak)[,3])

new_data_AL <- cbind(dataclus3,sub_grp2)

####################### K-Means ##########################
#membentuk k means dengan k=2
dataclus4 <- data[,-c(1,2)]
km_fit = kmeans(datafix,centers = 7,iter.max = 300 )
km_fit
win.graph()
fviz_cluster(km_fit, data = datafix)

km_fit1 = kmeans(datafix,centers = 7,iter.max = 300 )
win.graph()
fviz_cluster(km_fit1, data = datafix)
new_data_KM <- cbind(dataclus4,data.frame(km_fit1$cluster))
new_data_KM$km_fit1.cluster <- as.factor(new_data_KM$km_fit1.cluster)

databind_4=cbind(new_data_KM, Cluster=new_data_KM$km_fit1.cluster)
score_KM <- function(k){
  km <- kmeans(datafix, centers = k)
  ss <- silhouette(km$cluster, dist(datafix))
  mean(ss[, 3])
}
k <- 7
avg_sil <- sapply(k, score_KM)

###################### K-Medoids ###########################
fviz_nbclust(datafix, cluster::pam, method = "silhouette")
pamk.result <- pamk(datafix)
pamk.result$nc

pam.result <- pam(datafix,7)
# jarak
pam.result$diss
#dataframe hasil cluster ###
df.cluster = data.frame(datafix,pam.result$cluster)
View(df.cluster)
#plot cluster
fviz_cluster(pam.result, data = datafix)

databind_5=cbind(dataclus, data.frame(df.cluster$pam.result.cluster))
score_KME=mean(silhouette(df.cluster$pam.reslut.cluster, dmatrix=pam.result$diss)[,3])

new_data_KME <- cbind(dataclus,data.frame(df.cluster$pam.result.cluster))
new_data_KME$df.cluster <- as.factor(new_data_KME$df.cluster)

################# silhouette score ##################
print(paste("Silhouette Score Complete Linkage = ", round(score_CL, 3)))
print(paste("Silhouette Score Single Linkage = ", round(score_SL, 3)))
print(paste("Silhouette Score Average Linkage = ", round(score_AL, 3)))
print(paste("Silhouette Score K-Means = ", round(avg_sil, 3)))
print(paste("Silhouette Score K-Medoids = ", round(score_KME, 3)))

############### ICD Rate dan Pseudo F Statistics ####################
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

print("ICD Rate dan Pseudo-f Complete Linkage= "); icdrate(new_data_CL,length(new_data_CL),3)
print("ICD Rate dan Pseudo-f Single Linkage= "); icdrate(new_data_SL,length(new_data_SL),3)
print("ICD Rate dan Pseudo-f Average  Linkage= "); icdrate(new_data_AL,length(new_data_AL),3)
print("ICD Rate dan Pseudo-f K-Means= "); icdrate(new_data_KM,length(new_data_KM),3)
print("ICD Rate dan Pseudo-f K-Medoids= "); icdrate(new_data_KME,length(new_data_KME),3)

######################### Uji Normalitas Multivariat ########################
klaster1=read.csv("D:/klaster1.csv", header=TRUE, sep=";")
klaster1=klaster1[,-c(1,1)]
head(klaster1)

x1=klaster1$x1; x2=klaster1$x2; x3=klaster1$x3; x4=klaster1$x4; x5=klaster1$x5
x1=t(x1); x2=t(x2); x3=t(x3); x4=t(x4); x5=t(x5)

mshapiro.test(x1)
mshapiro.test(x2)
mshapiro.test(x3)
mshapiro.test(x4)
mshapiro.test(x5)

klaster2=read.csv("D:/klaster2.csv", header=TRUE, sep=";")
klaster2=klaster2[,-c(1,1)]
head(klaster2)

x1=klaster2$x1; x2=klaster2$x2; x3=klaster2$x3; x4=klaster2$x4; x5=klaster2$x5

x1=t(x1); x2=t(x2); x3=t(x3); x4=t(x4); x5=t(x5)

mshapiro.test(x1)
mshapiro.test(x2)
mshapiro.test(x3)
mshapiro.test(x4)
mshapiro.test(x5)

klaster3=read.csv("D:/klaster3.csv", header=TRUE, sep=";")
klaster3=klaster3[,-c(1,1)]
head(klaster3)

x1=klaster3$x1; x2=klaster3$x2; x3=klaster3$x3; x4=klaster3$x4; x5=klaster3$x5
x1=t(x1); x2=t(x2); x3=t(x3); x4=t(x4); x5=t(x5)

mshapiro.test(x1)
mshapiro.test(x2)
mshapiro.test(x3)
mshapiro.test(x4)
mshapiro.test(x5)

klaster4=read.csv("D:/klaster4.csv", header=TRUE, sep=";")
klaster4=klaster4[,-c(1,1)]
head(klaster4)

x1=klaster4$x1; x2=klaster4$x2; x3=klaster4$x3; x4=klaster4$x4; x5=klaster4$x5
x1=t(x1); x2=t(x2); x3=t(x3); x4=t(x4); x5=t(x5)

mshapiro.test(x1)
mshapiro.test(x2)
mshapiro.test(x3)
mshapiro.test(x4)
mshapiro.test(x5)

klaster5=read.csv("D:/klaster5.csv", header=TRUE, sep=";")
klaster5=klaster5[,-c(1,1)]
head(klaster5)

x1=klaster5$x1; x2=klaster5$x2; x3=klaster5$x3; x4=klaster5$x4; x5=klaster5$x5
x1=t(x1); x2=t(x2); x3=t(x3); x4=t(x4); x5=t(x5)

mshapiro.test(x1)
mshapiro.test(x2)
mshapiro.test(x3)
mshapiro.test(x4)
mshapiro.test(x5)

klaster6=read.csv("D:/klaster6.csv", header=TRUE, sep=";")
klaster6=klaster6[,-c(1,1)]
head(klaster6)

x1=klaster6$x1; x2=klaster6$x2; x3=klaster6$x3; x4=klaster6$x4; x5=klaster6$x5
x1=t(x1); x2=t(x2); x3=t(x3); x4=t(x4); x5=t(x5)

mshapiro.test(x1)
mshapiro.test(x2)
mshapiro.test(x3)
mshapiro.test(x4)
mshapiro.test(x5)

klaster6=read.csv("D:/klaster6.csv", header=TRUE, sep=";")
klaster6=klaster6[,-c(1,1)]
klaster6=t(klaster6)
mshapiro.test(klaster6)
head(klaster6)

normal=t(datafix)
mshapiro.test(normal)

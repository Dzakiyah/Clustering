data=read.csv("D:/Praktikum_Unsupervised.csv", header=TRUE, sep=",")
head(data)

#Menghitung kolom 1 dan 2
data_unsu=data[ ,-c(1,2)]
head(data_unsu)

#Mengganti nama kolom
colnames(data_unsu)=c("Umur", "Pendapatan", 'Skor')
head(data_unsu)

#Cek Missing Value
summary(data_unsu)
sum(is.na(data_unsu))

#Menentukan k optimum
library(factoextra)
fviz_nbclust(data_unsu, kmeans, method="wss") #Metode elbow, k optimum=garis antara 2 titik yang paling mendatar
fviz_nbclust(data_unsu, kmeans, method="silhouette") #Metode siluet, k optimum=titik yang ada garis titik2nya

#Algoritma clustering
set.seed(123) #Agar hasil cluster tidak berubah
k.means.fit=kmeans(data_unsu, iter.max=1000, 7)
k.means.fit$centers
k.means.fit

#Menghitung Silhouette score
library(cluster)
jarak=as.matrix(dist(data_unsu))
score=mean(silhouette(k.means.fit$cluster, dmatrix=jarak)[,3])
print(paste("silhouette score = ", round(score, 3)))

#Visualisasi
clusplot(data_unsu, k.means.fit$cluster, main="Visualisasi Cluster", color=TRUE, shade=TRUE, lines=0)

#==========================================================
#Aglomeratif Hirarki Clustering
#Menghitung matriks distance
d=dist(x=data_unsu, method="euclidean")

#Membangun cluster
hc_single=hclust(d=d, method="single")

#Membuat dendogram
plot(hc_single, hang=-1)

library(dplyr)
#Tabel hasil
cut_point_1=cutree(hc_single, k=3) #Memilih sebanyak 7 cluster
databind_1=cbind(data_unsu, Cluster=cut_point_1)
head(databind_1)

#Menghitung silhouette score
jarak=as.matrix(d)
score=mean(silhouette(databind_1$Cluster, dmatrix=jarak)[,3])
print(paste("silhouette score = ", round(score, 3)))

#Menghitung Karakteristik
data_unsu %>%
  group_by(databind_1$Cluster) %>%
  summarise_all(funs(mean=mean, median=median)) %>%
  as.data.frame()

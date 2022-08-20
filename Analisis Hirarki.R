data <- read.csv("D:/Data_Clustering_AD.csv", header=TRUE, sep = ";")

#Preprocessing
summary(data)
sum(is.na(data))
data <- subset(data, select = -Provinsi)
summary(data)

#Menghitung matriks distance
d=dist(x=data, method="euclidean")

#Metode Single Linkage
hc_single=hclust(d=d, method="single")

#Membuat dendogram single linkage
plot(hc_single, hang=-1)

#Metode Complete Linkage
hc_complete=hclust(d=d, method="complete")

#Membuat dendogram complete linkage
plot(hc_complete, cex=0.6, hang=-1)
abline(h=2, col='red')

library(dplyr)
#Tabel hasil single linkage
cut_point_1=cutree(hc_single, k=2) #Sebanyak 9 cluster
databind_1 <- cbind(data, Cluster = cut_point_1)  # Bind data
head(databind_1)

#Tabel Hasil Complete Linkage
cut_point_2 = cutree(hc_complete, k = 2) #Memilih sebanyak 2 klaster
databind_2 <- cbind(data, Cluster = cut_point_2)  # Bind data 
head(databind_2)

data %>%
  group_by(databind_1$Cluster) %>%
  summarise_all(funs(mean = mean, median = median, sd = sd)) %>%
  as.data.frame()

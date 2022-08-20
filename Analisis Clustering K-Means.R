data <- read.csv("D:/CC GENERAL.csv",sep = ",")

#Preprocessing
summary(data)
library(ggplot2)
ggplot(data,aes(x=CREDIT_LIMIT))+geom_histogram(aes(y=..density..),colour="black", fill="white")+geom_density(alpha=0.1,fill="red")
ggplot(data,aes(x=MINIMUM_PAYMENTS))+geom_histogram(aes(y=..density..),colour="black", fill="white")+geom_density(alpha=0.1,fill="red")
data$CREDIT_LIMIT[is.na(data$CREDIT_LIMIT)] <- median(data$CREDIT_LIMIT, na.rm = TRUE)
data$MINIMUM_PAYMENTS[is.na(data$MINIMUM_PAYMENTS)] <- median(data$MINIMUM_PAYMENTS, na.rm = TRUE)
summary(data)
sum(is.na(data))
data <- subset(data, select = -CUST_ID)
summary(data)

#Menentukan k Optimum
library(factoextra)
# Elbow method = optimum pada antara 2 titik yang grafiknya melandai
fviz_nbclust(data, kmeans, method = "wss")
# Silhouette method = optimum pada titik tertinggi (hasil 2 metode ini dibandingkan dan dipilih yang paling sesuai kriteria optimum masing2 metode)
fviz_nbclust(data, kmeans, method = "silhouette")

#Analisis Cluster
kmeans_1 <- kmeans(data,6)
databind_1 <- cbind(data, Cluster = kmeans_1$cluster)
head(databind_1)
options(scipen = 9)
centers_1 <- data.frame(kmeans_1$centers)
centers_1$CLUSTER <- 1:nrow(centers_1)
centers_1s <- data.frame(t(centers_1[-18]))
colnames(centers_1s) <- centers_1[,18]
centers_1s
g1 <- fviz_cluster(kmeans_1, geom=c("point","text"), data=data) + ggtitle("k = 6") + theme_light()
plot(g1)
library(dplyr)
data %>%
  group_by(kmeans_1$cluster) %>%
  summarise_all(funs(mean = mean, median = median, sd = sd)) %>%
  as.data.frame()

#K = 5
kmeans_2 <- kmeans(data,5)
databind_2 <- cbind(data, Cluster = kmeans_2$cluster)
head(databind_2)
centers_2 <- data.frame(kmeans_2$centers)
centers_2$CLUSTER <- 1:nrow(centers_2)
centers_2s <- data.frame(t(centers_2[-18]))
colnames(centers_2s) <- centers_2[,18]
centers_2s
g2 <- fviz_cluster(kmeans_2, geom=c("point","text"), data=data) + ggtitle("k = 5") + theme_light()
plot(g2)
data %>%
  group_by(kmeans_2$cluster) %>%
  summarise_all(funs(mean = mean, median = median, sd = sd)) %>%
  as.data.frame()

#K = 9
kmeans_3 <- kmeans(data,9)
databind_3 <- cbind(data, Cluster = kmeans_3$cluster)
head(databind_3)
centers_3 <- data.frame(kmeans_3$centers)
centers_3$CLUSTER <- 1:nrow(centers_3)
centers_3s <- data.frame(t(centers_3[-18]))
colnames(centers_3s) <- centers_3[,18]
centers_3s
g3 <- fviz_cluster(kmeans_3, geom=c("point","text"), data=data) + ggtitle("k = 9") + theme_light()
plot(g3)
data %>%
  group_by(kmeans_3$cluster) %>%
  summarise_all(funs(mean = mean, median = median, sd = sd)) %>%
  as.data.frame()
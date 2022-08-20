data <- read.csv("C:/Users/asus/Downloads/archive/CC GENERAL.csv",sep = ",")
summary(data)

# preprocessing
library(ggplot2)
ggplot(data,aes(x=CREDIT_LIMIT))+
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=0.1,fill="red")

ggplot(data,aes(x=MINIMUM_PAYMENTS))+
  geom_histogram(aes(y=..density..),colour="black", fill="white")+
  geom_density(alpha=0.1,fill="red")

# filling missing value
data$CREDIT_LIMIT[is.na(data$CREDIT_LIMIT)] <- median(data$CREDIT_LIMIT, na.rm = TRUE)
data$MINIMUM_PAYMENTS[is.na(data$MINIMUM_PAYMENTS)] <- median(data$MINIMUM_PAYMENTS, na.rm = TRUE)

summary(data)
sum(is.na(data))

data <- subset(data, select = -CUST_ID)
summary(data)

# Menghitung matriks distance
d <- dist(x = data, method = "euclidean")

# Metode single linkage
hc_single <- hclust(d = d, method = "single")

# Membuat Dendogram single linkage
plot(hc_single, hang = -1)

# Metode complete linkage
hc_complete <- hclust(d = d, method = "complete")

# Membuat dendogram complete linkage
plot(hc_complete, cex = 0.6, hang = -1)
abline(h = 3, col = 'red')

library(dplyr)

#Tabel Hasil Single Linkage
cut_point_1 = cutree(hc_single, k = 2) #Memilih sebanyak 2 klaster
databind_1 <- cbind(data, Cluster = cut_point_1)  # Bind data
head(databind_1)

#Tabel Hasil Complete Linkage
cut_point_2 = cutree(hc_complete, k = 2) #Memilih sebanyak 2 klaster
databind_2 <- cbind(data, Cluster = cut_point_2)  # Bind data 
head(databind_2)


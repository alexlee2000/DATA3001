library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms
library(dplyr) # Working with data frames
library(gridExtra) # For displaying graph
data_pb <- read.csv("bid_merged_pb_FIN.csv", header = TRUE)
data_ba <- read.csv("bid_merged_ba_FIN.csv", header = TRUE)
set.seed(123)
# select the pc1 and pc2 for price bands and band availability
pca_pb <- select(data_pb, PC1, PC2)
pca_ba <- select(data_ba, PC1, PC2)
pca_ba2 <- select(data_ba, BANDAVAIL1,BANDAVAIL2, BANDAVAIL3, BANDAVAIL4, BANDAVAIL5,
                  BANDAVAIL6, BANDAVAIL7, BANDAVAIL8, BANDAVAIL9, BANDAVAIL10)

# Scale data to variance 1 mean 0. 
pca_ba2_scale <- scale(pca_ba2)
head(pca_ba2_scale)
summary(pca_ba2_scale)

# Band available k means
k2b <- kmeans(pca_ba2_scale, centers = 2, nstart = 25)
k3b <- kmeans(pca_ba2_scale, centers = 3, nstart = 25)
k4b <- kmeans(pca_ba2_scale, centers = 4, nstart = 25)
k5b <- kmeans(pca_ba2_scale, centers = 5, nstart = 25)

p2b <- fviz_cluster(k2b, geom = "point", data = pca_ba2_scale) + ggtitle("k = 2")
p3b <- fviz_cluster(k3b, geom = "point", data = pca_ba2_scale) + ggtitle("k = 3")
p4b <- fviz_cluster(k4b, geom = "point", data = pca_ba2_scale) + ggtitle("k = 4")
p5b <- fviz_cluster(k5b, geom = "point", data = pca_ba2_scale) + ggtitle("k = 5")

grid.arrange(p2b, p3b, p4b, p5b, nrow = 4)
# Price band kmeans
pca_pb2 <- select(data_pb, PRICEBAND1,PRICEBAND2, PRICEBAND3, PRICEBAND4, PRICEBAND5,
                  PRICEBAND6, PRICEBAND7, PRICEBAND8, PRICEBAND9, PRICEBAND10)
pca_pb2_scale <- scale(pca_pb2)
head(pca_pb2_scale)
summary(pca_pb2_scale)

k2c <- kmeans(pca_pb2_scale, centers = 2, nstart = 25)
k3c <- kmeans(pca_pb2_scale, centers = 3, nstart = 25)
k4c <- kmeans(pca_pb2_scale, centers = 4, nstart = 25)
k5c <- kmeans(pca_pb2_scale, centers = 5, nstart = 25)

p2c <- fviz_cluster(k2c, geom = "point", data = pca_pb2_scale) + ggtitle("k = 2")
p3c <- fviz_cluster(k3c, geom = "point", data = pca_pb2_scale) + ggtitle("k = 3")
p4c <- fviz_cluster(k4c, geom = "point", data = pca_pb2_scale) + ggtitle("k = 4")
p5c <- fviz_cluster(k5c, geom = "point", data = pca_pb2_scale) + ggtitle("k = 5")

grid.arrange(p2c, p3c, p4c, p5c, nrow = 4)
##### Clusering base script workshop 2020 ####
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# read in data
df <- USArrests
df <- na.omit(df) # remove missing obs

df <- scale(df) # scale and center the data 

# 1) unsupervised data analysis 

# k-means clustering 
k2 <- kmeans(df, centers = 2, nstart = 25) # 2 clusters 

fviz_cluster(k2, data = df) # plot the clusters of the majority of the varaince

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare, using more than 2 cluster
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2) # plot all clusters next to one another

# choose number of clusers via gap-statistics 
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

fviz_gap_stat(gap_stat) #plot the results

# set the final results
final <- kmeans(df, 3, nstart = 25)
print(final)

fviz_cluster(final, data = df)

# choose from the "elbow" plot
set.seed(123)
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values=c()
for ( i in 1:length(k.values) ){
  wss_values[i] <- wss(i)
}

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# choosing different disimilarity measures
library(NbClust)

diss_matrix<- dist(df, method = "euclidean", diag=FALSE)

res<-NbClust(df, diss=diss_matrix, distance = NULL, min.nc=2, max.nc=25, 
             method = "complete", index = "gap")

res

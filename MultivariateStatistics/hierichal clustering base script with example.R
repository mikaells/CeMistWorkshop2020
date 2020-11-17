##### Clusering base script workshop 2020 ####
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms
# hierichal clustering 

# read in data
df <- USArrests

df <- na.omit(df) # remove missing values

# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# assement of clusering methods
m <- c( "average", "single", "complete", "ward") # methods
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

assesment=c()
for (i in 1:length(m)){
  assesment[i]=ac(m[i])
}
output=data.frame(assesment,"method"=m)

output # evaluate the strongeste strucutre 

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

# cut the dendrogram
hc5 <- hclust(d, method = "ward.D2" ) # Ward's method

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 4)

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)

# plotting the dendograms as clusters
fviz_cluster(list(data = df, cluster = sub_grp))

# compare the two methods of clustering
# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)

dend_list <- dendlist(dend1, dend2)


# compare methods via a number from entanglement 

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

# find optimal number of clusters via gap-statsitic
gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)



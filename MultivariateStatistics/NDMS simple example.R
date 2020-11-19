### multi dimensional scaling base script ####
library(ggplot2)
library(vegan)
library(MASS)
library(ChemometricsWithRData, lib.loc = "C:/Program Files/R/R-3.6.2/library")

# read in data
data(wines)
data(wines)

# classic MDS
wines.dist <- dist(scale(wines))
wines.cmdscale <- cmdscale(wines.dist,eig = TRUE,k=2)

# have the class annoation be the coloring guide 
ggplot(as.data.frame(wines.cmdscale), aes(x=wines.cmdscale[,1], y=wines.cmdscale[,2], col=wine.classes)) +
  geom_point() +
  labs(title = "classic MDS Plot")


# sammon mapping 
wines.sammon <- sammon(wines.dist)

ggplot(as.data.frame(wines.sammon$points), aes(x=wines.sammon$points[,1], y=wines.sammon$points[,2], col=wine.classes)) +
  geom_point() +
  labs(title = "classic MDS Plot")


# non-metric MDS
# find the best distance measure
rank.totus <- rankindex(wine.classes, wines, indices = c("bray", "euclid", "manhattan", "horn"), method = "spearman")

wines_dist = as.matrix((vegdist(wines, "manhattan")))


NMDS=isoMDS(wines.dist) # from mass package
NMDS = metaMDS(wines.dist) # from vegan package

#build a data frame with NMDS coordinates and metadata
MDS1 = NMDS$points[,1]
MDS2 = NMDS$points[,2]
NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, class=wine.classes)

ggplot(NMDS, aes(x=MDS1, y=MDS2, col=NMDS$class)) +
  geom_point() +
  labs(title = "NMDS Plot")


# compare:

NMDS$stress
wines.sammon$stress
wines.cmdscale$



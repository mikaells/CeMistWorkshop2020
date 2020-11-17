#### multidimensional scaling base script 2020 workshop ####

library(vegan)
library(ggplot2)

# read in data
setwd("C:/Users/mabso/Desktop/PhD/workshop 2020/data")

otus = read.csv(file = "EPIPHYTE_OTU_TABLE.csv", header = TRUE, check.names = FALSE, row.names = 1)
metadata = read.csv(file = "EPIPHYTE_METADATA.csv", header = TRUE, check.names = FALSE, row.names = 1)

# remove NA's
good_samples <- colnames(otus[(colSums(decostand(otus,"pa")) >= 1)])     # decostand(x,"pa") counts presence/absence
otus = otus[,good_samples]
metadata = metadata[good_samples,]

t_otus <- as.data.frame(t(otus)) # samples =rows and OUTs=columns

min_depth = min(colSums(otus))
t_otus_rarefied <- as.data.frame(round(rrarefy(t_otus, min_depth)))

sqrt_t_otus_rarefied = sqrt(t_otus_rarefied)
rank.totus <- rankindex(as.matrix(sqrt_t_otus_rarefied), t_otus_rarefied, indices = c("bray", "euclid", "manhattan", "horn"), method = "spearman")
print(paste("The highest rank was given by the", names(sort(rank.totus, decreasing = TRUE)[1]), "method."))

otus_dist = as.matrix((vegdist(t_otus_rarefied, "bray")))

# do the NDMS
NMDS = metaMDS(otus_dist)

#build a data frame with NMDS coordinates and metadata
MDS1 = NMDS$points[,1]
MDS2 = NMDS$points[,2]
NMDS = data.frame(MDS1 = MDS1, MDS2 = MDS2, Host = metadata$Host_Taxon, Location = metadata$Location)

ggplot(NMDS, aes(x=MDS1, y=MDS2, col=Location)) +
  geom_point() +
  stat_ellipse() +
  theme_bw() +
  labs(title = "NMDS Plot")

# testing overall influence of the loacation factor 

# testing the fidings via anosim
anosim_location = anosim(otus_dist, metadata$Location)
anosim_location # take a look at results
summary(anosim_location)
plot(anosim_location)

# testing via adonis to see if similar
adonis_location = adonis(otus_dist ~ Location, metadata)

# post hoc analysis not automated yet it seems - do manual parwise comparisons 
V69_cols = grep('^69CV',names(otus)) 
DSPK_cols = grep('^DS',names(otus))
Skeet_cols = grep('^SP',names(otus))
KK_cols = grep('^KK',names(otus))
ThreeP_cols = grep('^3P',names(otus))
KB_cols = grep('^KB',names(otus))
EKH_cols = grep('^EKH',names(otus))


# Generate a rarefied OTU table from each site:
Culvert_69 <- t_otus_rarefied[CV69_cols,]
DS_Palikea <- t_otus_rarefied[DSPK_cols,]
Skeet_Pass <- t_otus_rarefied[Skeet_cols,]
Kaaikukai <- t_otus_rarefied[KK_cols,]
Three_Pts <- t_otus_rarefied[ThreeP_cols,]
KaalaBog <- t_otus_rarefied[KB_cols,]
Ekahanui <- t_otus_rarefied[EKH_cols,]

#group OTUs from each set of donor/recipient sites...these will be the three comparisons
skeet_kaala = rbind(Skeet_Pass,KaalaBog)
three_pts_cv69 = rbind(Three_Pts, Culvert_69)
palikea_sites = rbind(DS_Palikea, Kaaikukai, Ekahanui)

#remove "empty" OTUs that pop up since we are subsetting the large OTU table
skeet_kaala = skeet_kaala[,which(colSums(skeet_kaala) != 0)]
three_pts_cv69 = three_pts_cv69[,which(colSums(three_pts_cv69) != 0)]
palikea_sites = palikea_sites[,which(colSums(palikea_sites) != 0)]

#perform anosim on each meaningful combination
anosim(three_pts_cv69, metadata$Location)
anosim(skeet_kaala, metadata$Location)
anosim(palikea_sites, metadata$Location)


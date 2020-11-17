#### PCA workshop script ####

library(prcomp) # read in needed pacakges 

data=iris # read in your data

dat_mat=as.matrix(data[,-5]) #convert data to matrix format for the PCA package

#check for outliers
library(robustbase)
library(chemometrics)

outlier=Moutlier(dat_mat,plot=TRUE) # use the embeded malahnobins distance in Mountlier 

idx_out_md=which(outlier$md>outlier$cutoff) # remove outliers from classic mahalnobis dist
idx_out_rd=(outlier$rd>outlier$cutoff)# remove outliers from robust mahalnobis dist

# remove the outliers from the orginal data

dat_md=data[-idx_out_md,]
dat_rd=data[-idx_out_rd,]


# preform PCA for the data (note the input should be a numerical matrix ) - remeber to scale and center
PCs=prcomp(as.matrix(dat_md[,-5]),center=TRUE,scale. = TRUE)

# preform PCA yourself via SVD
n=dim(data)[1]
p=dim(data)[2]

X=scale(dat_md[,-5],center = TRUE,scale = TRUEU)

svdlist=svd(X, nu = min(n, p), nv = min(n, p))

# keep only modes correspoding to strictly positive singular values, use the scaled and normalized data!
d = svdlist$d[svdlist$d> 1e-9] # scaled egienvalues - only positive values 
U = svdlist$u[,1:length(d)] # scores normalized to 1
V = svdlist$v[,1:length(d)] # loadings 

# compute scores and loadings 
loading=V
score=X%*%loading

# plot the results in a biplot 
library(ggbiplot)
ggbiplot(PCs,groups =dat_md[,5],obs.scale = 1,var.scale = 1,ellipse = T )

# plot the scores, loadings and explained varaince seperate from base R
PCAcolors <- c("#66c2a5","#fc8d62","#8da0cb")[as.integer(data$Species)]

PCAscores <- PCs$x
PCAloadings <- PCs$rotation


par(mfrow=c(1,2))
plot(PCAscores[,1:2],  # x and y data
     pch=21,           # point shape
     col=PCAcolors,    # point border color
     bg=PCAcolors,     # point color
     cex=1.5,          # point size
     main="Scores"     # title of plot
)
legend("topright",                                # position of legend
       legend=levels(data$Species),                       # legend display
       pch=21,                                    # point shape
       pt.bg=c("#66c2a5","#fc8d62","#8da0cb"),    # point colors
       pt.cex=1.5,                                # point size
       col = c("#66c2a5","#fc8d62","#8da0cb")    # point border color
)
plot(PCAloadings[,1:2],   # x and y data
     pch=21,              # point shape
     bg="black",          # point color
     cex=1,               # point size
     main="Loadings"      # title of plot
)
text(PCAloadings[,1:2],             # sets position of labels
     labels=rownames(PCAloadings)   # print labels
)

# plot explained varinace 

plot(PCs) # explained variance
# explained variance normalized vs number of PCs
plot(1:length(PCs$sdev), 1-PCs$sdev/sum(PCs$sdev), 
    cex = 1,ylab = "(explained variance)/(total variance)",xlab = "n PC",type="l") 


# select optimal number of princple components 

# kaiser criterion for PC -selection
screeplot(PCs, type = "l", npcs = length(data[,-5]), main = "Screeplot of the first 4 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

cumpro <- cumsum(PCs$sdev^2 / sum(PCs$sdev^2))
plot(cumpro[0:4], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 2, col="blue", lty=5)
abline(h = cumpro[2], col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC4"),
       col=c("blue"), lty=5, cex=0.6)




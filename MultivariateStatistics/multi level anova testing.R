### ANOVA analysis of multi level data ####
dat= read_csv("C:/Users/mabso/Desktop/PhD/workshop 2020/data/alldata/cookies.txt")
dat$assessor=as.factor(dat$assessor)
dat$treatm=as.factor(dat$treatm)
dat$colour=as.factor(dat$colour)
dat$cons=as.factor(dat$cons)
dat$quality=as.factor(dat$quality)

av=aov(taste~.,data=dat)
summary(av)

HSD=TukeyHSD(av)
plot(HSD)


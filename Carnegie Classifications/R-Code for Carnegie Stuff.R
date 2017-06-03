#Carnegie Classifications 2010
#######
setwd("X:/PH_Desktop/Carnegie") #if on computer in OPA
setwd("F:/Carnegie Classifications") #on personal computer
setwd("C:/Users/Paul/Documents/Carnegie Classifications")


#
#2010
cc2010.full <- read.csv("CC2010data.csv", header = TRUE)
cc2010 <- cc2010.full[(cc2010.full$BASIC2010>14&cc2010.full$BASIC2010<18),]
cc2010$BASIC2010 <- factor(cc2010$BASIC2010)
#ranked
cc2010Ps<-na.omit(cc2010[,c("NAME","BASIC2010","FACFTTOT","HUM_D","PROF_D","SOC_D","STEM_D","RESSTAFF","STEM_EXP","NONSTEM")])
cc2010.r <- data.frame(cc2010Ps[,1:3], sapply(cc2010Ps[,-c(1:3)],minrank))
#percapita values

cc2010percap <- cc2010Ps[,c("RESSTAFF","STEM_EXP","NONSTEM")]/cc2010Ps$FACFTTOT
cc2010percap.r <- data.frame(sapply(cc2010percap,rank))

########2015################
cc2015.full <- read.csv("CC2015data.csv", header = TRUE, as.is = TRUE)
#updated file
#cc2015.full <- read.csv("Updated2015.csv", header = TRUE)

cc2015 <- cc2015.full[(cc2015.full$BASIC2015>14&cc2015.full$BASIC2015<18),]
cc2015$BASIC2015 <- factor(cc2015$BASIC2015)
#ranked

frank <- function(x){rank(x, ties.method = "first")}
lrank <- function(x){rank(x, ties.method = "last")}
minrank <- function(x){rank(x, ties.method = "min")}
maxrank <- function(x){rank(x, ties.method = "max")}

cc2015Ps<-
na.omit(cc2015[,c("NAME","BASIC2010","BASIC2015","FACNUM","HUM_RSD","OTHER_RSD","SOCSC_RSD","STEM_RSD","PDNFRSTAFF","S.ER.D","NONS.ER.D")])

#calculate the ranked data
cc2015.r <- data.frame(cc2015Ps[,1:3],sapply(cc2015Ps[,-c(1:3)],minrank)) 

cc2015percap <- cc2015Ps[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc2015Ps$FACNUM
cc2015percap.r<-data.frame(sapply(cc2015percap,minrank))


#beanplots for ranked, unranked data
library(beanplot)
beanplot(cc2015Ps[,5],cc2015Ps[,6],cc2015Ps[,7], cc2015Ps[,8], method = "jitter", col =3, yint = 0, data = cc2015Ps)
c(cc2015Ps$STEM_RSD,cc2015Ps$SOCSC_RSD,cc2015Ps$HUM_RSD,cc2015Ps$OTHER_RSD)



#################
##Data Analysis## 
#################


#replicate 2010:
#some code to remove Rockefeller Univerisity right quick
which(cc2010.r$NAME == "Rockefeller University") 
####

pc.2010.rank <- prcomp(cc2010.r[,-c(1:3)], scale = TRUE, center = TRUE)
summary(pc.2010.rank)
pc.2010.percap <- prcomp(cc2010percap.r[,], scale = TRUE, center = TRUE)

plot(-pc.2010.rank$x[,1],pc.2010.percap$x[,1],xlab = "Aggregate Ranking",ylab ="Per Capita Ranking", col = cc2010$BASIC2010, type = "n")
#plot text
text(-pc.2010.rank$x[,1],pc.2010.percap$x[,1],labels = cc2010Ps$NAME, col = as.numeric(cc2010Ps$BASIC2010), cex = .3)
#plot points instead
points(-pc.2010.rank$x[,1],pc.2010.percap$x[,1],col = as.numeric(cc2010.r$BASIC2010), pch =20)


points(-pc.2010.percap$x[28,1],pc.2010.rank$x[28,1], pch = 14, col = "blue") #identifies Montana State University
title("2010 Carnegie Classifications")
legend(x = 'bottomright', fill = c("green","red","black", "blue"),legend = c("High Research","Higher Research", "Highest Research", "Montana State"))



#replicate 2015:
pc.2015.rank <- prcomp(cc2015.r[,-c(1:4)], scale = FALSE)
summary(pc.2015.rank)

pc.2015.percap <- prcomp(cc2015percap.r, scale = TRUE)
summary(pc.2015.percap)

##Standardize the Scores:
loadings.ag <- pc.2015.rank$rot[,1]
sdev.ag <- pc.2015.rank$sdev[1]
round(loadings.ag *sdev.ag,3)

loadings.pc <- pc.2015.percap$rot[,1]
sdev.pc <- pc.2015.percap$sdev[1]
round(loadings.pc * sdev.pc,3)

score.ag <- scale(pc.2015.rank$x[,1], center = TRUE, scale = TRUE)
score.pc <- scale(pc.2015.percap$x[,1], center = TRUE, scale = TRUE)

##Plot the Scores
plot(score.ag, -score.pc,xlab = "Per Capita Ranking",ylab ="Aggregate Ranking",col = cc2015Ps$BASIC2015, type = "n",xlim = c(-1.2,-.8), ylim = c(-1.5,-.5))
#plot text
text(score.ag, -score.pc, labels = cc2015Ps$NAME, col = as.numeric(cc2015Ps$BASIC2015)+2, cex = .7)
#plot points instead
#points(score.ag,-score.pc,col = as.numeric(cc2015Ps$BASIC2015)+2, pch =20)
title("Carnegie Classifications: Nearest Neighbors")

#let's take a quick look at Alaska Fairbanks
dat <- rbind(cc2015Ps[c(a,6),], cc2015.r[c(a,6),])
xtable(dat[c(1,3,2,4),-c(2,3)])


#Where is MSU?
a <- which(cc2015Ps$NAME == "Montana State University")
id <- which(cc2015Ps$NAME == "University of Idaho")
points(score.ag[id,],-score.pc[id,],col = "black", pch = 20) #identifies Montana State University
title("2015 Carnegie Classifications")
legend(x = 'topleft', fill = c("green","red","black", "blue"),legend = c("High Research","Higher Research", "Highest Research", "Montana State"))


#identify a cohort around MSU
#identify(score.ag, -score.pc, plot = TRUE, n = 5) #this code does work though


##playing with psych packages' principal function
library(psych)
pr1 <- principal(cc2015.r[,-c(1:4)], scale = FALSE)
pr2 <- principal(cc2015percap, scale = FALSE)


##some work with trying to figure out what the hell is going on here with the data
#trying to replicate some of the totals that they had
length(which(cc2015$BASIC2015==15))
length(which(cc2015$BASIC2015==16))
length(which(cc2015$BASIC2015==17))
length(which(cc2015$BASIC2015 == 16 & cc2015$BASIC2010 == 15))
length(which(cc2015$BASIC2015 == 16 & cc2015$BASIC2010 == 16))
length(which(cc2015$BASIC2015 == 16 & cc2015$BASIC2010 == 17))
length(which(cc2015$BASIC2015 == 15 & cc2015$BASIC2010 == 15))
length(which(cc2015$BASIC2015 == 15 & cc2015$BASIC2010 == 17))

library(tibble); library(pander)

tibble(c(t1,t2,t3))
 

##highest research activity is OK
##getting different numbers for "higher research activity
##exactly replicating the 



##Some more on the pc's:
#scree plots
par(mfrow = c(1,2))
plot(seq(0,7, by = 1),c(1,1-summary(pc.2015.rank)$importance[3,]), type = "l", lwd = 2, ylab = "1-Variance Explained")
title("Scree Plot for Aggregate Ranks")
abline(v = 1, col = "red", lty = 2)
pct1 <- summary(pc.2015.rank)$importance
#first PC explains 71.07% of the variation in the underlying covariates
pct2 <- summary(pc.2015.percap)$importance
pct2
plot(seq(0,3, by = 1), c(1,1-pct2[3,]), type = "l", lwd = 2, main = "Scree Plot for PC Ranks", ylab = "1-Variance Explained")
abline(v = 1, col = "red", lty = 2)


#first pc for per cap explains roughly 70.51 percent of the variation in PC covariates

#lets get the eigenvalues out: I might need to check on how to interpret these
pc.2015.rank$sdev[1]^2
pc.2015.percap$sdev[1]^2


###Getting Into Un-tie-ing some of the underlying variables
##Key Idea: We want to take the raw data in cc2015Ps (doctoral granting, NA omitted data)
## and then we want to adjust each covariate by a little bit here or there. From there we can
## see how the point for MSU changes, and how close we might actually be to moving up or down
## in the classifications

cc2015Ps #unranked

#lets try adding one Humanities Doctorate
#cc2015Ps[a,"SOCSC_RSD"] <- cc2015Ps[a,"SOCSC_RSD"] + 2
#cc2015percap[a,]

#functionalize the rankings:
#function for aggregate

#NOTE THAT THIS FUNCTION HAS TO BE PASSED THE CC2015PS or it will break, probably!
#MAKE ADJUSTMENTS TO CC2015 and THEN YOU CAN PASS THROUGH THE FUNCTIONS....
AGcc <- function(x){
  #rank the data
  ranked <- data.frame(x[,1:3],sapply(x[,-c(1:3)],minrank)) 
  #get pc's
  pca.ranked <- prcomp(ranked[,-c(1:4)], scale = TRUE)
  summary <- summary(pca.ranked)
  standard.score <- scale(pca.ranked$x[,1], scale = TRUE, center = TRUE)
  #needs to return the standardized scores
  return(list(scorez = standard.score, sum =summary))
}
#function for percap
PCcc <- function(x){
  #rank the data
  ranked.dat <- data.frame(sapply(x,minrank)) 
  #get pc's
  pc.ranked <- prcomp(ranked.dat, scale = TRUE)
  summary <- summary(pc.ranked)
  standard.score <- scale(pc.ranked$x[,1], scale = TRUE, center = TRUE)
  return(list(scorez = standard.score, sum = summary))
}


#plot the two: more of a final plot
percap <- PCcc(cc2015percap)
ag <- AGcc(cc2015Ps)
plot(ag$scorez, -percap$scorez, col = cc2015Ps$BASIC2015, pch = 20, xlab = "Aggregate Index", ylab = "Per Capita Index", main = "Scaled Scores")
points(ag$scorez[a],-percap$scorez[a],col = "blue", pch = 20)

#correlation values
cor(ag$scorez, -percap$scorez)
cor(-pc.2010.rank$x[,1],pc.2010.percap$x[,1])



#let's see if I can at least replicate the lines


mean.percap <- 340.52
sd.percap <- 170.51
mean.ag <- 780.74
sd.ag <- 413.10

rawscores.percap <- sd.percap * -percap$scorez + mean.percap
rawscores.ag <- sd.ag * ag$scorez + mean.ag

plot(rawscores.ag, rawscores.percap, pch = 20, col = cc2015Ps$BASIC2015, asp = 1, xlab = "Aggregate Index", ylab = "Per Capita Index")
title("2015 Carnegie Classifications")
points(rawscores.ag[-a],rawscores.percap[-a],col = cc2015Ps$BASIC2015[-a], pch = 20, cex = 1)
icon1 <- readPNG('Montana State Logo.png')
rasterImage(icon1,xleft=rawscores.ag[a]-30,xright =rawscores.ag[a]+30,  ybottom =rawscores.percap[a]-20, ytop =rawscores.percap[a]+20 ,angle = 0, interpolate = TRUE)
#plots the point of interest
funk <- function(r,x){sqrt(r^2 - x^2)}
x <- seq(500,984,by =1)
r <- 984.007
lines(x,funk(r,x),lty = 1, col = "gray60")
x <- seq(0,409,by =1)
r<- 409.461
lines(x,funk(r,x), lty = 1, col = "gray60")
#text(730,535, "MSU", col = "blue3")

legend('bottomright', fill = c(1,2,3), legend = c("R-1","R-2","R-3"))



#calculate the distance from the closest location to the line
library(spatstat)
abline(h = rawscores.percap[a])
r <- 984.007
crossdist(c(rawscores.ag[a], rawscores.percap[a]), funk(r,x))

y <- funk(984.007, 500:1000)
x <- 500:1000

euclidDist <- function(x1 = x1, y1 = y1, x2 = x2, y2 = y2){
  distance <- sqrt((x2-x1)^2 + (y2-y1)^2)
  return(list(dist = distance, mindist = min(distance),xnew = x2,ynew = y2))
}

dist1 <- euclidDist(x1=x,y1 = y, x2 = rawscores.ag[a], y2 = rawscores.percap[a])

#choose a variable, perturb it
upper <- 70
mindist <- new.y <- new.x <- rep(0,upper)
num.sosc <- rep(0, upper)
dist.matrix <- matrix(ncol = upper, nrow = 485)

new.ccPs <- cc2015Ps
new.ccPercap <- cc2015percap

for (j in 1:upper){
  #perturb value for just Montana State
  new.ccPs[a,"SOCSC_RSD"] <- new.ccPs[a,"SOCSC_RSD"] + 1
  new.ccPercap[a,] <- new.ccPercap[a,]
  
  #re-runs everything
  #plot the two: more of a final plot
  percap <- PCcc(new.ccPercap)
  ag <- AGcc(new.ccPs)
  
  #unstandardize stuff
  rawscores.percap <- sd.percap * -percap$scorez + mean.percap
  rawscores.ag <- sd.ag * ag$scorez + mean.ag
  
  #calculate distances to r1
  y <- funk(984.007, 500:984)
  x <- 500:984
  new.dist <- euclidDist(x1=x,y1 = y, x2 = rawscores.ag[a], y2 =rawscores.percap[a])
  mindist[j] <- new.dist$mindist
  dist.matrix[,j] <- new.dist$dist
  new.y[j] <- new.dist$ynew
  new.x[j] <- new.dist$xnew
  num.sosc[j] <- cc2015Ps[a,"SOCSC_RSD"] + j
}
par(mfrow = c(1,1))
plot(seq(1:j), mindist, main = "Distance to R1: Social Sciences", xlab = "Additional PhDs", type = "l", ylab = "Distance to R-1", col = "tomato2")
abline(v = which.min(mindist), col = "blue")


### other comparisons
##now re-calculate the per-capita scores, with additional staff added in
percap <- cc2015Ps[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc2015Ps$FACNUM

base.percap <- PCcc(old.dat[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc2015Ps$FACNUM)
base.ag <- AGcc(old.dat)

percap <- PCcc(percap)
ag <- AGcc(cc2015Ps)

mean.percap <- 340.52
sd.percap <- 170.51
mean.ag <- 780.74
sd.ag <- 413.10
rawscores.percap <- sd.percap * -percap$scorez + mean.percap
rawscores.ag <- sd.ag * ag$scorez + mean.ag

rawscores.base.pc <- sd.percap * -base.percap$scorez + mean.percap
rawscores.base.ag <- sd.ag * base.ag$scorez + mean.ag

plot(rawscores.base.ag, rawscores.base.pc, pch = 20, col = "gray74", asp = '1', xlab = "Aggregate Index", ylab = "Per Capita Index", cex.lab = 1.3)
points(rawscores.ag, rawscores.percap, pch = 20, col = as.numeric(cc2015Ps$BASIC2015))
title("2015 Carnegie Classifications", cex.main = 1.5)
#points(rawscores.ag[a],rawscores.percap[a],col = "gold2", pch = 15, cex = 1.6)
#include a Montana State Logo with a blue center to plot Montana State's Values
library(png)
icon1 <- readPNG('Montana State Logo.png')
rasterImage(icon1,xleft=rawscores.ag[a]-30,xright =rawscores.ag[a]+30,  ybottom =rawscores.percap[a]-20, ytop =rawscores.percap[a]+20 ,angle = 0, interpolate = TRUE)
#plots the point of interest


#plot the lines
funk <- function(r,x){sqrt(r^2 - x^2)}
x <- seq(500,984,by =1)
r <- 984.007
lines(x,funk(r,x), col = "gray50")
x <- c(seq(1,409,by = 1),409.461)
r<- 409.461

lines(x,funk(r,x))
legend('bottomright', fill = c(1,2,3,"gray74"), legend = c("R-1","R-2","R-3","Original Location"), bty = "n")



###some comparisons
#PHD Counts

r1 <- apply(cc2015Ps[,c(4:11)],2,summary)
library(xtable)

c <- cc2015Ps[a,4:11]


#the table gives the variable values as well as the average INDEX SCORE for the R-1 schools etc. 
sicktable <- data.frame(rbind(data.frame(c), as.data.frame(r1)))[c(1,2,3,4,5),]
scorestable <- rbind(c(1200.4, 488.7), c(650.3, 490.6))
rownames(sicktable) <- c("Montana State","Min","1st Quartile R1","Median R1","Mean R1")
colnames(sicktable) <- c("Faculty", "Hum PhD","Other PhD","Soc PhD","STEM PhD","Res Staff", "Stem Exp","Non-Stem Exp" )

rownames(scorestable) <- c("Montana State", "Average R-1")
colnames(scorestable) <- c("Aggregate Index","Per Capita Index")

xtable(sicktable) #generates the necessary latex code to get this thing
xtable(scorestable) #generates latex table for scores table

##MATPLOtS

#Final plot with thoughts about PCP of ranks:
scale01 <- function(vec){
  vec1 = vec[!is.na(vec)]
  (vec-min(vec1))/(max(vec1)-min(vec1))
}

cc2015Ps$BASIC2015 <- as.numeric(as.character(cc2015Ps$BASIC2015))

cc2015Ps_scale <- sapply(cc2015Ps[,-c(1,2)],scale01) #Also scales group labels to go from 0 to 1

summary(cc2015Ps_scale)



matplot(t(cc2015Ps_scale[which(cc2015Ps$BASIC2015==15),3:ncol(cc2015Ps_scale)]), lwd = 2, type = "l", yaxt = "n", xaxt = "n", ylab = "Scaled Responses",col = "gray60", ylim = c(-.01,1.2), lty = 2)

tics = 1:8
xlabs = c("Hum_D", "Other_D", "Soc_D", "Stem_D", "Res Staff", "StemExp", "N.S.Exp")
ylabs = c("min", "max")
tck <- axis(1, at = tics, labels = FALSE)
tck2 <- axis(2, at = tics2, labels = FALSE)
text(tck, par("usr")[3], labels = xlabs, srt = 315, xpd = TRUE, adj = c(-.2, 1.2, cex = 0.9))
text(par("usr")[1], tck2, labels = ylabs, srt = 315, xpd = TRUE, adj = c(2, .5, cex = 0.9))


tufts <- which(cc2015Ps$NAME == "Tufts University")
beavers <- which(cc2015Ps$NAME == "Oregon State University")

segments(x0 = 1:6, y0 = cc2015Ps_scale[tufts,3:ncol(cc2015Ps_scale)], x1 = 2:7,
         y1 = cc2015Ps_scale[tufts,4:ncol(cc2015Ps_scale)], lwd = 3,lty=1,col="gold1")

segments(x0 = 1:6, y0 = cc2015Ps_scale[beavers,3:ncol(cc2015Ps_scale)], x1 = 2:7,
         y1 = cc2015Ps_scale[beavers,4:ncol(cc2015Ps_scale)], lwd = 3,lty=1,col="orangered")



medianzzz <- apply(cc2015Ps_scale[,3:ncol(cc2015Ps_scale)], 2, median)

#segments(x0 = 1:7, y0 = medianzzz[1:8], x1 = 2:8,
         #y1 = medianzzz[2:9], lwd = 2,lty=1,col="red")
title("Montana State vs. R-1 Schools")

##add Montana State
segments(x0 = 1:6, y0 = cc2015Ps_scale[a,3:ncol(cc2015Ps_scale)], x1 = 2:7,
         y1 = cc2015Ps_scale[a,4:ncol(cc2015Ps_scale)], lwd = 3,lty=1,col="blue4")

legend('topleft', xjust = 10, inset = c(0,-.02),bty = 'n', fill = c('blue4',"gold3","orangered",'gray50'), legend = c("Montana State","Tufts University", "Oregon State","R1"), horiz = TRUE)



##Curious as to where everything would be if we averaged over all the years
#setwd("C:/Users/Paul/Downloads")
cc5 <- read.csv("cc2005dat.csv")
cc10 <- read.csv("cc2010dat.csv")
cc5d <- cc5[which(cc5$BASIC2005==15|cc5$BASIC2005==16|cc5$BASIC2005==17),]
cc10d <- cc5[which(cc10$BASIC2005==15|cc10$BASIC2005==16|cc10$BASIC2005==17),]



##model based clustering
library(mclust)


rawscores.percap <- sd.percap * -percap$scorez + mean.percap
rawscores.ag <- sd.ag * ag$scorez + mean.ag

#do some clustering on the rawscores
library(tibble)
X <- cbind(cc2015Ps$NAME,rawscores.percap[1:276], rawscores.ag[1:276])


#First Cluster: Let the Data Speak a Bit
clust1 <- Mclust(X[,2:3])
plot(rawscores.ag, rawscores.percap, col = clust1$classification, pch = 20, xlab = "Aggregate Index", ylab = "Per-Capita Index")
title("Model-Based Clustering with 4 Clusters")
rasterImage(icon1,xleft=as.numeric(X[a,3])-30,xright =as.numeric(X[a,3])+30,  ybottom =as.numeric(X[a,2])-20, ytop = as.numeric(X[a,2])+20 ,angle = 0, interpolate = TRUE)
funk <- function(r,x){sqrt(r^2 - x^2)}
x <- seq(500,984,by =1)
r <- 984.007
lines(x,funk(r,x),lty = 1, col = "gray60")
x <- seq(0,409,by =1)
r<- 409.461
lines(x,funk(r,x), lty = 1, col = "gray60")
points(rawscores.ag[-a], rawscores.percap[-a], col = clust1$classification[-a], pch = 20)

xtable(table(clust1$classification, cc2015Ps$BASIC2015))
#misclassification rate
(12 + 31)/276



#Second Clustering Method
clust2 <- Mclust(X[,2:3], G = 3)
plot(X[,3],X[,2], col = clust2$classification, pch = 20, xlab = "Aggregate Index", ylab = "Per-Capita Index")
title("Model-Based Clustering with 3 Clusters")
rasterImage(icon1,xleft=as.numeric(X[a,3])-30,xright =as.numeric(X[a,3])+30,  ybottom =as.numeric(X[a,2])-20, ytop = as.numeric(X[a,2])+20 ,angle = 0, interpolate = TRUE)
points(X[-a,3],X[-a,2], col = clust2$classification[-a], pch = 20)
funk <- function(r,x){sqrt(r^2 - x^2)}
x <- seq(500,984,by =1)
r <- 984.007
lines(x,funk(r,x),lty = 1, col = "gray60")
x <- seq(0,409,by =1)
r<- 409.461
lines(x,funk(r,x), lty = 1, col = "gray60")
xtable(table(clust2$classification, cc2015Ps$BASIC2015))
#mc rate
(27 + 39)/276


#kmeans might be better
#code below generates the median values of each 
med15 <- c(median(rawscores.ag[which(cc2015Ps$BASIC2015 == 15)]), median(rawscores.percap[which(cc2015Ps$BASIC2015== 15)]))
med16 <- c(median(rawscores.ag[which(cc2015Ps$BASIC2015 == 16)]), median(rawscores.percap[which(cc2015Ps$BASIC2015== 16)]))
med17 <- c(median(rawscores.ag[which(cc2015Ps$BASIC2015 == 17)]), median(rawscores.percap[which(cc2015Ps$BASIC2015== 17)]))


km1 <- kmeans(X[,2:3], centers = 3,iter.max =10)
plot(X[,3],X[,2], col = km1$cluster, pch = 20, main = "K-Means Solution with 3 Clusters", xlab = "Aggregate Index", ylab = "Per Capita Index")
points(X[a,3],X[a,2], col = "blue3",cex =1.2, pch =20)
rasterImage(icon1,xleft=as.numeric(X[a,3])-30,xright =as.numeric(X[a,3])+30,  ybottom =as.numeric(X[a,2])-20, ytop = as.numeric(X[a,2])+20 ,angle = 0, interpolate = TRUE)
points(X[-a,3],X[-a,2], col = km1$cluster[-a], pch = 20)
#text(as.numeric(X[,3]),as.numeric(X[,2]),labels = km1$cluster)
funk <- function(r,x){sqrt(r^2 - x^2)}
x <- seq(500,984,by =1)
r <- 984.007
lines(x,funk(r,x),lty = 1, col = "gray60")
x <- seq(0,409,by =1)
r<- 409.461
lines(x,funk(r,x), lty = 1, col = "gray60")
xtable(table(km1$cluster, cc2015Ps$BASIC2015))
(36 + 47)/(276)




#partition around medoids (PAMclust)
#I'm going to try and partition around the middle values for each 
#This seems to be doing some pretty weird stuff


library(cluster)
pam1 <- pam(as.numeric(X[,2:3]), diss = FALSE, metric = "euclidean", k = 4)
plot(X[,3],X[,2], col = pam1$id.med, pch = 20)
plot(pam1)


plot(rawscores.ag, rawscores.percap, pch = 20, col = cc2015Ps$BASIC2015, asp = 1, xlab = "Aggregate Index", ylab = "Per Capita Index")
title("2015 Carnegie Classifications")
points(rawscores.ag[-a],rawscores.percap[-a],col = cc2015Ps$BASIC2015[-a], pch = 20, cex = 1)
icon1 <- readPNG('Montana State Logo.png')
rasterImage(icon1,xleft=rawscores.ag[a]-30,xright =rawscores.ag[a]+30,  ybottom =rawscores.percap[a]-20, ytop =rawscores.percap[a]+20 ,angle = 0, interpolate = TRUE)
#plots the point of interest
funk <- function(r,x){sqrt(r^2 - x^2)}
x <- seq(500,984,by =1)
r <- 984.007
lines(x,funk(r,x),lty = 1, col = "gray60")
x <- seq(0,409,by =1)
r<- 409.461
lines(x,funk(r,x), lty = 1, col = "gray60")
#text(730,535, "MSU", col = "blue3")

legend('bottomright', fill = c(1,2,3), legend = c("R-1","R-2","R-3"))



#data frame describing dataset
means <- apply(cc2015Ps[-c(1,2,3)],2,mean); sd <- apply(cc2015Ps[-c(1,2,3)],2,sd)
bigskools <- cc2015Ps$NAME[apply(cc2015Ps[-c(1,2,3)],2,which.max)]
table <- data.frame(names(cc2015Ps[-c(1,2,3)]), round(means,1),round(sd,1), bigskools)

colnames(table) <- c("Variable","Mean","SD","Name of Biggest Schools")
xtable(table)


#fun with faceplots
library(aplpack)
nearneigh <- c(a,6,146,177,165,34,24,168,187,147,118,204,99,238,207,246)
unames <- cc2015Ps$NAME[nearneigh]
unames[c(9,10)]<- c("North Dakota State U.","U of New Hampshire")
faces(xy = cc2015Ps[nearneigh,-c(1,2,3)], labels = unames, face.type = 1, fill = TRUE)
title("Chernov Face Plots for University Comparison")






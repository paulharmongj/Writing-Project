#crazy for loop I'm going to write 
library(tibble)
library(pander)
#ranked

frank <- function(x){rank(x, ties.method = "first")}
lrank <- function(x){rank(x, ties.method = "last")}
minrank <- function(x){rank(x, ties.method = "min")}
maxrank <- function(x){rank(x, ties.method = "max")}

cc2015Ps<-
  na.omit(cc2015[,c("NAME","BASIC2010","BASIC2015","FACNUM","HUM_RSD","OTHER_RSD","SOCSC_RSD","STEM_RSD","PDNFRSTAFF","S.ER.D","NONS.ER.D")])
cc2015percap <- cc2015Ps[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/cc2015Ps$FACNUM

#initialize some stuff
loadings.mat <- matrix(0,nrow = 7,ncol = 5)
loadingspc.mat <- matrix(0,nrow = 3,ncol = 5)
funk <- c(rank, frank,lrank,minrank,maxrank)


funky.names <- c("average","first","last","min","max")
function.vec <- c(rank, frank, lrank, minrank, maxrank)

par(mfrow = c(3,2))
for (j in 1:length(function.vec)){
 #call the correct function
  funk <- function.vec[[j]]
 #creates different ranked dataframes 
  cc2015.r <- data.frame(cc2015Ps[,1:3],sapply(cc2015Ps[,-c(1:3)],funk)) 
  cc2015percap.r<-data.frame(sapply(cc2015percap, funk))
                             
 #calculate the PCA for aggregate ranks
  pc.2015.rank <- prcomp(cc2015.r[,-c(1:4)], scale = TRUE)
  summary(pc.2015.rank)
 #calculate the PCA for per-capita ranks
  pc.2015.percap <- prcomp(cc2015percap.r, scale = TRUE)
  summary(pc.2015.percap)
  
 ##Standardize the Scores:
  loadings.ag <- pc.2015.rank$rot[,1]
  sdev.ag <- pc.2015.rank$sdev[1]
  loadings.mat[,j] <- round(loadings.ag *sdev.ag,3)
  
  loadings.pc <- pc.2015.percap$rot[,1]
  sdev.pc <- pc.2015.percap$sdev[1]
  loadingspc.mat[,j] <- round(loadings.pc * sdev.pc,3)
  
  #create a plot
  score.ag <- scale(pc.2015.rank$x[,1], center = TRUE, scale = TRUE)
  score.pc <- scale(pc.2015.percap$x[,1], center = TRUE, scale = TRUE)
  
  ##Plot the Scores
  plot(score.ag, -score.pc,xlab = "Per Capita Ranking",ylab ="Aggregate Ranking",col = cc2015Ps$BASIC2015, type = "n", asp = .15)
  #plot points instead
  points(score.ag,-score.pc,col = as.numeric(cc2015Ps$BASIC2015), pch =16)
  title(c("Ranking Method: ", funky.names[j]))

}
#generate a table of loadings
PC.load <- data.frame(loadingspc.mat)
AG.load <- data.frame(loadings.mat)
colnames(PC.load) <- colnames(AG.load) <- funky.names
rownames(PC.load) <- c("PDNFRSTAFF","S.ER.D","NONS.ER.D")
rownames(AG.load) <- c("HUM_RSD","OTHER_RSD","SOCSC_RSD","STEM_RSD", "PDNFRSTAFF","S.ER.D","NONS.ER.D")
pander(PC.load)
pander(AG.load)

#if I compile to word, I can combine the tables in a rough draft. Otherwise I can include as two separate tables









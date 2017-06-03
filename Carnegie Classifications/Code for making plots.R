#####Code for Making Plots of R-1:
#First I'm going to look at the aggregate ones, then the per capita ones.

par(mfrow = c(2,2))
##SOSC_RSD
#choose a variable, perturb it
variable.new <- "SOCSC_RSD"
#variable.new <- "STEM_RSD"
#variable.new <- "HUM_RSD"
#variable.new <- "OTHER_RSD"

###Start code

#choose a variable, perturb it
upper <- 100
mindist <- new.y <- new.x <- rep(0,upper)
num.sosc <- rep(0, upper)
dist.matrix <- matrix(ncol = upper, nrow = 485)

new.ccPs <- cc2015Ps
new.ccPercap <- cc2015percap

for (j in 1:upper){
  #perturb value for just Montana State
  new.ccPs[a,variable.new] <- new.ccPs[a,variable.new] + 1
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
  num.sosc[j] <- cc2015Ps[a,variable.new] + j
}



plot(seq(1:j), mindist, main = "Distance to R1: Social Sciences", xlab = "Additional PhDs", type = "l", ylab = "Distance to R-1", col = "tomato2", ylim = c(0,180))
abline(v = which.min(mindist), col = "blue")
#text(88,120, "58 Additional PhDs")
##############################################################################33

#STEM RSD
variable.new <- "STEM_RSD"

###Start code
#choose a variable, perturb it
upper <- 650
mindist <- new.y <- new.x <- rep(0,upper)
num.stem <- rep(0, upper)
dist.matrix <- matrix(ncol = upper, nrow = 485)

new.ccPs <- cc2015Ps
new.ccPercap <- cc2015percap

for (j in 1:upper){
  #perturb value for just Montana State
  new.ccPs[a,variable.new] <- new.ccPs[a,variable.new] + 1
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
  num.stem[j] <- cc2015Ps[a,variable.new] + j
}



plot(seq(1:j), mindist, main = "Distance to R1: STEM", xlab = "Additional PhDs", type = "l", ylab = "Distance to R-1", col = "tomato2", ylim = c(0,180))


########################

variable.new <- "HUM_RSD"

###Start code
#choose a variable, perturb it
upper <- 250
mindist <- new.y <- new.x <- rep(0,upper)
num.hum <- rep(0, upper)
dist.matrix <- matrix(ncol = upper, nrow = 485)

new.ccPs <- cc2015Ps
new.ccPercap <- cc2015percap

for (j in 1:upper){
  #perturb value for just Montana State
  new.ccPs[a,variable.new] <- new.ccPs[a,variable.new] + 1
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
  num.hum[j] <- cc2015Ps[a,variable.new] + j
}


plot(seq(1:j), mindist, main = "Distance to R1: Humanities", xlab = "Additional PhDs", type = "l", ylab = "Distance to R-1", col = "tomato2", ylim = c(0,180))


#################################3

variable.new <- "OTHER_RSD"

###Start code
#choose a variable, perturb it
upper <- 300
mindist <- new.y <- new.x <- rep(0,upper)
num.other <- rep(0, upper)
dist.matrix <- matrix(ncol = upper, nrow = 485)

new.ccPs <- cc2015Ps
new.ccPercap <- cc2015percap

for (j in 1:upper){
  #perturb value for just Montana State
  new.ccPs[a,variable.new] <- new.ccPs[a,variable.new] + 1
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
  num.other[j] <- cc2015Ps[a,variable.new] + j
}


plot(seq(1:j), mindist, main = "Distance to R1: Other PhDs", xlab = "Additional PhDs", type = "l", ylab = "Distance to R-1", col = "tomato2", ylim = c(0,180))



#####Code for Making Plots of R-1: Per Capita and Aggregate Variables #########################################
#This looks at research staff, serd, and nonserd which are used in both indices.
par(mfrow = c(2,2))
#choose a variable, perturb it
variable.new <- "PDNFRSTAFF"

###Start code
#choose a variable, perturb it
upper <- 5000
mindist <- new.y <- new.x <- rep(0,upper)
num.staff <- rep(0, upper)
dist.matrix <- matrix(ncol = upper, nrow = 485)

new.ccPs <- cc2015Ps
new.ccPercap <- cc2015percap

for (j in 1:upper){
  #perturb value for just Montana State
  new.ccPs[a,variable.new] <- new.ccPs[a,variable.new] + 1
  new.cc.percap <- new.ccPs[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/new.ccPs$FACNUM #reflects changes in per cap values 
  
  
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
  num.staff[j] <- cc2015Ps[a,variable.new] + j
}


plot(seq(1:j), mindist, main = "Distance to R1: Research Staff", xlab = "Additional Staff", type = "l", ylab = "Distance to R-1", col = "tomato2", ylim = c(0,175))


##SERD###############################
variable.new <- "S.ER.D"

###Start code
#choose a variable, perturb it
upper <- 2200
mindist <- new.y <- new.x <- rep(0,upper)
num.serd <- rep(0, upper)
dist.matrix <- matrix(ncol = upper, nrow = 485)

new.ccPs <- cc2015Ps
new.ccPercap <- cc2015percap
increase <- seq(from = 0, to = 222800, by = 1000)

for (j in 1:upper){
  #perturb value for just Montana State
  new.ccPs[a,variable.new] <- new.ccPs[a,variable.new] + 1000
  new.cc.percap <- new.ccPs[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/new.ccPs$FACNUM #reflects changes in per cap values 
  
  
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
  num.serd[j] <- cc2015Ps[a,variable.new] + 1000*j
}



plot(seq(1:j), mindist, main = "Distance to R1: Stem Expenditures", xlab = "Additional Expenditures (Thousands of Dollars)", type = "l", ylab = "Distance to R-1", col = "tomato2", ylim = c(0,175))





####NONSERD#############################################
variable.new <- "NONS.ER.D"

###Start code
#choose a variable, perturb it
upper <- 150
mindist <- new.y <- new.x <- rep(0,upper)
num.nonserd <- rep(0, upper)
dist.matrix <- matrix(ncol = upper, nrow = 485)

new.ccPs <- cc2015Ps
new.ccPercap <- cc2015percap

for (j in 1:upper){
  #perturb value for just Montana State
  new.ccPs[a,variable.new] <- new.ccPs[a,variable.new] + 1000
  new.cc.percap <- new.ccPs[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/new.ccPs$FACNUM #reflects changes in per cap values 
  
  
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
  num.nonserd[j] <- cc2015Ps[a,variable.new] + j*1000
}


plot(seq(1:j), mindist, main = "Distance to R1: Non-Stem Expenditures", xlab = "Additional Expenditures (Thousands of Dollars)", type = "l", ylab = "Distance to R-1", col = "tomato2", ylim = c(0,175))


###decrease faculty size: By reducing Faculty Size the per-capita counts should increase. Not sure what's happening here.

variable.new <- "FACNUM"
###Start code
#choose a variable, perturb it
upper <- 1000
mindist <- new.y <- new.x <- rep(0,upper)
num.fac <- rep(0, upper)
dist.matrix <- matrix(ncol = upper, nrow = 485)

new.ccPs <- cc2015Ps
new.ccPercap <- cc2015percap

for (j in 1:upper){
  #perturb value for just Montana State
  new.ccPs[a,variable.new] <- new.ccPs[a,variable.new] - 1
  new.cc.percap <- new.ccPs[,c("PDNFRSTAFF","S.ER.D","NONS.ER.D")]/new.ccPs$FACNUM #reflects changes in per cap values 
  
  
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
  num.fac[j] <- cc2015Ps[a,variable.new] + j
}


plot(seq(1:j), mindist, main = "Distance to R1: Tenured/Tenurable Faculty", xlab = "Fewer Faculty", type = "l", ylab = "Distance to R-1", col = "tomato2", ylim = c(0,175))











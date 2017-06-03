##Introductory R Code
##Paul Harmon
##February 5, 2017

##A note on 537 Homework Assignments
##HWK 3, Hwk 5 appear to be focused on Carnegie Classifications
###Check for STAT 537 Homework Keys



###Read in Data
cc.total <- read.csv("Raw Carnegie Data.csv", header = TRUE)
#15,16,17 are the doctoral universities
cc <- cc.total[which(cc.total$BASIC2015 == 15 | cc.total$BASIC2015 == 16 | cc.total$BASIC2015 == 17),]


###Identify Montana State
msu <- which(cc$NAME == "Montana State University")
cc[msu,] #identifies MSU-Bozeman


#Make a Data Frame:
cc.1 <- cc[,c(29:36)]


#ranked data frame
cc.rank <- data.frame(apply(cc.1, 2, rank)) #gives a ranked matrix of covariates of interest



#Principal Components
#percapita values
SERDperFAC <- cc.rank$S.ER.D/cc.rank$FACNUM
N.SERDperFAC <- cc.rank$NONS.ER.D/cc.rank$FACNUM
PDperFAC <- cc.rank$PDNFRSTAFF/cc.rank$FACNUM

pc.1 <- prcomp(~STEM_RSD + HUM_RSD + SOCSC_RSD + OTHER_RSD + NONS.ER.D + PDNFRSTAFF + SERDperFAC + N.SERDperFAC + PDperFAC , data = cc.rank)
summary(pc.1)
scores <- pc.1$x
pc.1$rot
plot(scores[,1], scores[,2], pch = 20, col = cc$BASIC2015, main = "First Two PCs")






###2010 Rankings








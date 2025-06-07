
## This source code is licensed under a CC-BY4.0 license (see https://creativecommons.org/licenses/by/4.0/)
## Here is some code showing how we would prepare the data for the analysis in R or Mplus; it is optional in the sense that you can use your own code to prepare the data

library(plyr) 

## Import data

dat <- read.table("...",header=T)
head(dat)

## This function is used to group-mean center the Level-1 predictors,
## to compute the interaction and the quadratic terms and to save the 
## group means for later analysis

Center <- function(dfs) {

   # center x and y on grand mean across both variables
   grand.M <- mean( c(dfs$x, dfs$y), na.rm = TRUE )
   dfs$x.c <- dfs$x-grand.M
   dfs$y.c <- dfs$y-grand.M
   
   # compute higher order terms based on the centered predictors
   dfs$x2.c <- dfs$x.c^2
   dfs$xy.c <- dfs$x.c*dfs$y.c
   dfs$y2.c <- dfs$y.c^2
   
   # save means and compute their higher order terms
   dfs$x.mean  <- mean( dfs$x, na.rm = TRUE )
   dfs$y.mean  <- mean( dfs$y, na.rm = TRUE )
   dfs$x2.mean <- dfs$x.mean^2
   dfs$xy.mean <- dfs$x.mean*dfs$y.mean  
   dfs$y2.mean <- dfs$y.mean^2
   
   dfs

}

## We use the function to prepare the data; the Level-2 indicator is
## "GroupID"

dat <- ddply(dat,.var="GroupID",.fun=Center)
summary(dat)

## Standardize the Level-2 predictor
## in the first two lines we first extrat one value of the Level-2 predictor
## of each Level-2 unit and then compute the average and the standard deviation

g.mean  <- mean(ddply(dat[,c("GroupID","g")],.var="GroupID",.fun=function(x) { x$g[1] })$V1)
g.sd    <- sd(ddply(dat[,c("GroupID","g")],.var="GroupID",.fun=function(x) { x$g[1] })$V1)
dat$g.s <- (dat$g - g.mean)/g.sd

## Save the data for lme4 or Mplus

write.table(dat,"Fakedata_lme4.dat",row.names=F,col.names=T)  # delete row names
write.table(dat,"Fakedata_Mplus.dat",row.names=F,col.names=F) # delete row and col names
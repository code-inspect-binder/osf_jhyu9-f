## This source code is licensed under a CC-BY4.0 license (see https://creativecommons.org/licenses/by/4.0/)
## by Steffen Nestler (2018). When you find a bug, please write a mail to steffen.nestler@uni-leipzig.de.

## Here, we show how to analyze the data with lme4
# if not installed yet: install the necessary packages:
# install.packages(c("lme4", "lmerTest", "RSA", "gridExtra"))

library(lme4)
library(lmerTest)
library(RSA)
library(gridExtra)

source("MultilevelRSA.r") # the R file with the functions

## Import data

dat <- read.table("Fakedata_lme4.dat",header=T)
head(dat)

## ++++++++++++++++++++++++++++++++++++
##      Average response surface
## ++++++++++++++++++++++++++++++++++++

## Estimate the lme4-model (without the covariate g)
## Note that we use the ML-estimator here as this is the estimator in Mplus
## In your own application, you can change the code to REML=TRUE

## Edit 25-08-2019: some month ago the lme4-team made changes to the default 
##                  optimizer which results in convergence warnings when one 
##                  uses more recent versions of lme4
##                  you can switch to the old optimizer by adding
##                  control=lmerControl(optimizer="bobyqa")

m1 <- lmer(z~x.c+y.c+x2.c+xy.c+y2.c+x.mean+y.mean+x2.mean+xy.mean+y2.mean+
          (1+x.c+y.c+x2.c+xy.c+y2.c|GroupID),data=dat,REML=F,
           control=lmerControl(optimizer="bobyqa"))
summary(m1)

# Some results of this model are reported in the note to Table 7.

## Compute the average response surface with the MLRSA_AverageSurface-function:
## MLRSA_AverageSurface needs the lme4-model,  
## the names of the variables (= name_vars) in the following order: 
## 1. the x - variable (here: x.c)
## 2. the y - variable (here: y.c)
## 3. the square of x (here: x.c2)
## 4. the interaction of x and y (here: xy.c)
## 5. the square of y (here: y.c2)
## and the names of the variables that were set to random in the lmer-object
## the order in random_vars should be the same as the order in name_vars

## NOTE: when one of the variables was not set to random, then specify NA;
##       for example, when you had not set y.c to random, then specify
##       random_vars=c("x.c",NA,"x.c2","xy.c","y.c2")

MLRSA_AverageSurface(m1,name_vars=c("x.c","y.c","x2.c","xy.c","y2.c"),random_vars=c("x.c","y.c","x2.c","xy.c","y2.c")) 

# These results are shown in Table 7.

## Plot the average surface using MLRSA_AverageSurfacePlot:
## The function needs the lme4-model, the names of the variable
## and the data; there are some defaults in this function (see the function code)

## NOTE: MLRSA_AverageSurfacePlot is a wrapper for the plotRSA-function 
##       contained in the RSA package

MLRSA_AverageSurfacePlot(m1,name_vars=c("x.c","y.c","x2.c","xy.c","y2.c"),
                         outcome="z",data=dat,
                         xlab="Competence",ylab="Demands",zlab="Satisfaction")
                         
## This function was not used to make the plot in Figure 2A (we used the plot 
## function MLRSA_PickaPointPlot below instead.) 

## +++++++++++++++++++++++++++++++++++++++++
##    Moderation of the congruence effect
## +++++++++++++++++++++++++++++++++++++++++

## Estimate the lme4-model (including the covariate g)
## Note that we use the ML-estimator here as this is the estimator in Mplus.
## In your own research, you can change the code to REML=TRUE.

## Edit 25-08-2019: see above for the control-parm change.

m2 <- lmer(z~g.s*x.c+g.s*y.c+g.s*x2.c+g.s*xy.c+g.s*y2.c+x.mean+y.mean+x2.mean+xy.mean+y2.mean+
           (1+x.c+y.c+x2.c+xy.c+y2.c|GroupID),data=dat,REML=F,
           control=lmerControl(optimizer="bobyqa"))
summary(m2)

## Compute the conditional response surface parameters with the MLRSA_PickaPoint-function
## The function needs the lme4-model,  
## the names of the RSA variables (= name_vars; see above for explanations),
## the name of the Level 2 predictor and the value of the Level 2 predictor
## for which you want to compute the conditional RSA parameters

## Important: At present, the covariate must appear before the RSA variables
##            in ths formula above, that is, you have to specify g.s*x.c and 
##            not x.c*g.s

MLRSA_PickaPoint(m2,name_vars=c("x.c","y.c","x2.c","xy.c","y2.c"),name_cov="g.s",value_cov=-1)
MLRSA_PickaPoint(m2,name_vars=c("x.c","y.c","x2.c","xy.c","y2.c"),name_cov="g.s",value_cov=1)

# These are the results shown in Table 8.

## We can also plot the response surface for a specific value of the Level 2 
## predictor. The function is similar to MLRSA_AverageSurfacePlot; the only 
## difference is that one also deliver the name of the Level 2 variable
## and the value of the Level 2 variable

MLRSA_PickaPointPlot(m2,name_vars=c("x.c","y.c","x2.c","xy.c","y2.c"),
                     outcome="z",data=dat,
                     name_cov="g.s",value_cov=-1,
                     xlab="Real age",ylab="Felt age",zlab="Satisfaction")

# see MakePlots.r for the code for Figure 2 
                     
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##     Region of the significant conditional RSA parameters
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Compute the regions of the values of the Level 2 predictor for which the 
## conditional response surface parameters are significant (or not).
## The function needs the lme4-model,  
## the names of the RSA variables (= name_vars; see above for explanations),
## the name of the Level 2 predictor and the critical z-value. 

MLRSA_Region(m2,name_vars=c("x.c","y.c","x2.c","xy.c","y2.c"),name_cov="g.s",z=1.95)

# These are the results shown in Table 9
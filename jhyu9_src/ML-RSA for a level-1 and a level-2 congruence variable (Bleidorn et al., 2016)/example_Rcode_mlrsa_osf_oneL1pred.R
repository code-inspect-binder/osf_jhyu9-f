
## This code shows how the code provided by Nestler et al. (2019) can be adapted
## to a random intercept model with one individual-level predictor and one
## state-level predictor that is the within-group average of the
## individual-level predictor.

## This code uses functions from the code file MultilevelRSA_25082019.R that was
## published by Steffen Nestler (2018) under a CC-BY4.0 license
## (https://creativecommons.org/licenses/by/4.0/).

## If you use these functions, please cite the paper that they accompany
## (Nestler et al., 2019) and their web address (https://osf.io/jhyu9).

## The code was prepared by Sarah Humberg and edited by Steffen Nestler.

## ++++++++++++++++++++++++++++++++++++
##      Load packages
## ++++++++++++++++++++++++++++++++++++

# if not installed yet: install the necessary packages:
# install.packages(c("lme4", "lmerTest", "RSA"))

library(lme4)
library(lmerTest)
library(RSA)
library(plyr)

# source the R file with the ML-RSA functions (downloaded from https://osf.io/jhyu9)
source("MultilevelRSA_25082019.r") 


## ++++++++++++++++++++++++++++++++++++
##      Prepare the data
## ++++++++++++++++++++++++++++++++++++


## read the data
df <- read.table("fakedata_mlrsa.dat", header=T)
head( df )

# df is a fake data frame that contains the following variables for all individuals in the data set:
#
# id = ID variable for each individual
# state = state where the individual lives
# IV = individual-level value score (e.g., the individual's score on the openness to change scale of the TwIVI)
# selfesteem = the individual's self-esteem


## compute state-level value variable (SV) = average individual-level value within each state
df <- ddply(df, 
            .var="state", 
            .fun=function(subdat){
              subdat$SV  <- mean( subdat$IV, na.rm = TRUE )
              subdat
              }
            )


# center IV and SV on the grand mean across both variables
grand.M <- mean( c(df$IV, df$SV), na.rm = TRUE )
df$IV.c <- df$IV-grand.M
df$SV.c <- df$SV-grand.M

# Note that centering at the grand mean preserves the discrepancy between peoples' individual 
# value score and their state's average value score. 

# For example, Person 2258's raw value score (IV) almost perfectly matches the average value 
# score in the state she lives in (SV), and the same is true for her grand-mean centered scores (IV.c and SV.c): 
df[df$id==2258, c("IV","SV","IV.c","SV.c")]
df[df$id==2258, "IV"] - df[df$id==2258, "SV"]
df[df$id==2258, "IV.c"] - df[df$id==2258, "SV.c"]

# As another example, Person 654's raw value score (IV) exceeds the average value score in the state
# she lives in (SV) by 0.79, and the same is true for her grand-mean centered scores (IV.c and SV.c): 
df[df$id==654, c("IV","SV","IV.c","SV.c")]
df[df$id==654, "IV"] - df[df$id==654, "SV"]
df[df$id==654, "IV.c"] - df[df$id==654, "SV.c"]

## compute higher order terms
df$IV2.c <- df$IV.c^2
df$IVSV.c <- df$IV.c*df$SV.c
df$SV2.c <- df$SV.c^2

## ++++++++++++++++++++++++++++++++++++
##         Pre-Analysis
## ++++++++++++++++++++++++++++++++++++

# define function to count predictor combinations that are considered congruent (within half a grand standard deviation) vs. 
# discrepant in the one or the other direction (with the strategy described in Humberg et al., in press)

discr <- function(data, x, y){
  
  diff <- (data[,x] - data[,y])/sd(c(data[,x], data[,y]), na.rm=T)
  cong <- cut(diff, breaks=c(-Inf, -.5, .5, Inf), labels=c(paste0(x," < ", y), "Congruence", paste0(x," > ", y)))
  tab <- as.data.frame(round(prop.table(table(cong)), 2)*100)
  return(tab)
}

# show percentates for the present data
discr(data=df, x="IV.c", y="SV.c")

## ++++++++++++++++++++++++++++++++++++
##      Conduct RSA 
## ++++++++++++++++++++++++++++++++++++

# We want to estimate the RSA model in which the intercept is allowed to vary across states.
# The equation for individual i living in state j (z = selfesteem, x = IV, y = SV) is:
# zij = b0 + b1*xij + b2*yj + b3*xij^2 + b4*xij*yj + b5*yj^2 + uj + eij

# specify and estimate this model:
m.c <- lmer(selfesteem ~ IV.c + SV.c + IV2.c + IVSV.c + SV2.c + (1 | state), data = df)
summary(m.c)

# Compute and test the RSA parameters with the MLRSA_AverageSurface-function 
# (see the description in the file https://osf.io/x8mut/ for further details)

# NOTE: 
# random_vars = the names of the variables that were set to random in the lmer-object, where 
# variables that were not set to random are specified as "NA" -> in the present model, only 
# the intercept is set to random, but none of the other coefficients, so all entries of random_vars 
# must be NA.

MLRSA_AverageSurface(m.c, 
                     name_vars=c("IV.c","SV.c","IV2.c","IVSV.c","SV2.c"),
                     random_vars=c(NA,NA,NA,NA,NA)) 



# Plot the average surface using MLRSA_AverageSurfacePlot:

MLRSA_AverageSurfacePlot(m.c, 
                         name_vars=c("IV.c","SV.c","IV2.c","IVSV.c","SV2.c"),
                         outcome="selfesteem",
                         data=df,
                         xlab="Individual-level values", ylab="State-level values", zlab="Self-esteem")


# If necessary (e.g., to inspect how many of the data points lie beyond the ridge line), 
# compute further RSA parameters
coefs <- fixef(m.c)
rsa_parameters <- RSA.ST(x = coefs["IV.c"],
                         y = coefs["SV.c"],
                         x2 = coefs["IV2.c"],
                         xy = coefs["IVSV.c"],
                         y2 = coefs["SV2.c"])

# e.g., the coefficients of the first principal axis:
unname(rsa_parameters$p10)
unname(rsa_parameters$p11)

## ++++++++++++++++++++++++++++++++++++
##      References
## ++++++++++++++++++++++++++++++++++++

# Humberg, S., Schönbrodt, F. D., Back, M. D., & Nestler, S. (in press). Cubic
# response surface analysis: Investigating asymmetric and level-dependent
# congruence effects with third-order polynomial models. Psychological Methods.
# https://doi.org/10.1037/met0000352. Retrieved from https://psyarxiv.com/v6m35

# Nestler, S., Humberg, S., & Schönbrodt, F. D. (2019). Response surface
# analysis with multilevel data: Illustration for the case of congruence
# hypotheses. Psychological Methods, 24(3), 291–308.
# https://doi.org/10.1037/met0000199
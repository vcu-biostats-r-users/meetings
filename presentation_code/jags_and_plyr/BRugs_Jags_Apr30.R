## Kabita Joshi
## Updated April 30, 2014
# BRugs example
##------------------------------------------------------------------------
# In this note I will use both BRugs and jags code to show how similar these
# two functions 
# I prefer jags because it gives less error message and each function DIC, CODA
# can be done separately
# This is the example data
#-----------------------------------------------------------------------------
# BRugs Example
#-----------------------------------------------------------------------------
# Set the working directory
setwd("D:/R users group April")    # change this to *your* working directory
getwd()

# Upload the required library
library(BRugs)

# Two samples y1, y2: each with 15 observation
# Use the normal distribution to model these data (parameters of each sample are different)
# Obtain the posterior distributions for the means and variances of each sample
# Estimate the posterior distributions of mu1-mu2 and sigma1/sigma2.
# Mu is mean of the sample and sigma is standard deviation of the sample.

y1=c(1.5,0.54,0.85,2.04,-0.49,-0.65,0.98,-0.63,-1.72,-0.06,0.60,1.25,-0.72,0.66,-0.39)
y2=c(0.80,0.03,-0.37,0.67,0.76,2.47,1.36,2.29,2.26,2.71,-0.28,1.70,1.88,-0.35,1.90)
N1=length(y1) # Number of observation in the first sample


# Specify the data and save it in txt file
bugsData(c('y1','y2','N1'),"q1_data.txt")

# The model
q1model <- function()
	     {
	for (i in 1:N1)
	{
	y1[i]~dnorm(mu1,tau1)
	y2[i]~dnorm(mu2,tau2)
      }
	mu1~dnorm(0,1.0E-10)
	mu2~dnorm(0,1.0E-10)
	sigma1~dunif(0,10)
	sigma2~dunif(0,10)
	tau1<-1/(sigma1*sigma1)
	tau2<-1/(sigma2*sigma2)
	var1<-1/tau1
	var2<-1/tau2
	diff_mean<-mu1-mu2
	ratio_sigma<-sigma1/sigma2
		}

writeModel(q1model,'q1_BUGS.txt')

# Initialize the chain and save it in text file.
bugsInits(list(pairlist(mu1=5,sigma1=2,mu2=7,sigma2=3)), 1, "q1_inits.txt")

#bugsInits(inits, numChains = 1, fileName, format="E", digits = 5)

Fit1<-BRugsFit(modelFile='q1_BUGS.txt', data='q1_data.txt',inits= c('q1_inits.txt'), numChains = 1,
    parametersToSave=c('mu1','mu2','var1','var2','diff_mean','ratio_sigma'), nBurnin = 1000, nIter = 10000,
	nThin=1,coda=F,DIC=TRUE,seed=12)
Fit1$Stats
Fit1$DIC

#seed=Set the starting state of the random number generator.

#BRugsFit(modelFile, data, inits, numChains = 3, parametersToSave,
#    nBurnin = 1000, nIter = 1000, nThin = 1, coda = FALSE,
#    DIC = TRUE, working.directory = NULL, digits = 5, seed=NULL,
#    BRugsVerbose = getOption("BRugsVerbose"))

#---------------------------------------------------------------------------
# End of BRugs code

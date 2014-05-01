## Kabita Joshi
## April 28, 2014
##------------------------------------------------------------------------
# In this note I will use both BRugs and jags code to show how similar these
# two functions 
# I prefer jags because it gives less error message and each function DIC, CODA
# can be done separately
# This is the example data
#-----------------------------------------------------------------------------
# BRugs Example
#-----------------------------------------------------------------------------
setwd("C:/Bayesian data analysis")    # change this to *your* working directory
getwd()
library(BRugs)

y1=c(1.5,0.54,0.85,2.04,-0.49,-0.65,0.98,-0.63,-1.72,-0.06,0.60,1.25,-0.72,0.66,-0.39)
y2=c(0.80,0.03,-0.37,0.67,0.76,2.47,1.36,2.29,2.26,2.71,-0.28,1.70,1.88,-0.35,1.90)

bugsData(c('y1','y2'),"q1_data.txt")

q1model <- function()
{
  for (i in 1:15)
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
bugsInits(list(pairlist(mu1=5,sigma1=2,mu2=7,sigma2=3)), 1, "q1_inits.txt")

BRugsFit(modelFile='q1_BUGS.txt', data='q1_data.txt',inits= c('q1_inits.txt'), numChains = 1,
         parametersToSave=c('mu1','mu2','var1','var2','diff_mean','ratio_sigma'), nBurnin = 1000, nIter = 10000,
         nThin=1,coda=FALSE,DIC=TRUE,seed=12)

#seed=Set the starting state of the random number generator.

#BRugsFit(modelFile, data, inits, numChains = 3, parametersToSave,
#    nBurnin = 1000, nIter = 1000, nThin = 1, coda = FALSE,
#    DIC = TRUE, working.directory = NULL, digits = 5, seed=NULL,
#    BRugsVerbose = getOption("BRugsVerbose"))

#---------------------------------------------------------------------------
# Jags exmaple
#-----------------------------------------------------------------------------
# Download and install the jags from the link
# link: https://sourceforge.net/projects/mcmc-jags
library(rjags)
y1=c(1.5,0.54,0.85,2.04,-0.49,-0.65,0.98,-0.63,-1.72,-0.06,0.60,1.25,-0.72,0.66,-0.39)
y2=c(0.80,0.03,-0.37,0.67,0.76,2.47,1.36,2.29,2.26,2.71,-0.28,1.70,1.88,-0.35,1.90)

# Specify the data 
dataList=list(
  'y1'=y1,
  'y2'=y2
) # dataList closed.

# Write a model
cat("model {
    for (i in 1:15)
{
    y1[i]~dnorm(mu1,tau1)
    y2[i]~dnorm(mu2,tau2)
} # for i closed
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
    }", # model closed
	file="model1.txt")

#Initialize the chain.
mean.inits=function(){
  list('mu1'=5,
       'mu2'=7,
       'sigma1'=2,
       'sigma2'=3,
       'diff_mean'=0,
       'ratio_sigma'=1)}

# for n.chain=3, we need 3 inits
initsList <- list(mean.inits(),mean.inits(),mean.inits())

# Run the chains
parameters <- c("mu1","mu2","var1","var2","diff_mean","ratio_sigma")
adaptSteps <- 100
burnInSteps <- 100
nChains <-3
numSavedSteps <-100
thinSteps <-3
nIter <- ceiling((numSavedSteps * thinSteps) / nChains)

# Create, initialize and adapt the model

jagsModel <- jags.model(file="model1.txt",
                        data = dataList,
                        inits = mean.inits,
                        n.chains = nChains,
                        n.adapt = adaptSteps)

#DIC 

dicSamples<-dic.samples(jagsModel, n.iter=nIter, thin=thinSteps, type="pD")

# Burn-in
update(jagsModel, n.iter=burnInSteps)
# Coda
codaSamples<-coda.samples(jagsModel, variable.names=parameters,n.iter=nIter,
                          thin=thinSteps) #for MCMC object

gelman <- gelman.diag(codaSamples) # for diagnostic purpose

#-------------------------------------------------------------------
#--- End of BRugs and Jags Code---------------------------------------------





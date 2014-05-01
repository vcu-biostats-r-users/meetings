# Jags exmaple
# Author : Kabita Joshi
# April 30, 2014
#-----------------------------------------------------------------------------
# Download and install the jags from the link
# link: https://sourceforge.net/projects/mcmc-jags
#Set the working directory
setwd("D:/R users group April")    # change this to *your* working directory
getwd()

#Upload the required library
library(rjags)

y1=c(1.5,0.54,0.85,2.04,-0.49,-0.65,0.98,-0.63,-1.72,-0.06,0.60,1.25,-0.72,0.66,-0.39)
y2=c(0.80,0.03,-0.37,0.67,0.76,2.47,1.36,2.29,2.26,2.71,-0.28,1.70,1.88,-0.35,1.90)
N1=length(y1) # Number of observation in the first sample

# Specify the data as a list.
dataList=list(
		'y1'=y1,
		'y2'=y2,
		'N1'=N1
		) # dataList closed.


# The model
cat("model {
	for (i in 1:N1)
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
			".RNG.name" = "base::Super-Duper",
			".RNG.seed" = 2
		      )}

# for n.chain=3, we need 3 inits
initsList <- list(mean.inits(),mean.inits(),mean.inits())

# Specify the parameters to be monitored, adaptation steps, burnin steps....

parameters <- c("mu1","mu2","var1","var2","diff_mean","ratio_sigma")# The parameters
# to be monitored
adaptSteps <- 100	# Number of steps to "tune" the samplers
burnInSteps <- 100 # Number of steps to "burn-in" the samplers
nChains <-3 # Number of chains to run
numSavedSteps <-100 # Number of steps in chains to save
thinSteps <-3 # Number of steps to "thin" the samplers
nIter <- ceiling((numSavedSteps * thinSteps) / nChains) # Steps per chain

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

#gelman <- gelman.diag(codaSamples) # for diagnostic purpose
summary1<-summary(codaSamples)
stat1<-summary1$stat
CI1<-summary1$quant

#-------------------------------------------------------------------
#--- End of Jags Code---------------------------------------------






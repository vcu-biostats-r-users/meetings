system.time({
  doors <- 1:3
  runs <- 1e6
  game.outputs <- numeric(runs)
  for (run in 1:runs){
    prize.door <- sample(doors, size=1)
    choice <- sample(doors, size=1)
    
    if (choice!=prize.door) game.outputs[run] <- 1 # Always switch
  }
  avg <- mean(game.outputs)
})[3]
avg


#########################################
# ---------------------------------------
# Functions
# ---------------------------------------
#########################################

# One simulation of the Monty Hall game
onerun <- function(.){ # Function of no arguments
  doors<-1:3
  prize.door <- sample(doors, size=1)
  choice <- sample(doors, size=1)
  
  if (choice==prize.door) return(0) else return(1) # Always switch
}
####################################################################
#Using lapply with a function

system.time({runs<-1e6
             game.outputs<-numeric(runs)
             avg<-mean(unlist(lapply(game.outputs,onerun)))
})[3]
avg
####################################################################
# Many simulations of Monty Hall games
MontyHall2 <- function(runs, cores){ #cores=detectCores()
  
  require(parallel)
  #doors <- 1:3
  # clusterApply() for Windows
  #if (Sys.info()[1] == "Windows"){
    cl <- makeCluster(cores)
    game.outputs<-numeric(runs)
  
    runtime <- system.time({
      A<-parLapply(cl,game.outputs,onerun)
      avg<- mean(unlist(A))
    })[3]
    stopCluster(cl) # Don't forget to do this--I frequently do
    
    # mclapply() for everybody else
#   } else {
#     runtime <- system.time({
#       avg <- mean(unlist(mclapply(X=1:runs, FUN=onerun, mc.cores=cores)))
#     })[3]
  #}
  return(list(avg=avg, runtime=runtime))
}

#########################################
# ---------------------------------------
# Outputs
# ---------------------------------------
#########################################

#run1 <- rbind(c(MontyHall2(1e6, cores=1), "cores"=1))
run2 <- rbind(c(MontyHall2(1e5, cores=2), "cores"=2))
run3 <- rbind(c(MontyHall2(1e5, cores=4), "cores"=4))
run2
run3
rm(list=ls())

fact1 <- function(x) {
if (x==1) return(1) else return(x*fact1(x-1))}

fact2 <- function(x) {
prod(1:x)}

fact3 <- function(x) {
gamma(x+1)}

fact1(100)
fact2(100)
fact3(100)

system.time(for (i in 1:1e4) fact1(100))
system.time(for (i in 1:1e4) fact2(100))
system.time(for (i in 1:1e4) fact3(100))


fibR <- function(n) {
    if (n == 0) return(0)
    if (n == 1) return(1)
    return (fibR(n - 1) + fibR(n - 2))
}


fibonacci <- local({
    memo <- c(1, 1, rep(NA, 100))
    f <- function(x) {
        if(x == 0) return(0)
        if(x < 0) return(NA)
        if(x > length(memo))
        stop("'x' too big for implementation")
        if(!is.na(memo[x])) return(memo[x])
        ans <- f(x-2) + f(x-1)
        memo[x] <<- ans
        ans
    }
})

fibR(30)   ## momemt to have rest!
fibonacci(30)




######################
vus.simple <- function(new.1,new.2,new.3) {
ksum <- 0
n1=length(new.1)
n2=length(new.2)
n3=length(new.3)
for (i in 1:n1) {
for (j in 1:n2) {
for (k in 1:n3) {
ksum = ksum + 1*((new.1[i]<new.2[j])&(new.2[j]<new.3[k]))
                }}}
return(ksum/n1/n2/n3)
}



vus.MWU <- function(new.1,new.2,new.3) {
n1=length(new.1)
n2=length(new.2)
n3=length(new.3)
usum <- function(u) {
is.small = new.2[u<new.2]
if (length(is.small)>0) return(
     sum(sapply(is.small, function(v) sum(v<new.3)))
      ) else return(0) }
return(sum(sapply(new.1,usum))/n1/n2/n3)
}


dyn.load("empAUC_VUS.dll")
nonp.vus <- function(x,y,z) {
.C("empVUS",as.double(x),as.double(y),as.double(z),
            as.integer(length(x)),
            as.integer(length(y)),
            as.integer(length(z)),
            result=double(1))[["result"]]
}



new.1 <- rnorm(100)
new.2 <- rnorm(100)
new.3 <- rnorm(100)
vus.simple(new.1,new.2,new.3)
vus.MWU(new.1,new.2,new.3)
nonp.vus(new.1,new.2,new.3)


system.time(
for (i in 1:100){
new.1 <- rnorm(100)
new.2 <- rnorm(100)
new.3 <- rnorm(100)
vus.MWU(new.1,new.2,new.3)
})

system.time(
for (i in 1:100){
new.1 <- rnorm(100)
new.2 <- rnorm(100)
new.3 <- rnorm(100)
nonp.vus(new.1,new.2,new.3)
})


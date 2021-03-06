# Author : Kabita Joshi
# Updated : April 30, 2014
#------------------------------------------------------------
# There are many functions in plyr we will look at ddply and ldply of library(plyr)
# Function ddply is used for summarizing the data based on different category
# Example 1 for ddply
#----------------------------------
library(plyr)
name <- c("A","B","C","D","E","F","G","H")
gender <- as.factor(c(rep(1,4),rep(2,4)))
race_cat <- as.factor(c(1,2,3,1,2,3,1,2))
disease <- as.factor(c(1,0,0,1,1,0,1,1))
age <- c(40,35,78,34,67,23,45,56)
data_new <- data.frame(name,gender,race_cat,disease,age)
# data structure
#------------------------------------
#  name gender race_cat disease age
#1    A      1        1       1  40
#2    B      1        2       0  35
#3    C      1        3       0  78
#4    D      1        1       1  34
#5    E      2        2       1  67
#6    F      2        3       0  23
#7    G      2        1       1  45
#8    H      2        2       1  56
#---------------------------

# summarize the age of people by gender, race and disease category
summary1 <- ddply(data_new,.(gender,race_cat,disease),summarize,
	mean1=mean(age),sd1=sd(age))
#-----------------------------------------------
# Result for example 1
  gender race_cat disease mean1      sd1
1      1        1       1  37.0 4.242641
2      1        2       0  35.0       NA
3      1        3       0  78.0       NA
4      2        1       1  45.0       NA
5      2        2       1  61.5 7.778175
6      2        3       0  23.0       NA

#----------------------------------------------

# Example 2 ddply
dfx <- data.frame(
  group = c(rep('A', 8), rep('B', 15), rep('C', 6)),
  gender = sample(c("M", "F"), size = 29, replace = TRUE),
  age = runif(n = 29, min = 18, max = 54)
	)
head(dfx)
# 	group gender      age
#1     A   F 	29.40447
#2     A   F 	23.53007
#3     A   F 	34.90296
#4     A   M 	19.74842
#5     A   M 	32.82742
#6     A   F 	46.00904

# Apply the function ddply

summary2 <- ddply(dfx, .(group, gender), summarize,
 mean = round(mean(age), 2),
 sd = round(sd(age), 2))
summary2

# group gender  mean    sd
#1     A      F 45.28  4.67
#2     A      M 33.48 13.98
#3     B      F 40.36 11.29
#4     B      M 39.60 12.01
#5     C      F 39.57 12.85
#6     C      M 37.21  6.50

## What if we just use subset function?
subset1<-subset(dfx,gender=='M' & group=='A')
mean(subset1[,3]) #33.48
subset2<-subset(dfx,gender=='F' & group=='A')
mean(subset2[,3]) #45.27
subset3<-subset(dfx,gender=='M' & group=='B')
mean(subset3[,3]) #39.59
subset4<-subset(dfx,gender=='F' & group=='B')
mean(subset4[,3]) #40.35
#-----------------------------------------------------
# Function ldply
# Like lapply in base R it is used for list data
rho1 <- seq(0,1,length.out=10)
rho2 <- seq(1,10,length.out=10)
rho3 <- seq(5,10,length.out=10)
rho <- list(rho1,rho2,rho3)
# data structure in list
#----------------------------------------------------
#[[1]]
# [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
# [8] 0.7777778 0.8888889 1.0000000

#[[2]]
# [1]  1  2  3  4  5  6  7  8  9 10

#[[3]]
# [1]  5.000000  5.555556  6.111111  6.666667  7.222222  7.777778  8.333333
# [8]  8.888889  9.444444 10.000000
#---------------------------------------------------------------

summary3 <- ldply(rho,.fun=function(x){
	sum1 <- sum(x)
	mean1 <- mean(x)
	out <- c(sum1,mean1)
	})
colnames(summary3) <- c('sum1','mean1')
# Result of example ldply
#---------------------------------------------------------
#> summary3
#  sum1  mean1
#1  5 0.5
#2 55 5.5
#3 75 7.5

## What happens when we use lapply instead of ldply?

summary4 <- lapply(rho,function(x){
			sum1 <- sum(x)
			mean1 <- mean(x)
			out <- c(sum1,mean1)
			})
summary4
#[[1]]
#[1] 5.0 0.5

#[[2]]
#[1] 55.0  5.5

#[[3]]
#[1] 75.0  7.5





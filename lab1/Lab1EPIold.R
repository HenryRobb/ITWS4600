# Had to adjust the sequence range as there was a value < 20

#slide 9
EPI_data <- read.csv("epi2024results06022024.csv") 
# Note: replace default data frame name – cannot start with numbers! Munging has begun! (ugh)

# Note: replace <path> with either a directory path or use: 

View(EPI_data)

#slide 10
attach(EPI_data)
EPI.old
NAs <- is.na(EPI.old)
EPI.old.noNAs <- EPI.old[!NAs]

# Exercise 1
summary(EPI.old)

fivenum(EPI.old,na.rm=TRUE)# [1] 32.1 48.6 59.2 67.6 93.5 
stem(EPI.old) # stem and leaf plot 
hist(EPI.old) 
hist(EPI.old, seq(15., 75., 1.0), prob=TRUE) 
lines(density(EPI.old,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(EPI.old)#Use help(<command>), e.g. > help(stem) 

# comparing distributions
boxplot(EPI.old, APO.old)

#histogram of EPI.old
hist(EPI.old, seq(15., 75., 1.0), prob=TRUE)

lines (density(EPI.old,na.rm=TRUE,bw=1.)) 

rug(EPI.old)

# with bw="SJ"
hist(EPI.old, seq(15., 75., 1.0), prob=TRUE)

lines (density(EPI.old,na.rm=TRUE,bw="SJ")) 

rug(EPI.old) 

# making the histogram more satisfying

x<-seq(20,80,1) 

q<- dnorm(x,mean=42, sd=5,log=FALSE) 

lines(x,q)
lines(x,.4*q) 

q<-dnorm(x,mean=65, sd=5,log=FALSE) 

lines(x,.12*q) 


# Exercise 2

#Cumulative density function? 
plot(ecdf(EPI.old), do.points=FALSE, verticals=TRUE)


# Quantile-Quantile?
qqnorm(EPI.old); qqline(EPI.old) 

# Make a Q-Q plot against the generating distribution by: 
qqplot(rnorm(250), EPI.old, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.old)

qqplot(rt(250, df = 5), EPI.old, xlab = "Q-Q plot for t dsn") 
qqline(EPI.old)


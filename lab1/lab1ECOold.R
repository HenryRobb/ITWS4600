# Had to adjust the sequence range as there was a value < 20

#slide 9
ECO_data <- read.csv("epi2024results06022024.csv") 
# Note: replace default data frame name – cannot start with numbers! Munging has begun! (ugh)

# Note: replace <path> with either a directory path or use: 

View(ECO_data)

#slide 10
attach(ECO_data)
ECO.old
NAs <- is.na(ECO.old)
ECO.old.noNAs <- ECO.old[!NAs]

# Exercise 1
summary(ECO.old)

fivenum(ECO.old,na.rm=TRUE)# [1] 32.1 48.6 59.2 67.6 93.5 
stem(ECO.old) # stem and leaf plot 
hist(ECO.old) 
hist(ECO.old, seq(20., 90., 1.0), prob=TRUE) 
lines(density(ECO.old,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(ECO.old)#Use help(<command>), e.g. > help(stem) 

# comparing distributions
boxplot(ECO.old, APO.old)

#histogram of ECO.old
hist(ECO.old, seq(20., 90., 1.0), prob=TRUE)

lines (density(ECO.old,na.rm=TRUE,bw=1.)) 

rug(ECO.old)

# with bw="SJ"
hist(ECO.old, seq(20., 90., 1.0), prob=TRUE)

lines (density(ECO.old,na.rm=TRUE,bw="SJ")) 

rug(ECO.old) 

# making the histogram more satisfying

x<-seq(20,80,1) 

q<- dnorm(x,mean=42, sd=5,log=FALSE) 

lines(x,q)
lines(x,.4*q) 

q<-dnorm(x,mean=65, sd=5,log=FALSE) 

lines(x,.12*q) 


# Exercise 2

#Cumulative density function? 
plot(ecdf(ECO.old), do.points=FALSE, verticals=TRUE)


# Quantile-Quantile?
qqnorm(ECO.old); qqline(ECO.old) 

# Make a Q-Q plot against the generating distribution by: 
qqplot(rnorm(250), ECO.old, xlab = "Q-Q plot for norm dsn") 
qqline(ECO.old)

qqplot(rt(250, df = 5), ECO.old, xlab = "Q-Q plot for t dsn") 
qqline(ECO.old)


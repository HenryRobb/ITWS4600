#slide 9
EPI_data <- read.csv("epi2024results06022024.csv") 
# Note: replace default data frame name – cannot start with numbers! Munging has begun! (ugh)

# Note: replace <path> with either a directory path or use: 

View(EPI_data)

#slide 10
attach(EPI_data)
EPI.new
NAs <- is.na(EPI.new)
EPI.new.noNAs <- EPI.new[!NAs]

# Exercise 1
summary(EPI.new)

fivenum(EPI.new,na.rm=TRUE)# [1] 32.1 48.6 59.2 67.6 93.5 
stem(EPI.new) # stem and leaf plot 
hist(EPI.new) 
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” 
rug(EPI.new)#Use help(<command>), e.g. > help(stem) 

# comparing distributions
boxplot(EPI.new, APO.new)

#histogram of EPI.new
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)

lines (density(EPI.new,na.rm=TRUE,bw=1.)) 

rug(EPI.new)

# with bw="SJ"
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)

lines (density(EPI.new,na.rm=TRUE,bw="SJ")) 

rug(EPI.new) 

# making the histogram more satisfying

x<-seq(20,80,1) 

q<- dnorm(x,mean=42, sd=5,log=FALSE) 

lines(x,q)
lines(x,.4*q) 

q<-dnorm(x,mean=65, sd=5,log=FALSE) 

lines(x,.12*q) 


# Exercise 2

#Cumulative density function? 
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)


# Quantile-Quantile?
qqnorm(EPI.new); qqline(EPI.new) 

# Make a Q-Q plot against the generating distribution by: 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)

qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)


#Experiment-1
install.packages("moments")
install.packages(c("moments", "ggplot2", "devtools"))
X=as.character(5.2) 
paste("Baa", "Baa", "Black", "Sheep") 
library(readr)
importedfile <- read_csv("C:/Users/sarva/Documents/Academics/ProbabExp1.csv")
read.csv(file.choose()) #in console
library(readxl)
importedfile <- read_excel("C:/Users/sarva/Documents/Academics/ProbabExp1.xlsx")
View(importedfile)
seq(1,10,2)
seq(1:10)
seq(1,10,by=2)
y <- c(1:6,0,-3)
z <- 1:7



#Experiment-2
age=c(30,37,45,32,50,60,35,32,34,43,32,30,43,50,60)
empinfo <- data.frame(empid, age, gender, status)
empinfo$gender = factor(empinfo$gender, labels = c('male','female'))
male= subset(empinfo, empinfo$gender='male')
summary(male)
summary(age)
table1 = table(empinfo$gender, empinfo$status)
plot(empinfo$age,type="l",main="Age of employees",xlab="empid",ylab="age in years")
pie(table1)
barplot(table2,beside=T,xlim=c(1,15),ylim=c(0,5),col=c("blue", "red"))
legend("topright",legend=rownames(table2),fill=c('blue','red'),bty="n")
boxplot(empinfo$age~empinfo$status,col=c('red','blue'))



#Experiment-3 : Correlation & Regression
data=cars
summary(data)
v1=var(data$speed)
v2=var(data$dist)
#covariance between speed and distance
covariance=cov(data$speed,data$dist) 
covariance=var(data$speed,data$dist)
#correlation coefficient using Pearson's formula
corr=covariance/(sd(data$speed)*sd(data$dist))
corr=cor(data$speed,data$dist) 
# Test for association between paired samples
cor.test(data$speed,data$dist,method="pearson")
cor.test(data$speed,data$dist,method="spearman")
# Regression
regression1=lm(data$speed~data$dist)
regression1 <- lm(speed ~ dist, data = data)
plot(data$dist, data$speed)
abline(regression1)
summary(regression1)
coeff=coefficients(regression1)
mode1<-lm(bmi~weight)
summary.lm(mode1)



#Experiment-4 : Multiple regression
# Linear regression model of Y on X1 and X2
X=data$mpg
Y=mtcars$disp
RegModel=lm(Y~X1+X2)
RegModel<-lm(Z~X+Y)
RegModel
library(scatterplot3d)
graph=scatterplot3d(X,Y,Z)
graph$plane3d(RegModel)



#Experiment-5 : Binomial Distribution
n=10
p=0.5
x=0:n
fx=dbinom(x,n,p)
fx
data.frame("Prob"=fx,row.names=x)
Fx=pbinom(x,n,p)
Fx
data.frame("Cum-Prob"=Fx,row.names=x) 
par(mfrow=c(2,1)) 
plot(x,fx,type='h',xlab="x",ylab="f[x]", main="pmf-Binomial")
plot(x,Fx,type='h',xlab="y",ylab="F[x]", main="cdf-Binomial")
#Mean & Variance
Ex=weighted.mean(x,fx)
mu=Ex
mu
Ex2=weighted.mean(x^2,fx)
sigma2=Ex2-(Ex)^2
sigma2
# Expected number of r successes and n-r failures in N trials
N=256
r=8
n=10
p=0.5
prs=dbinom(r,n,p)
ens=N*prs
ens
pv1=dbinom(2,n,p)
pv2=pbinom(2,n,p)-pbinom(1,n,p)
pv3=sum(dbinom(c(2:9),n,p))
pv4=1-pbinom(1,n,p)
pv5=pbinom(2,n,p,lower.tail=FALSE) # >=2 success
#P(X<=k)>=alpha
#P(X>k)=beta
# Find k for which P(X<=k)>=alpha
alpha=0.25
k1=qbinom(alpha,n,p)
k1
# Find k for which P(X>k)=beta
beta=1-alpha
k2=qbinom(beta,n,p,lower.tail=FALSE)
k2

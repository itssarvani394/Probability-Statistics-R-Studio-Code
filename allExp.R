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
pv5=pbinom(2,n,p,lower.tail=FALSE) # False-atLeast True-atMost
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



#Experiment-6 : poisson Distribution
n=25
p=0.2
lambda=n*p
x=seq(0,n)
# probability distribution of X and the table
px=dpois(x,lambda) 
px
data.frame("Prob"=px,row.names=x)
# cumulative distribution of X and the table
Fx=ppois(x,lambda) 
Fx
data.frame("Cum-Prob"=Fx,row.names=x) 
# Plot Poisson distribution and its Cumulative distribution
par(mfrow=c(2,1)) 
plot(x,px,type='h',xlab="x",ylab="p[x]", main="pmf-Poisson")
plot(x,Fx,type='h',xlab="y",ylab="F[x]", main="cdf-Poisson")
# Mean (mu) and variance (sigma2)
Ex=weighted.mean(x,px)
mu=Ex
mu
Ex2=weighted.mean(x^2,px)
sigma2=Ex2-(Ex)^2
sigma2
pv1=dpois(2,lambda)
pv2=ppois(2,lambda)-ppois(1,lambda)
pv3=sum(dpois(c(2:9),lambda))
pv4=1-ppois(1,lambda)
pv5=ppois(1,lambda,lower.tail=FALSE)
# Find k for which P(X<=k)>=alpha
alpha=0.25
k1=qpois(alpha,lambda)
k1
#Worked Example
lambda=3
p1=dpois(5,lambda)
round(p1,4)



#Experiment-7
#Large Sample Mean Test
#z=xbar-mu/(sigma/sqrt(n))
xbar=14.6
mu0=15.4
sigma=2.5
n=35
z=(xbar-mu0)/(sigma/sqrt(n))
alpha=0.05
zalpha=qnorm(1-(alpha/2))
pval=2*pnorm(z)
if(pval>alpha){print("Accept Null hypothesis")} else{print("Reject Null hypothesis")}
#Large Sample proportion Test
#z=p-P/(sqrt(PQ/n))
n=640
Sprop=63/n
Pprop=0.1726
q=1-Pprop
z=(Sprop-Pprop)/sqrt(Pprop*q/n)
alpha=0.05
zalpha=qnorm(1-alpha)
E=qnorm(.975) #critical Value
c(-E,E) #critical Region
Sprop+c(-E,E)*sqrt(Pprop*(1-Pprop)/n) #confidence Interval
if(z>-E && z<E){print("Hospital is not efficient")} else{print("Hospital is efficient")}



#Experiment-8
#Two sample Mean Test
x1=72
x2=70
s1=8
s2=6
n1=32
n2=36
z=(x1-x2)/sqrt((8^2/32)+(6^2/36))
alpha=0.05
zalpha=qnorm(1-alpha)
if(z<zalpha){print("Accept H0")}else{print("Reject H0")}
#Two Sample Proportion Test
p1=0.20
p2=0.185
n1=900
n2=1600
P=(n1*p1+n2*p2)/(n1+n2)
Q=1-P
z=(p1-p2)/sqrt(P*Q*((1/n1)+(1/n2)))
alpha=0.05
zalpha=qnorm(1-(alpha/2))
if(z<=zalpha){print("Accept Null hypothesis")} else{print("Reject Null hypothesis")}



#Experiment-9
#Student's t-test
sample1=c(19,17,15,21,16,18,16,14)
sample2=c(15,14,15,19,15,18,16,20)
t=t.test(sample1,sample2)
cv=t$statistic #test statistic
tv=qt(0.975,14) #critical value
if(cv <= tv){print("Accept Ho")} else{print("Reject Ho")}
#Paired t-test
test1=c(19,17,15,21,16,18,16,14,19,20)
test2=c(15,14,15,19,15,18,16,20,22,19)
t=t.test(sample1,sample2,paired=TRUE)
alpha=0.05 #level of significance
tv=t$p.value #p-value
if(tv >alpha){print("Accept Ho")} else{print("Reject Ho")}
#F-test
sample1=c(19,17,15,21,16,18,16,14)
sample2=c(15,14,15,19,15,18,16,20)
f=var.test(sample1,sample2)
cv=f$statistic
tv=qf(0.95,7,7)
if(cv <= tv){print("Accept Ho")} else{print("Reject Ho")}



#Experiment-10
#chi-square goodness of fit test
n=5
alpha=0.05
N=256
P = 0.5
x = c(0:n)
obf = c(5,35,75,84,45,12)
exf = (dbinom(x,n,P)*256)
sum(obf)
sum(exf)
chisq<-sum((obf-exf)^2/exf)
tv = qchisq(1-alpha,n)
if(cv <= tv){print("Accept H0/Fit is good")} else{print("Reject H0/Fit is not good")}
#chi-square test of independence
data<-matrix(c(69,51,81,20,35,44),ncol=2,byrow=T)
l=length(data)
cv=chisq.test(data)
cv=cv$p.value
if(cv >alpha){print("Attributes are independent")} else{print("Attributes are not independent")}

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

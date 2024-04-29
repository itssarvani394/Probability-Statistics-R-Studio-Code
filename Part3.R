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

#SETA
#Question-1
X <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
Y <- c(9, 8, 10, 12, 11, 13, 14, 16, 15)
mean_X <- mean(X)
mean_Y <- mean(Y)
mean_X
mean_Y
sd_X <- sd(X)
sd_Y <- sd(Y)
sd_X
sd_Y
cov_XY <- cov(X, Y)
cov_XY
cor_XY <- cor(X, Y)
cor_XY
plot(X, Y, main = "Scatter plot of Y on X", xlab = "X", ylab = "Y",col = "blue")
regression1=lm(Y~x)
abline(regression1, col = "red")

#Question-2
sample1 <- c(24, 27, 26, 21, 25)
sample2 <- c(27, 30, 28, 31, 22, 36)
t_test_result <- t.test(sample1, sample2)
cv <- t_test_result$statistic
tv <- qt(0.975, df = length(sample1) + length(sample2) - 2)
if(cv <= tv) {
  print("Accept Ho: The means of the two samples are not significantly different.")
} else {
  print("Reject Ho: The means of the two samples are significantly different.")
}

#Question-3
x1 <- c(9, 17, 15, 21, 16, 18, 16, 14)
x2 <- c(15, 14, 15, 19, 15, 18, 16, 20)
x1b <- mean(x1)
x2b <- mean(x2)
n1 <- length(x1)
n2 <- length(x2)
nu1 <- n1 - 1
nu2 <- n2 - 1
s12 <- var(x1)
s22 <- var(x2)
F <- s12 / s22
alpha <- 0.05
tfv <- qf(1 - alpha/2, nu1, nu2)
tfv
if (F >= tfv) {
  print("Reject Ho: The variances are significantly different.")
} else {
  print("Accept Ho: The variances are not significantly different.")
}


#SETB
#Question-1
n <- 10
p <- 0.5
n
p
x <- 0:n 
fx <- dbinom(x, n, p)
data.frame("Prob" = fx, row.names = x)
Fx <- pbinom(x, n, p)
data.frame("Cum-Prob" = Fx, row.names = x)
plot(x, fx, type = 'h', xlab = "x", ylab = "f[x]", main = "pdf-Binomial")
plot(x, Fx, type = 'h', xlab = "y", ylab = "F[x]", main = "cdf-Binomial")
prob_at_most_5 <- pbinom(5, size = n, prob = p)
prob_between_3_and_6 <- diff(pbinom(c(3, 6), size = n, prob = p))
prob_between_3_and_6_inclusive <- pbinom(6, size = n, prob = p) - pbinom(2, size = n, prob = p)
prob_at_least_5 <- 1 - pbinom(4, size = n, prob = p)

#Question-2
x1 <- 1234  
x2 <- 1036  
s1 <- 36   
s2 <- 40    
n1 <- 8     
n2 <- 7    
alpha <- 0.05 
z <- (x1 - x2) / sqrt((s1^2 / n1) + (s2^2 / n2))
z_alpha <- qnorm(1 - alpha)
if (z < z_alpha) {
  print("Accept H0")
} else {
  print("Reject H0")
}

#Question-3
n <- 5
alpha <- 0.05
N <- 256
P <- 0.5
x <- 0:n
obf <- c(5, 35, 75, 84, 45, 12) 
exf <- (dbinom(x, n, P) * N)  
sum(obf)
sum(exf)
chisq <- sum((obf - exf)^2 / exf)
cv <- chisq
cv
tv <- qchisq(1 - alpha, n)
tv
if (cv <= tv) {
  print("Accept H0/Fit is good")
} else {
  print("Reject H0/Fit is not good")
}


#SETC
#Question-3
x1 <- c(19, 17, 15, 21, 16, 18, 16, 14, 19, 20)
x1b <- mean(x1) 
x2 <- c(15, 14, 15, 19, 15, 18, 16, 20, 22, 19) 
x2b <- mean(x2)
d <- x1 - x2  
n <- length(d)
db <- mean(d)  
s2 <- sum((d - db)^2)
s <- sqrt(s2) 
cvt <- db / (s / sqrt(n))  
alpha <- 0.05
nu <- n - 1  
tv <- qt(1 - alpha / 2, nu)  
if (abs(cvt) <= tv) {
  print("Accept Ho")
} else {
  print("Reject Ho")
}

#Question-1
mean_time <- 20
sd_time <- 5
less_than_15 <- pnorm(15, mean = mean_time, sd = sd_time)
more_than_25 <- 1 - pnorm(25, mean = mean_time, sd = sd_time)
between_15_and_25 <- pnorm(25, mean = mean_time, sd = sd_time) - pnorm(15, mean = mean_time, sd = sd_time)
less_than_15
more_than_25
between_15_and_25
x <- seq(mean_time - 10, mean_time + 10, by = 0.1)
pdf <- dnorm(x, mean = mean_time, sd = sd_time)
plot(x, pdf, type = "l", xlab = "x", ylab = "f(x)", main = "Normal pdf")
grid()

#Question-2
observed_A <- 400
n_A <- 100
observed_B <- 400
n_B <- 800
prop_A <- observed_A / n_A
prop_B <- observed_B / n_B
expected_A <- n_A * prop_B
expected_B <- n_B * prop_A
chi_squared <- ((observed_A - expected_A)^2 / expected_A) + ((observed_B - expected_B)^2 / expected_B)
df <- 1  # for a 2x2 contingency table
p_value <- pchisq(chi_squared, df, lower.tail = FALSE)
chi_squared
df
p_value
if (p_value < 0.05) {
  cat("There is significant evidence to reject the null hypothesis. There is a significant difference between the proportions of wheat consumers in town A and town B.\n")
} else {
  cat("There is not enough evidence to reject the null hypothesis. There is no significant difference between the proportions of wheat consumers in town A and town B.\n")
}

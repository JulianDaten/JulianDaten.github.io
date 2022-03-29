alpha <- 15
beta <- 1

x <-
  c(15, 14, 9, 12, 16, 10, 14, 12, 19, 17, 14, 18, 17, 15, 11, 17, 21, 15, 12, 12)
n <- 20

alpha_p <- alpha + sum(x)
beta_p <- beta + n

i <- seq(10, 20, 0.1)
plot(i, dgamma(i, alpha_p, beta_p), type = "l", col=2)

# point estimate
qgamma(0.5, alpha_p, beta_p) # median
(alpha_p - 1) / beta_p # mode
alpha_p / beta_p # mean

# interval estimate
cat(qgamma(0.025, alpha_p, beta_p),
    qgamma(1 - 0.025, alpha_p, beta_p))
abline(v = qgamma(0.025, alpha_p, beta_p))
abline(v = qgamma(1 - 0.025, alpha_p, beta_p))

# hypothesis testing
# H0: lambda =< 14 
# at 5% significance 
pgamma(14, alpha_p, beta_p)
pgamma(14, alpha_p, beta_p) < 0.05
# we don't reject H0
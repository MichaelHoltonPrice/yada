# Test calc_conditional_gaussian_integral

# Test for J=1 and K=0
# J=1 / K=0. Check error is given if lo and hi have different lengths
expect_error(
  calc_conditional_gaussian_integral(1.5,0.75,lo=.65,hi=c(.85,1)),
  'lo and hi must have the same length'
)

# J=1 / K=0. Check integral from -Inf .65
expect_equal(
  calc_conditional_gaussian_integral(1.5,0.75,lo=-Inf,hi=.65),
  pnorm((.65-1.5)/sqrt(.75))
)

expect_equal(
  calc_conditional_gaussian_integral(1.5,0.75,lo=-Inf,hi=.65,log=T),
  pnorm((.65-1.5)/sqrt(.75),log=T)
)

# J=1 / K=0. Check integral from .65 to .85
expect_equal(
  calc_conditional_gaussian_integral(1.5,0.75,lo=.65,hi=.85),
  pnorm((.85-1.5)/sqrt(.75)) - pnorm((.65-1.5)/sqrt(.75))
)

expect_equal(
  calc_conditional_gaussian_integral(1.5,0.75,lo=.65,hi=.85,log=T),
  log(pnorm((.85-1.5)/sqrt(.75)) - pnorm((.65-1.5)/sqrt(.75)))
)

# J=1 / K=0. Check integral from .85 to Inf
expect_equal(
  calc_conditional_gaussian_integral(1.5,0.75,lo=.85,hi=Inf),
  1 - pnorm((.85-1.5)/sqrt(.75))
)

expect_equal(
  calc_conditional_gaussian_integral(1.5,0.75,lo=.85,hi=Inf,log=T),
  log(1 - pnorm((.85-1.5)/sqrt(.75)))
)

# Test for J=0 and K=1
# J=0 / K=1
expect_equal(
  calc_conditional_gaussian_integral(1.25,0.85,y_giv=1.3),
  dnorm(1.3,mean=1.25,sd=sqrt(.85))
)

expect_equal(
  calc_conditional_gaussian_integral(1.25,0.85,y_giv=1.3,log=T),
  dnorm(1.3,mean=1.25,sd=sqrt(.85),log=T)
)

# Test for J=1 and K=1
# Specify the covariance matrix with ordinal variance .75, continuous variance
# .85, and correlation .4
cov_mat <- diag(c(.75,.85))
cov_mat[1,2] <- sqrt(.75)*sqrt(.85)*.4
cov_mat[2,1] <- cov_mat[1,2]

# Caculate the conditiona mean and standard deviation
# The conditional mean and standard deviation:
#         ord_mean    z     ord_sd     cont_sd     y_giv   cont_mean
mu_bar <-      1.5 + .4 * sqrt(.75) / sqrt(.85) * (  1.3 -      1.25)
#           ord_sd            z
sd_bar <- sqrt(.75) * sqrt(1-.4^2)

# J=1 / K=1. Check integral from -Inf .65
expect_equal(
  calc_conditional_gaussian_integral(c(1.5,1.25),cov_mat,lo=-Inf,hi=.65,y_giv=1.3),
  dnorm(1.3,mean=1.25,sd=sqrt(.85))*pnorm((.65-mu_bar)/sd_bar)
)

expect_equal(
  calc_conditional_gaussian_integral(c(1.5,1.25),cov_mat,lo=-Inf,hi=.65,y_giv=1.3,log=T),
  dnorm(1.3,mean=1.25,sd=sqrt(.85),log=T) + pnorm((.65-mu_bar)/sd_bar,log=T)
)

# J=1 / K=1. Check integral from .65 to .85
expect_equal(
  calc_conditional_gaussian_integral(c(1.5,1.25),cov_mat,lo=.65,hi=.85,y_giv=1.3),
  dnorm(1.3,mean=1.25,sd=sqrt(.85))*(pnorm((.85-mu_bar)/sd_bar) - pnorm((.65-mu_bar)/sd_bar))
)

expect_equal(
  calc_conditional_gaussian_integral(c(1.5,1.25),
                                     cov_mat,
                                     lo=.65,
                                     hi=.85,
                                     y_giv=1.3,
                                     log=T),
  dnorm(1.3,
        mean=1.25,
        sd=sqrt(.85),log=T) +
           log(pnorm((.85-mu_bar)/sd_bar) - pnorm((.65-mu_bar)/sd_bar))
)

# J=1 / K=1. Check integral from .85 to Inf
expect_equal(
  calc_conditional_gaussian_integral(c(1.5,1.25),
                                     cov_mat,
                                     lo=.85,
                                     hi=Inf,
                                     y_giv=1.3),
  dnorm(1.3,mean=1.25,sd=sqrt(.85))*(1 - pnorm((.85-mu_bar)/sd_bar))
)

expect_equal(
  calc_conditional_gaussian_integral(c(1.5,1.25),
                                     cov_mat,
                                     lo=.85,
                                     hi=Inf,
                                     y_giv=1.3,
                                     log=T),
  dnorm(1.3,mean=1.25,sd=sqrt(.85),log=T) + log(1 - pnorm((.85-mu_bar)/sd_bar))
)

# Test for J=2 and K=2
mean_vect <- c(1.5,1.2,1.25,1.8)

cov_mat <- diag(c(.75,.2,.85,1.2))
cov_mat[1,2] <- sqrt(cov_mat[1,1])*sqrt(cov_mat[2,2])*  .25
cov_mat[2,1] <- cov_mat[1,2]
cov_mat[1,3] <- sqrt(cov_mat[1,1])*sqrt(cov_mat[3,3])*  .40
cov_mat[3,1] <- cov_mat[1,3]
cov_mat[1,4] <- sqrt(cov_mat[1,1])*sqrt(cov_mat[4,4])*(-.20)
cov_mat[4,1] <- cov_mat[1,4]
cov_mat[2,4] <- sqrt(cov_mat[1,1])*sqrt(cov_mat[3,3])*(-.10)
cov_mat[2,4] <- cov_mat[4,2]

y_giv <- c(1.3,1.77)



condMean <- mean_vect[1:2] + cov_mat[1:2,3:4] %*%
                            solve(cov_mat[3:4,3:4]) %*%
                            as.matrix(y_giv - mean_vect[3:4])
condCov  <- cov_mat[1:2,1:2] - cov_mat[1:2,3:4] %*%
                              solve(cov_mat[3:4,3:4]) %*%
                              cov_mat[3:4,1:2]

# J=2 / K=2. Check integral from -Inf to .85 for j=1
lo <- c(-Inf,.42)
hi <- c(.65,.74)
logLik <- mvtnorm::dmvnorm(y_giv,mean_vect[3:4],cov_mat[3:4,3:4],log=T)
condIntegral <- mvtnorm::pmvnorm(lower=lo,
                                 upper=hi,
                                 mean=as.vector(condMean),
                                 sigma=condCov)
logLik <- logLik + log(as.numeric(condIntegral))
expect_equal(
  calc_conditional_gaussian_integral(mean_vect,
                                     cov_mat,
                                     lo=lo,
                                     hi=hi,
                                     y_giv=y_giv,
                                     log=T),
  logLik
)

expect_equal(
  calc_conditional_gaussian_integral(mean_vect,cov_mat,lo=lo,hi=hi,y_giv=y_giv),
  exp(logLik)
)

# J=2 / K=2. Check integral from .65 to .85 for j=1
lo <- c(.65,.42)
hi <- c(.85,.74)
logLik <- mvtnorm::dmvnorm(y_giv,mean_vect[3:4],cov_mat[3:4,3:4],log=T)
condIntegral <- mvtnorm::pmvnorm(lower=lo,
                                 upper=hi,
                                 mean=as.vector(condMean),
                                 sigma=condCov)
logLik <- logLik + log(as.numeric(condIntegral))
expect_equal(
  calc_conditional_gaussian_integral(mean_vect,
                                     cov_mat,
                                     lo=lo,
                                     hi=hi,
                                     y_giv=y_giv,
                                     log=T),
  logLik
)

expect_equal(
  calc_conditional_gaussian_integral(mean_vect,cov_mat,lo=lo,hi=hi,y_giv=y_giv),
  exp(logLik)
)

# J=2 / K=2. Check integral from .85 to Inf for j=1
lo <- c(.85,.42)
hi <- c(Inf,.74)
logLik <- mvtnorm::dmvnorm(y_giv,mean_vect[3:4],cov_mat[3:4,3:4],log=T)
condIntegral <- mvtnorm::pmvnorm(lower=lo,
                                 upper=hi,
                                 mean=as.vector(condMean),
                                 sigma=condCov)
logLik <- logLik + log(as.numeric(condIntegral))
expect_equal(
  calc_conditional_gaussian_integral(mean_vect,
                                     cov_mat,
                                     lo=lo,
                                     hi=hi,
                                     y_giv=y_giv,
                                     log=T),
  logLik
)

expect_equal(
  calc_conditional_gaussian_integral(mean_vect,cov_mat,lo=lo,hi=hi,y_giv=y_giv),
  exp(logLik)
)

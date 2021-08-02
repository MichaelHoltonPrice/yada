# Test yada::yada_sample_mcmc

neg_dnorm <- function(x,...) {
  return(-dnorm(x,...))
}

expect_error(
  samp <- yada_sample_mcmc(neg_dnorm,1,100,2,5e-1,mean=-.5,sd=1.5,log=T),
  NA
)

expect_equal(
  samp$theta0,
  1
)

expect_equal(
  samp$eta0,
  neg_dnorm(1,-.5,1.5,log=T)
)

expect_equal(
  samp$temp,
  2
)

expect_equal(
  length(samp$theta_best),
  1
)

expect_equal(
  length(samp$eta_best),
  1
)

expect_equal(
  length(samp$accept_vect),
  100
)

expect_equal(
  length(samp$eta_vect),
  100
)


expect_equal(
  dim(samp$theta_mat),
  c(100,1)
)

th_x <- list(fit_type = 'uniform',xmin=0,xmax=80)
N <- 100

# Test yada::yada_tailored_annealing
expect_error(
  optimOut <- yada_tailored_annealing(neg_dnorm,1,mean=-.5,sd=1.5,log=T),
  NA
)

expect_equal(
  length(optimOut$eta_best),
  1
)

expect_equal(
  length(optimOut$theta_best),
  1
)

expect_equal(
  length(optimOut$eta_vect),
  130000
)

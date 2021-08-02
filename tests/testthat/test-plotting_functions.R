library(doParallel)
registerDoParallel(detectCores())

problem <- list()
problem$x <- runif(10, 0, 12)
problem$Y <- as.matrix(cbind(seq(1:10),
                             runif(10,100,250)))
problem$var_names <- c('HME_EF','RDB')
problem$mod_spec$J <- 1
problem$mod_spec$K <- 1
problem$mod_spec$M <- c(6)

th_w_ord <- c(5.199, 12.331, 12.817, 13.230, 13.764, 14.957, 2.060)
th_w_cont <- c(0.430, 5.654, 8.523, 1.374, 0.069)

mod_spec_ord <- list(mean_spec='lin_ord', noise_spec='const',
                    J=1, M=6)
mod_spec_cont <- list(mean_spec='pow_law', noise_spec='lin_pos_int',
                     K=1, cdep_spec='indep')

# Test vis_cont_fit
expect_error(
  vis_cont_fit(problem$x, problem$Y[,2], th_w_cont, mod_spec_cont),
  NA
)

expect_error(
  vis_cont_fit(problem$x, problem$Y[,2], th_w_cont, mod_spec_cont, xlab='Age [years]', ylab='Probability'),
  NA
)

# Test vis_ord_fit
expect_error(
  vis_ord_fit(problem$x, problem$Y[,1], th_w_ord, mod_spec_ord),
  NA
)

# Test plot_x_posterior
xcalc <- seq(0, 23, by=0.01)
th_x <- list(fit_type='uniform', fit=c(0,23))

post_obj <- calc_x_posterior(problem$Y[1,1], th_x, th_w_ord, mod_spec_ord, xcalc=c(), normalize=T)

analysis <- analyze_x_posterior(post_obj$x, post_obj$density, problem$x[1])

expect_error(
  plot_x_posterior(analysis),
  NA
)

expect_error(
  plot_x_posterior(analysis, xlab='Age [years]', ylab='Density', lwd=2),
  NA
)

expect_error(
  plot_x_posterior(analysis, xlab='Age [years]', ylab='Density', col='red'),
  NA
)

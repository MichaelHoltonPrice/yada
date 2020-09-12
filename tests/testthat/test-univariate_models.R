# Test yada::get_num_var_univariate_cont
expect_equal(
  yada::get_num_var_univariate_cont('c',list(meanSpec='powLaw')),
  3
)

expect_equal(
  yada::get_num_var_univariate_cont('c',list(meanSpec=3)),
  3
)

expect_error(
  yada::get_num_var_univariate_cont('c',list(meanSpec='notAModel')),
  'Unrecognized meanSpec for a continuous variable, notAModel'
)

expect_equal(
  yada::get_num_var_univariate_cont('kappa',list(noiseSpec='const')),
  1
)

expect_equal(
  yada::get_num_var_univariate_cont('kappa',list(noiseSpec=0)),
  1
)

expect_equal(
  yada::get_num_var_univariate_cont('kappa',list(noiseSpec='lin_pos_int')),
  2
)

expect_equal(
  yada::get_num_var_univariate_cont('kappa',list(noiseSpec=1)),
  2
)

expect_equal(
  yada::get_num_var_univariate_cont('kappa',list(noiseSpec='hyperb')),
  3
)

expect_equal(
  yada::get_num_var_univariate_cont('kappa',list(noiseSpec=2)),
  3
)

expect_error(
  yada::get_num_var_univariate_cont('kappa',list(noiseSpec='notAModel')),
  'Unrecognized case, noiseSpec = notAModel'
)

expect_error(
  yada::get_num_var_univariate_cont('notAVariable',list()),
  'Unrecognized variable name for a continuous variable, notAVariable'
)

# Test yada::get_num_var_univariate_ord
expect_equal(
  yada::get_num_var_univariate_ord('b',list(meanSpec='powLawOrd')),
  1
)

expect_equal(
  yada::get_num_var_univariate_ord('b',list(meanSpec=0)),
  1
)

expect_error(
  yada::get_num_var_univariate_ord('b',list(meanSpec='notAModel')),
  'Unrecognized meanSpec for an ordinal variable, notAModel'
)

expect_equal(
  yada::get_num_var_univariate_ord('tau',list(M=3)),
  3
)

expect_equal(
  yada::get_num_var_univariate_ord('beta',list(noiseSpec='const')),
  1
)

expect_equal(
  yada::get_num_var_univariate_ord('beta',list(noiseSpec=0)),
  1
)

expect_equal(
  yada::get_num_var_univariate_ord('beta',list(noiseSpec='lin_pos_int')),
  2
)

expect_equal(
  yada::get_num_var_univariate_ord('beta',list(noiseSpec=1)),
  2
)

expect_equal(
  yada::get_num_var_univariate_ord('beta',list(noiseSpec='hyperb')),
  3
)

expect_equal(
  yada::get_num_var_univariate_ord('beta',list(noiseSpec=2)),
  3
)

expect_error(
  yada::get_num_var_univariate_ord('beta',list(noiseSpec='notAModel')),
  'Unrecognized case, noiseSpec = notAModel'
)

expect_error(
  yada::get_num_var_univariate_ord('notAVariable',list()),
  'Unrecognized variable name for an ordinal variable, notAVariable'
)

# Test yada::get_var_index_univariate_cont
expect_equal(
  yada::get_var_index_univariate_cont('c',list(meanSpec='powLaw')),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_cont('c',list(meanSpec=3)),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_cont('kappa',list(meanSpec='powLaw',noiseSpec='const')),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_cont('kappa',list(meanSpec='powLaw',noiseSpec=0)),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_cont('kappa',list(meanSpec='powLaw',noiseSpec='lin_pos_int')),
  4:5
)

expect_equal(
  yada::get_var_index_univariate_cont('kappa',list(meanSpec='powLaw',noiseSpec=1)),
  4:5
)

expect_equal(
  yada::get_var_index_univariate_cont('kappa',list(meanSpec='powLaw',noiseSpec='hyperb')),
  4:6
)

expect_equal(
  yada::get_var_index_univariate_cont('kappa',list(meanSpec='powLaw',noiseSpec=2)),
  4:6
)

expect_error(
  yada::get_var_index_univariate_cont('notAVariable',list()),
  'Unrecognized variable name for a continuous variable, notAVariable'
)

# Test yada::get_var_index_univariate_ord
expect_equal(
  yada::get_var_index_univariate_ord('b',list(meanSpec='powLawOrd')),
  1:1
)

expect_equal(
  yada::get_var_index_univariate_ord('b',list(meanSpec=0)),
  1:1
)

expect_equal(
  yada::get_var_index_univariate_ord('b',list(meanSpec='logOrd')),
  c()
)

expect_equal(
  yada::get_var_index_univariate_ord('b',list(meanSpec=1)),
  c()
)

expect_equal(
  yada::get_var_index_univariate_ord('b',list(meanSpec='linOrd')),
  c()
)

expect_equal(
  yada::get_var_index_univariate_ord('b',list(meanSpec=2)),
  c()
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(meanSpec='powLawOrd',M=3)),
  2:4
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(meanSpec=0,M=3)),
  2:4
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(meanSpec='logOrd',M=3)),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(meanSpec=1,M=3)),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(meanSpec='linOrd',M=3)),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(meanSpec=2,M=3)),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='powLawOrd',noiseSpec='const',M=3)),
  5:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='powLawOrd',noiseSpec=0,M=3)),
  5:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='powLawOrd',noiseSpec='lin_pos_int',M=3)),
  5:6
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='powLawOrd',noiseSpec=1,M=3)),
  5:6
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='powLawOrd',noiseSpec='hyperb',M=3)),
  5:7
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='powLawOrd',noiseSpec=2,M=3)),
  5:7
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='logOrd',noiseSpec='const',M=3)),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='logOrd',noiseSpec=0,M=3)),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='logOrd',noiseSpec='lin_pos_int',M=3)),
  4:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='logOrd',noiseSpec=1,M=3)),
  4:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='logOrd',noiseSpec='hyperb',M=3)),
  4:6
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='logOrd',noiseSpec=2,M=3)),
  4:6
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='linOrd',noiseSpec='const',M=3)),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='linOrd',noiseSpec=0,M=3)),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='linOrd',noiseSpec='lin_pos_int',M=3)),
  4:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='linOrd',noiseSpec=1,M=3)),
  4:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='linOrd',noiseSpec='hyperb',M=3)),
  4:6
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(meanSpec='linOrd',noiseSpec=2,M=3)),
  4:6
)

expect_error(
  yada::get_var_index_univariate_ord('notAVariable',list()),
  'Unrecognized variable name for an ordinal variable, notAVariable'
)

# Test yada::calc_noise_univariate_cont
# This is a wrapper to call calc_noise in noise_functions.R. Hence, test one 
# case only since the full functionality of calc_noise is checked in
# test-noise_functions.R.
expect_equal(
  yada::calc_noise_univariate_cont(c(1,1.5),c(0.5,0.2,-.4,0.25,0.10),list(meanSpec='powLaw',noiseSpec='lin_pos_int')),
  0.25*(1 + c(1,1.5)*0.10)
)

# Test yada::calc_noise_univariate_ord
# This is a wrapper to call calc_noise in noise_functions.R. Hence, test one 
# case only since the full functionality of calc_noise is checked in
# test-noise_functions.R.
expect_equal(
  yada::calc_noise_univariate_ord(c(1,1.5),c(0.5,1,2,3,0.25,0.10),list(meanSpec='powLawOrd',noiseSpec='lin_pos_int',M=3)),
  0.25*(1 + c(1,1.5)*0.10)
)

# Test yada::calc_mean_univariate_cont
# This is a wrapper to call calc_mean in mean_functions.R. Hence, test one case
# only since the full functionality of calc_mean is checked in
# test-mean_functions.R.
expect_equal(
  yada::calc_mean_univariate_cont(c(1,1.5),c(0.5,0.2,-.4,0.25,0.10),list(meanSpec='powLaw',noiseSpec='lin_pos_int')),
  0.2*c(1,1.5)^0.5 - 0.4
)

# Test yada::calc_mean_univariate_ord
# This is a wrapper to call calc_mean in mean_functions.R. Hence, test one case
# only since the full functionality of calc_mean is checked in
# test-mean_functions.R.
expect_equal(
  yada::calc_mean_univariate_ord(c(1,1.5),c(0.5,1,2,3,0.25,0.10),list(meanSpec='powLawOrd',noiseSpec='lin_pos_int',M=3)),
  c(1,1.5)^0.5
)

# test yada::calc_neg_log_lik_vect_ord and yada::calc_neg_log_lik_ord
b_powLawOrd <- 0.90
tau1        <- 11
tau2        <- 15
tau         <- c(tau1,tau2)
M           <- length(tau)
beta_const       <- c(5)
beta_lin_pos_int <- c(0.05,0.01)
beta_hyperb      <- c(0.05,0.2,-2)

# Directly calculate the log likelihood for all combinations of the mean and
# noise specifications
x1 <- 10
x2 <- 20
x3 <- 30
x <- c(x1,x2,x3)
v <- c(0,1,2)
for(meanSpec in c('powLawOrd','logOrd','linOrd')) {
  for(noiseSpec in c('const','lin_pos_int','hyperb')) {
    if(meanSpec == 'powLawOrd') {
      th_v <- b_powLawOrd
    } else {
      th_v <- c()
    }
    th_v <- c(th_v,tau)
    modSpec <- list(meanSpec=meanSpec,noiseSpec=noiseSpec,M=M)
    tfCatVect <- get_univariate_ord_transform_categories(modSpec)
    if(noiseSpec == 'const') {
      th_v <- c(th_v,beta_const)
    } else if(noiseSpec == 'lin_pos_int') {
      th_v <- c(th_v,beta_lin_pos_int)
    } else if(noiseSpec == 'hyperb') {
      th_v <- c(th_v,beta_hyperb)
    } else {
      stop('This should not happen')
    }
    th_v_bar <- param_constr2unconstr(th_v,tfCatVect)
    g     <- calc_mean_univariate_ord(x,th_v,modSpec)
    gamma <- calc_noise_univariate_ord (x,th_v,modSpec)

    eta_v1 <- -log(pnorm( (tau1 - g[1])/gamma[1] ))
    eta_v2 <- -log( pnorm( (tau2 - g[2])/gamma[2] ) - pnorm( (tau1 - g[2])/gamma[2] ))
    eta_v3 <- -log( 1 - pnorm( (tau2 - g[3])/gamma[3] ))
    eta_v  <- c(eta_v1,eta_v2,eta_v3)

    expect_equal(
      calc_neg_log_lik_vect_ord(th_v,x,v,modSpec),
      eta_v
    )

    expect_equal(
      calc_neg_log_lik_vect_ord(th_v_bar,x,v,modSpec,tfCatVect),
      eta_v
    )

    expect_equal(
      calc_neg_log_lik_ord(th_v,x,v,modSpec),
      eta_v1 + eta_v2 + eta_v3
    )

    expect_equal(
      calc_neg_log_lik_ord(th_v_bar,x,v,modSpec,tfCatVect),
      eta_v1 + eta_v2 + eta_v3
    )
  }
}

# test yada::calc_neg_log_lik_vect_cont and yada::calc_neg_log_lik_cont
c_powLaw <- c(0.45,2,1.2)
kappa_const       <- c(.5)
kappa_lin_pos_int <- c(0.05,0.01)
kappa_hyperb      <- c(0.05,0.2,-2)

# Directly calculate the log likelihood for all combinations of the mean and
# noise specifications
x <- c(1.3,2.1)
w <- c(.7,3.8)
meanSpec = 'powLaw'
for(noiseSpec in c('const','lin_pos_int','hyperb')) {
  th_w <- c_powLaw
  modSpec <- list(meanSpec=meanSpec,noiseSpec=noiseSpec)
  tfCatVect <- get_univariate_cont_transform_categories(modSpec)
  # Use same noise specifications as for the ordinal case above
  if(noiseSpec == 'const') {
    th_w <- c(th_w,kappa_const)
  } else if(noiseSpec == 'lin_pos_int') {
    th_w <- c(th_w,kappa_lin_pos_int)
  } else if(noiseSpec == 'hyperb') {
    th_w <- c(th_w,kappa_hyperb)
  } else {
    stop('This should not happen')
  }

  th_w_bar <- param_constr2unconstr(th_w,tfCatVect)
  h   <- calc_mean_univariate_cont(x,th_w,modSpec)
  psi <- calc_noise_univariate_cont (x,th_w,modSpec)

  eta_w <- log(sqrt(2*pi)) + log(psi) + 0.5*((w-h)/psi)^2

  expect_equal(
    calc_neg_log_lik_vect_cont(th_w,x,w,modSpec),
    eta_w
  )

  expect_equal(
    calc_neg_log_lik_vect_cont(th_w_bar,x,w,modSpec,tfCatVect),
    eta_w
  )

  expect_equal(
    calc_neg_log_lik_cont(th_w,x,w,modSpec),
    sum(eta_w)
  )

  expect_equal(
    calc_neg_log_lik_cont(th_w_bar,x,w,modSpec,tfCatVect),
    sum(eta_w)
  )
}













# test yada::sim_univariate_cont, yada::init_univariate_ord, and yada::fit_univariate_cont
# A uniform prior on x on the interval 0 to 80
th_x <- list(fitType = 'uniform',xmin=0,xmax=80)
N <- 100
set.seed(180190)

for(meanSpec in c('powLawOrd','logOrd','linOrd')) {
  for(noiseSpec in c('const','lin_pos_int','hyperb')) {
    if(meanSpec == 'powLawOrd') {
      th_v <- b_powLawOrd
    } else {
      th_v <- c()
    }
    th_v <- c(th_v,tau)
    modSpec <- list(meanSpec=meanSpec,noiseSpec=noiseSpec,M=M)
    tfCatVect <- get_univariate_ord_transform_categories(modSpec)
    if(noiseSpec == 'const') {
      th_v <- c(th_v,beta_const)
    } else if(noiseSpec == 'lin_pos_int') {
      th_v <- c(th_v,beta_lin_pos_int)
    } else if(noiseSpec == 'hyperb') {
      th_v <- c(th_v,beta_hyperb)
    } else {
      stop('This should not happen')
    }

    expect_error(
      sim <- sim_univariate_ord(th_v,modSpec,N,th_x),
      NA
    )

    expect_equal(
      names(sim),
      c('x','v','vstar')
    )

    expect_equal(
      length(sim$x),
      N
    )

    expect_equal(
      length(sim$v),
      N
    )

    expect_equal(
      length(sim$vstar),
      N
    )

    expect_error(
      sim <- sim_univariate_ord(th_v,modSpec,x=sim$x),
      NA
    )

    expect_equal(
      names(sim),
      c('x','v','vstar')
    )

    expect_equal(
      length(sim$x),
      N
    )

    expect_equal(
      length(sim$v),
      N
    )

    expect_equal(
      length(sim$vstar),
      N
    )

    expect_error(
      th_v0 <- init_univariate_ord(sim$x,sim$v,modSpec),
      NA
    )

    expect_equal(
      any(is.na(th_v0)),
      F
    )

    expect_equal(
      length(th_v0),
      length(th_v)
    )

    expect_error(
      th_v_fit <- fit_univariate_ord(sim$x,sim$v,modSpec),
      NA
    )

    expect_equal(
      any(is.na(th_v_fit)),
      F
    )

    expect_equal(
      length(th_v_fit),
      length(th_v)
    )
  }
}

# test yada::sim_univariate_cont and yada::fit_univariate_cont
meanSpec = 'powLaw'

for(noiseSpec in c('const','lin_pos_int','hyperb')) {
  th_w <- c_powLaw
  modSpec <- list(meanSpec=meanSpec,noiseSpec=noiseSpec)
  # Use same noise specifications as for the ordinal case above
  if(noiseSpec == 'const') {
    th_w <- c(th_w,kappa_const)
  } else if(noiseSpec == 'lin_pos_int') {
    th_w <- c(th_w,kappa_lin_pos_int)
  } else if(noiseSpec == 'hyperb') {
    th_w <- c(th_w,kappa_hyperb)
  } else {
    stop('This should not happen')
  }

  expect_error(
    sim <- sim_univariate_cont(th_w,modSpec,N,th_x),
    NA
  )

  expect_equal(
    names(sim),
    c('x','w')
  )

  expect_equal(
    length(sim$x),
    N
  )

  expect_equal(
    length(sim$w),
    N
  )

  expect_error(
    sim <- sim_univariate_cont(th_w,modSpec,x=sim$x),
    NA
  )

  expect_equal(
    names(sim),
    c('x','w')
  )

  expect_equal(
    length(sim$x),
    N
  )

  expect_equal(
    length(sim$w),
    N
  )

  expect_error(
    th_w_fit <- fit_univariate_cont(sim$x,sim$w,modSpec),
    NA
  )

  expect_equal(
    length(th_w_fit),
    length(th_w)
  )
}

# Test yada::get_univariate_cont_transform_matrix
expect_equal(
  yada::get_univariate_cont_transform_categories(list(meanSpec='powLaw',noiseSpec='const')),
  c(1,1,0,1)
)

expect_equal(
  yada::get_univariate_cont_transform_categories(list(meanSpec='powLaw',noiseSpec='lin_pos_int')),
  c(1,1,0,1,1)
)

expect_equal(
  yada::get_univariate_cont_transform_categories(list(meanSpec='powLaw',noiseSpec='hyperb')),
  c(1,1,0,1,1,0)
)

# Test yada::get_univariate_ord_transform_matrix
for(meanSpec in c('powLawOrd','logOrd','linOrd')) {
  if(meanSpec == 'powLawOrd') {
    catVectMean <- 1
  } else if(meanSpec == 'logOrd') {
    catVectMean <- c()
  } else if(meanSpec == 'linOrd') {
    catVectMean <- c()
  } else {
    stop('This should not happen')
  }
  for(M in 1:3) {
    catVectTau <- c(0,rep(3,M-1))
    for(noiseSpec in c('const','lin_pos_int','hyperb')) {
      if(noiseSpec == 'const') {
        catVectNoise <- 1
      } else if(noiseSpec == 'lin_pos_int') {
        catVectNoise <- c(1,1)
      } else if(noiseSpec == 'hyperb') {
        catVectNoise <- c(1,1,0)
      } else {
        stop('This should not happen')
      }
      catVect <- c(catVectMean,catVectTau,catVectNoise)
      modSpec <- list(meanSpec=meanSpec,noiseSpec=noiseSpec,M=M)
      expect_equal(
        get_univariate_ord_transform_categories(modSpec),
        catVect
      )
    }
  }
}

# TODO:
# get_approx_x_range_ord
# get_approx_x_range_cont

# Test yada::get_num_var_univariate_ord
expect_equal(
  yada::get_num_var_univariate_ord('b',list(mean_spec='pow_law_ord')),
  1
)

expect_equal(
  yada::get_num_var_univariate_ord('b',list(mean_spec=0)),
  1
)

expect_error(
  yada::get_num_var_univariate_ord('b',list(mean_spec='not_a_model')),
  'Unrecognized mean_spec for an ordinal variable, not_a_model'
)

expect_equal(
  yada::get_num_var_univariate_ord('tau',list(M=3)),
  3
)

expect_equal(
  yada::get_num_var_univariate_ord('beta',list(noise_spec='const')),
  1
)

expect_equal(
  yada::get_num_var_univariate_ord('beta',list(noise_spec=0)),
  1
)

expect_equal(
  yada::get_num_var_univariate_ord('beta',list(noise_spec='lin_pos_int')),
  2
)

expect_equal(
  yada::get_num_var_univariate_ord('beta',list(noise_spec=1)),
  2
)

expect_error(
  yada::get_num_var_univariate_ord('beta',list(noise_spec='not_a_model')),
  'Unrecognized case, noise_spec = not_a_model'
)

expect_error(
  yada::get_num_var_univariate_ord('not_a_variable',list()),
  'Unrecognized variable name for an ordinal variable, not_a_variable'
)

# Test yada::get_var_index_univariate_ord
expect_equal(
  yada::get_var_index_univariate_ord('b',list(mean_spec='pow_law_ord')),
  1:1
)

expect_equal(
  yada::get_var_index_univariate_ord('b',list(mean_spec=0)),
  1:1
)

expect_equal(
  yada::get_var_index_univariate_ord('b',list(mean_spec='log_ord')),
  c()
)

expect_equal(
  yada::get_var_index_univariate_ord('b',list(mean_spec=1)),
  c()
)

expect_equal(
  yada::get_var_index_univariate_ord('b',list(mean_spec='lin_ord')),
  c()
)

expect_equal(
  yada::get_var_index_univariate_ord('b',list(mean_spec=2)),
  c()
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(mean_spec='pow_law_ord',M=3)),
  2:4
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(mean_spec=0,M=3)),
  2:4
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(mean_spec='log_ord',M=3)),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(mean_spec=1,M=3)),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(mean_spec='lin_ord',M=3)),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_ord('tau',list(mean_spec=2,M=3)),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='pow_law_ord',
                                                 noise_spec='const',
                                                 M=3)),
  5:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='pow_law_ord',
                                                 noise_spec=0,
                                                 M=3)),
  5:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='pow_law_ord',
                                                 noise_spec='lin_pos_int',
                                                 M=3)),
  5:6
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='pow_law_ord',
                                                 noise_spec=1,
                                                 M=3)),
  5:6
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='log_ord',
                                                 noise_spec='const',
                                                 M=3)),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='log_ord',
                                                 noise_spec=0,
                                                 M=3)),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='log_ord',
                                                 noise_spec='lin_pos_int',
                                                 M=3)),
  4:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='log_ord',
                                                 noise_spec=1,
                                                 M=3)),
  4:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='lin_ord',
                                                 noise_spec='const',
                                                 M=3)),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='lin_ord',
                                                 noise_spec=0,
                                                 M=3)),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='lin_ord',
                                                 noise_spec='lin_pos_int',
                                                 M=3)),
  4:5
)

expect_equal(
  yada::get_var_index_univariate_ord('beta',list(mean_spec='lin_ord',
                                                 noise_spec=1,
                                                 M=3)),
  4:5
)

expect_error(
  yada::get_var_index_univariate_ord('not_a_variable',list()),
  'Unrecognized variable name for an ordinal variable, not_a_variable'
)
# Test yada::get_num_var_univariate_cont
expect_equal(
  yada::get_num_var_univariate_cont('c',list(mean_spec='pow_law')),
  3
)

expect_equal(
  yada::get_num_var_univariate_cont('c',list(mean_spec=3)),
  3
)

expect_error(
  yada::get_num_var_univariate_cont('c',list(mean_spec='not_a_model')),
  'Unrecognized mean_spec for a continuous variable, not_a_model'
)

expect_equal(
  yada::get_num_var_univariate_cont('kappa',list(noise_spec='const')),
  1
)

expect_equal(
  yada::get_num_var_univariate_cont('kappa',list(noise_spec=0)),
  1
)

expect_equal(
  yada::get_num_var_univariate_cont('kappa',list(noise_spec='lin_pos_int')),
  2
)

expect_equal(
  yada::get_num_var_univariate_cont('kappa',list(noise_spec=1)),
  2
)

expect_error(
  yada::get_num_var_univariate_cont('kappa',list(noise_spec='not_a_model')),
  'Unrecognized case, noise_spec = not_a_model'
)

expect_error(
  yada::get_num_var_univariate_cont('not_a_variable',list()),
  'Unrecognized variable name for a continuous variable, not_a_variable'
)

# Test yada::get_var_index_univariate_cont
expect_equal(
  yada::get_var_index_univariate_cont('c',list(mean_spec='pow_law')),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_cont('c',list(mean_spec=3)),
  1:3
)

expect_equal(
  yada::get_var_index_univariate_cont('kappa',list(mean_spec='pow_law',
                                                   noise_spec='const')),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_cont('kappa',list(mean_spec='pow_law',
                                                   noise_spec=0)),
  4:4
)

expect_equal(
  yada::get_var_index_univariate_cont('kappa',list(mean_spec='pow_law',
                                                   noise_spec='lin_pos_int')),
  4:5
)

expect_equal(
  yada::get_var_index_univariate_cont('kappa',list(mean_spec='pow_law',
                                                   noise_spec=1)),
  4:5
)

expect_error(
  yada::get_var_index_univariate_cont('not_a_variable',list()),
  'Unrecognized variable name for a continuous variable, not_a_variable'
)

# Test yada::calc_noise_univariate_ord
# This is a wrapper to call calc_noise in noise_calculations.R. Hence, test one
# case only since the full functionality of calc_noise is checked in
# test-noise_calculations.R.
expect_equal(
  yada::calc_noise_univariate_ord(c(1,1.5),
                                  c(0.5,1,2,3,0.25,0.10),
                                  list(mean_spec='pow_law_ord',
                                       noise_spec='lin_pos_int',
                                       M=3)),
  0.25*(1 + c(1,1.5)*0.10)
)

# Test yada::calc_noise_univariate_cont
# This is a wrapper to call calc_noise in noise_calculations.R. Hence, test one
# case only since the full functionality of calc_noise is checked in
# test-noise_calculations.R.
expect_equal(
  yada::calc_noise_univariate_cont(c(1,1.5),
                                   c(0.5,0.2,-.4,0.25,0.10),
                                   list(mean_spec='pow_law',
                                        noise_spec='lin_pos_int')),
  0.25*(1 + c(1,1.5)*0.10)
)

# Test yada::calc_mean_univariate_ord
# This is a wrapper to call calc_mean in mean_calculations.R. Hence, test one
# case only since the full functionality of calc_mean is checked in
# test-mean_calculations.R.
expect_equal(
  yada::calc_mean_univariate_ord(c(1,1.5),
                                 c(0.5,1,2,3,0.25,0.10),
                                 list(mean_spec='pow_law_ord',
                                      noise_spec='lin_pos_int',
                                      M=3)),
  c(1,1.5)^0.5
)

# Test yada::calc_mean_univariate_cont
# This is a wrapper to call calc_mean in mean_calculations.R. Hence, test one
# case only since the full functionality of calc_mean is checked in
# test-mean_calculations.R.
expect_equal(
  yada::calc_mean_univariate_cont(c(1,1.5),
                                  c(0.5,0.2,-.4,0.25,0.10),
                                  list(mean_spec='pow_law',
                                       noise_spec='lin_pos_int')),
  0.2*c(1,1.5)^0.5 - 0.4
)

# test yada::calc_neg_log_lik_vect_ord and yada::calc_neg_log_lik_ord
b_pow_law_ord      <- 0.4
tau1             <- 2.5
tau2             <- 4.5
tau              <- c(tau1,tau2)
M                <- 2
beta_const       <- c(.5)
beta_lin_pos_int <- c(.4, (.6/.4 - 1)/80)
lin_ord_rescale <- 10

# Directly calculate the log likelihood for all combinations of the mean and
# noise specifications
x1 <- 10
x2 <- 20
x3 <- 30
x <- c(x1,x2,x3)
v <- c(0,1,2)
for(mean_spec in c('pow_law_ord','log_ord','lin_ord')) {
  for(noise_spec in c('const','lin_pos_int')) {
    if(mean_spec == 'pow_law_ord') {
      th_v <- c(b_pow_law_ord,tau)
    } else if(mean_spec == 'log_ord') {
      th_v <- tau
    } else if(mean_spec == 'lin_ord') {
      th_v <- tau*lin_ord_rescale
    } else {
      stop('This should not happen')
    }
    mod_spec <- list(mean_spec=mean_spec,noise_spec=noise_spec,M=M)
    tf_cat_vect <- get_univariate_ord_transform_categories(mod_spec)
    if(noise_spec == 'const') {
      th_v <- c(th_v,beta_const)
    } else if(noise_spec == 'lin_pos_int') {
      th_v <- c(th_v,beta_lin_pos_int)
    } else {
      stop('This should not happen')
    }

    if(mean_spec == 'lin_ord') {
      ind_beta <- get_var_index_univariate_ord('beta',mod_spec)
      th_v[ind_beta] <- th_v[ind_beta]*lin_ord_rescale
    }

    # tau is modified for the lin_ord case, so extract the actual tau used
    tau_calc <- th_v[get_var_index_univariate_ord('tau',mod_spec)]

    th_v_bar <- param_constr_to_unconstr(th_v,tf_cat_vect)
    g     <- calc_mean_univariate_ord(x,th_v,mod_spec)
    gamma <- calc_noise_univariate_ord (x,th_v,mod_spec)

    eta_v1 <- -log(pnorm( (tau_calc[1] - g[1])/gamma[1] ))
    eta_v2 <- -log( pnorm( (tau_calc[2] - g[2])/gamma[2] ) -
                      pnorm( (tau_calc[1] - g[2])/gamma[2] ))
    eta_v3 <- -log( 1 - pnorm( (tau_calc[2] - g[3])/gamma[3] ))
    eta_v  <- c(eta_v1,eta_v2,eta_v3)

    expect_equal(
      calc_neg_log_lik_vect_ord(th_v,x,v,mod_spec),
      eta_v
    )

    expect_equal(
      calc_neg_log_lik_vect_ord(th_v_bar,x,v,mod_spec,tf_cat_vect),
      eta_v
    )

    expect_equal(
      calc_neg_log_lik_ord(th_v,x,v,mod_spec),
      eta_v1 + eta_v2 + eta_v3
    )

    expect_equal(
      calc_neg_log_lik_ord(th_v_bar,x,v,mod_spec,tf_cat_vect),
      eta_v1 + eta_v2 + eta_v3
    )
  }
}

# test yada::calc_neg_log_lik_vect_cont and yada::calc_neg_log_lik_cont
c_pow_law <- c(0.45,2,1.2)
kappa_const       <- c(.5)
kappa_lin_pos_int <- c(0.05,0.01)

# Directly calculate the log likelihood for all combinations of the mean and
# noise specifications
x <- c(1.3,2.1)
w <- c(.7,3.8)
mean_spec = 'pow_law'
for(noise_spec in c('const','lin_pos_int')) {
  th_w <- c_pow_law
  mod_spec <- list(mean_spec=mean_spec,noise_spec=noise_spec)
  tf_cat_vect <- get_univariate_cont_transform_categories(mod_spec)
  # Use same noise specifications as for the ordinal case above
  if(noise_spec == 'const') {
    th_w <- c(th_w,kappa_const)
  } else if(noise_spec == 'lin_pos_int') {
    th_w <- c(th_w,kappa_lin_pos_int)
  } else {
    stop('This should not happen')
  }

  th_w_bar <- param_constr_to_unconstr(th_w,tf_cat_vect)
  h   <- calc_mean_univariate_cont(x,th_w,mod_spec)
  psi <- calc_noise_univariate_cont (x,th_w,mod_spec)

  eta_w <- log(sqrt(2*pi)) + log(psi) + 0.5*((w-h)/psi)^2

  expect_equal(
    calc_neg_log_lik_vect_cont(th_w,x,w,mod_spec),
    eta_w
  )

  expect_equal(
    calc_neg_log_lik_vect_cont(th_w_bar,x,w,mod_spec,tf_cat_vect),
    eta_w
  )

  expect_equal(
    calc_neg_log_lik_cont(th_w,x,w,mod_spec),
    sum(eta_w)
  )

  expect_equal(
    calc_neg_log_lik_cont(th_w_bar,x,w,mod_spec,tf_cat_vect),
    sum(eta_w)
  )
}

# test yada::sim_univariate_ord, yada::init_univariate_ord,
# yada::fit_univariate_ord, and yada::get_approx_x_range_ord
# A uniform prior on x on the interval 0 to 80
th_x <- list(fit_type = 'uniform',xmin=0,xmax=80)
N <- 100
set.seed(180190)
for(mean_spec in c('pow_law_ord','log_ord','lin_ord')) {
  for(noise_spec in c('const','lin_pos_int')) {
    if(mean_spec == 'pow_law_ord') {
      th_v <- c(b_pow_law_ord,tau)
    } else if(mean_spec == 'log_ord') {
      th_v <- tau
    } else if(mean_spec == 'lin_ord') {
      th_v <- tau*lin_ord_rescale
    } else {
      stop('This should not happen')
    }
    mod_spec <- list(mean_spec=mean_spec,noise_spec=noise_spec,M=M)
    tf_cat_vect <- get_univariate_ord_transform_categories(mod_spec)
    if(noise_spec == 'const') {
      th_v <- c(th_v,beta_const)
    } else if(noise_spec == 'lin_pos_int') {
      th_v <- c(th_v,beta_lin_pos_int)
    } else {
      stop('This should not happen')
    }

    expect_error(
      sim <- sim_univariate_ord(th_v,mod_spec,N,th_x),
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
      sim <- sim_univariate_ord(th_v,mod_spec,x=sim$x),
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
      th_v0 <- init_univariate_ord(sim$x,sim$v,mod_spec),
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
      th_v0_4000a <- init_univariate_ord(sim$x,sim$v,mod_spec,anneal_seed=4000),
      NA
    )

    expect_equal(
      any(is.na(th_v0_4000a)),
      F
    )

    expect_equal(
      length(th_v0_4000a),
      length(th_v)
    )

    expect_error(
      th_v0_4000b <- init_univariate_ord(sim$x,sim$v,mod_spec,anneal_seed=4000),
      NA
    )

    expect_equal(
      th_v0_4000a,
      th_v0_4000b
    )

    expect_error(
      th_v0_4001 <- init_univariate_ord(sim$x,sim$v,mod_spec,anneal_seed=4001),
      NA
    )

    expect_equal(
      all(th_v0_4000a == th_v0_4001),
      FALSE
    )

    expect_error(
      th_v0 <- init_univariate_ord(sim$x,sim$v,mod_spec,anneal=F),
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
      th_v_fit <- fit_univariate_ord(sim$x,sim$v,mod_spec),
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

    expect_error(
      th_v_fit_5000a <- fit_univariate_ord(sim$x,sim$v,mod_spec,
                                           anneal_seed=5000),
      NA
    )

    expect_equal(
      any(is.na(th_v_fit_5000a)),
      F
    )

    expect_equal(
      length(th_v_fit_5000a),
      length(th_v)
    )

    expect_error(
      th_v_fit_5000b <- fit_univariate_ord(sim$x,sim$v,mod_spec,
                                           anneal_seed=5000),
      NA
    )

    expect_equal(
      th_v_fit_5000a,
      th_v_fit_5000b
    )

    expect_error(
      th_v_fit_5001 <- fit_univariate_ord(sim$x,sim$v,mod_spec,
                                          anneal_seed=5001),
      NA
    )

    expect_equal(
      all(th_v_fit_5000a == th_v_fit_5001),
      FALSE
    )

    expect_error(
      th_v_fit <- fit_univariate_ord(sim$x,sim$v,mod_spec,anneal=F),
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

    expect_error(
      x_range  <- get_approx_x_range_ord(sim$v[1],th_v_fit,mod_spec),
      NA
    )

    expect_equal(
      length(x_range),
      2
    )

    expect_equal(
      any(is.na(x_range)),
      FALSE
    )

    expect_error(
      x_range  <- get_approx_x_range_ord(sim$v,th_v_fit,mod_spec),
      "v should be a single value, not a vector"
    )
    bad_mod_spec <- mod_spec
    bad_mod_spec$mean_spec <- "bad_spec"
    expect_error(
      x_range  <- get_approx_x_range_ord(sim$v[1],th_v_fit,bad_mod_spec),
      "Unrecognized mean_spec for an ordinal variable, bad_spec"
    )
  }
}

# test yada::sim_univariate_cont and yada::fit_univariate_cont
mean_spec = 'pow_law'

for(noise_spec in c('const','lin_pos_int')) {
  th_w <- c_pow_law
  mod_spec <- list(mean_spec=mean_spec,noise_spec=noise_spec)
  # Use same noise specifications as for the ordinal case above
  if(noise_spec == 'const') {
    th_w <- c(th_w,kappa_const)
  } else if(noise_spec == 'lin_pos_int') {
    th_w <- c(th_w,kappa_lin_pos_int)
  } else {
    stop('This should not happen')
  }

  expect_error(
    sim <- sim_univariate_cont(th_w,mod_spec,N,th_x),
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
    sim <- sim_univariate_cont(th_w,mod_spec,x=sim$x),
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
    th_w_fit <- fit_univariate_cont(sim$x,sim$w,mod_spec),
    NA
  )

  expect_equal(
    length(th_w_fit),
    length(th_w)
  )

  expect_error(
    x_range  <- get_approx_x_range_cont(sim$w[1],th_w_fit,mod_spec),
    NA
  )

  expect_equal(
    length(x_range),
    2
  )

  expect_equal(
    any(is.na(x_range)),
    FALSE
  )

  expect_error(
    x_range  <- get_approx_x_range_cont(sim$w,th_w_fit,mod_spec),
    "w should be a single value, not a vector"
  )
  bad_mod_spec <- mod_spec
  bad_mod_spec$mean_spec <- "bad_spec"
  expect_error(
    x_range  <- get_approx_x_range_cont(sim$w[1],th_w_fit,bad_mod_spec),
    "Unrecognized mean_spec for a continuous variable, bad_spec"
  )
}

# Test yada::get_univariate_ord_transform_categories
for(mean_spec in c('pow_law_ord','log_ord','lin_ord')) {
  if(mean_spec == 'pow_law_ord') {
    catVectMean <- 1
  } else if(mean_spec == 'log_ord') {
    catVectMean <- c()
  } else if(mean_spec == 'lin_ord') {
    catVectMean <- c()
  } else {
    stop('This should not happen')
  }
  for(M in 1:3) {
    catVectTau <- c(0,rep(3,M-1))
    for(noise_spec in c('const','lin_pos_int')) {
      if(noise_spec == 'const') {
        catVectNoise <- 1
      } else if(noise_spec == 'lin_pos_int') {
        catVectNoise <- c(1,1)
      } else {
        stop('This should not happen')
      }
      catVect <- c(catVectMean,catVectTau,catVectNoise)
      mod_spec <- list(mean_spec=mean_spec,noise_spec=noise_spec,M=M)
      expect_equal(
        get_univariate_ord_transform_categories(mod_spec),
        catVect
      )
    }
  }
}

# Test yada::get_univariate_cont_transform_categories
expect_equal(
  yada::get_univariate_cont_transform_categories(list(mean_spec='pow_law',
                                                      noise_spec='const')),
  c(1,1,0,1)
)

expect_equal(
  yada::get_univariate_cont_transform_categories(list(mean_spec='pow_law',
                                                      noise_spec='lin_pos_int')),
  c(1,1,0,1,1)
)

# Test yada::calc_q
# Test every combination of the mean specification, noise speficiation, and
# response value
x <- seq(0,40,by=.1)
v <- c(0,1,2)
M <- 2
for (n in 1:length(v)) {
  m <- v[n]
  for(mean_spec in c('pow_law_ord','log_ord','lin_ord')) {
    for(noise_spec in c('const','lin_pos_int')) {
      if(mean_spec == 'pow_law_ord') {
        th_v <- c(b_pow_law_ord,tau)
      } else if(mean_spec == 'log_ord') {
        th_v <- tau
      } else if(mean_spec == 'lin_ord') {
        th_v <- tau*lin_ord_rescale
      } else {
        stop('This should not happen')
      }
      mod_spec <- list(mean_spec=mean_spec,noise_spec=noise_spec,M=M)
      tf_cat_vect <- get_univariate_ord_transform_categories(mod_spec)
      if(noise_spec == 'const') {
        th_v <- c(th_v,beta_const)
      } else if(noise_spec == 'lin_pos_int') {
        th_v <- c(th_v,beta_lin_pos_int)
      } else {
        stop('This should not happen')
      }

      if(mean_spec == 'lin_ord') {
        ind_beta <- get_var_index_univariate_ord('beta',mod_spec)
        th_v[ind_beta] <- th_v[ind_beta]*lin_ord_rescale
      }

      # tau is modified for the lin_ord case, so extract the actual tau used
      tau_calc <- th_v[get_var_index_univariate_ord('tau',mod_spec)]

      expect_equal(
        calc_q(x,th_v,m,mod_spec),
        exp(-calc_neg_log_lik_vect_ord(th_v,x,rep(m,length(x)),mod_spec))
      )
    }
  }
}

# Test yada::calc_bin_bounds
x <- c(0, 0.5,0.75, 1, 1.5, 4, 5)
v <- c(0,   1,   1, 2,   2, 1, 2)

expect_error(
  calc_bin_prob(x, v),
  NA
)

expect_error(
  bin_lists <- calc_bin_prob(x, v, bin_bounds = c(0,1, 5)),
  NA
)

expect_equal(
  bin_lists$bin_centers,
  c(0.5, 3)
)

expect_equal(
  bin_lists$bin_counts,
  matrix(c(1,0,2,1,0,3),ncol=3)
)

data_dir <- tempdir()
clear_temp_dir()
analysis_name <- 'US-analysis'

## Simulate main problem
problem <- list()
problem$x <- c(17.47, 0.37, 1.15, 8.93, 20.64, 19.62, 14.80, 7.12, 0.52, 15.44)
problem$Y <- as.matrix(rbind(c(6, 1, 3, 1, NA, 6, 5, 1, 1, 3),
                             c(NA, 103, 381, 299, NA, NA, NA, NA, 108, NA)))
problem$var_names <- c('FH_EF','FDL')
problem$mod_spec$J <- 1
problem$mod_spec$K <- 1
problem$mod_spec$M <- c(6)

saveRDS(problem, build_file_path(data_dir, analysis_name, "main_problem"))

## Simulate test samples
test1 <- list(x=c(20.83, 15.55, 0.03, 1.07, 19.25, 0.22, 20.38, 0.27, 16.90, 17.07),
             Y = as.matrix(rbind(c(5, NA, 0, 1, 6, 0, 5, 1, 5, 6),
                                  c(NA, NA, 74, 140, NA, 91, NA, 92, NA, NA))))
saveRDS(test1, build_file_path(data_dir, analysis_name, "test_problem", fold=1))

test2 <- list(x=c(1.52, 14.94, 0.10, 19.50, 1.49, 1.07, 12.35, 19.06, 15.02, 16.73),
              Y = as.matrix(rbind(c(1, 5, 0, NA, 1, 1, 3, 4, 6, NA),
                                   c(161, NA, 77, NA, 155, 133, 411, NA, NA, NA))))
saveRDS(test2, build_file_path(data_dir, analysis_name, "test_problem", fold=2))

# For the continuous variable, add one main solution and two fold solutions
# for each mean_spec / noise_spec pairing (=6). Do this with a for loop, where
# the parameter vector is obtained from the following list.
cont_theta_y_list <- list(
  pow_law=list(
    const=list(
      main =c(0.61, 66.83, 64.29, 13.94),
      fold1=c(0.60, 68.14, 63.32, 13.73),
      fold2=c(0.59, 67.86, 63.77, 13.84)
    ),
    lin_pos_int=list(
      main =c(0.61, 65.94, 64.88, 7.70, 0.19),
      fold1=c(0.60, 67.55, 63.68, 7.87, 0.18),
      fold2=c(0.61, 66.72, 64.58, 7.30, 0.22)
    )
  )
)

for (mean_spec in "pow_law") {
  for (noise_spec in c("const","lin_pos_int")) {
    for(run in c("main", "fold1", "fold2")) {
      if (run == "main") {
        fold <- NA
      } else if(run == "fold1") {
        fold <- 1
      } else {
        # fold2
        fold <- 2
      }
      file_path <- build_file_path(data_dir,
                                   analysis_name,
                                   "univariate_cont_soln",
                               k=1,
                               var_name="FDL",
                               mean_spec=mean_spec,
                               noise_spec=noise_spec,
                               fold=fold)
      th_y <- cont_theta_y_list[[mean_spec]][[noise_spec]][[run]]
      solutiony <- list(th_y=th_y,
                        mod_spec = list(
                          mean_spec = mean_spec,
                          noise_spec = noise_spec,
                          K = 1,
                          cdep_spec = 'indep'
                        ))
      saveRDS(solutiony, file_path)
    }
  }
}

# For the ordinal variable, add one main solution and two fold solutions
# for each mean_spec / noise_spec pairing (=18). Do this with a for loop, where
# the parameter vector is obtained from the following list.
ord_theta_y_list <- list(
  pow_law_ord=list(
    const=list(
      main =c(0.28, 0.75, 1.94, 2.00, 2.04, 2.10, 2.20, 0.11),
      fold1=c(0.31, 0.73, 2.05, 2.14, 2.18, 2.25, 2.37, 0.13),
      fold2=c(0.29, 0.75, 1.97, 2.03, 2.07, 2.13, 2.24, 0.12)
    ),
    lin_pos_int=list(
      main =c(0.43, 0.64, 2.79, 2.93, 3.02, 3.14, 3.39, 0.15, 0.05),
      fold1=c(0.42, 0.65, 2.70, 2.86, 2.93, 3.05, 3.28, 0.16, 0.04),
      fold2=c(0.49, 0.62, 3.22, 3.40, 3.49, 3.66, 4.02, 0.16, 0.08)
    )
  ),
  log_ord=list(
    const=list(
      main=c(-1.09, 2.27, 2.39, 2.46, 2.56, 2.79, 0.29),
      fold1=c(-1.13, 2.25, 2.39, 2.45, 2.56, 2.78, 0.29),
      fold2=c(-1.05, 2.28, 2.39, 2.45, 2.56, 2.81, 0.30)
    ),
    lin_pos_int=list(
      main=c(-1.09, 2.27, 2.39, 2.46, 2.57, 2.79, 0.29, 0.00),
      fold1=c(-1.13, 2.25, 2.39, 2.45, 2.56, .279, 0.30, 0.00),
      fold2=c(-1.06, 2.28, 2.39, 2.44, 2.55, 2.81, 0.30, 0.00)
    )
  ),
  lin_ord=list(
    const=list(
      main=c(0.09, 10.79, 12.23, 13.09, 14.37, 16.78, 2.43),
      fold1=c(0.15, 10.61, 12.22, 13.03, 14.33, 16.70, 2.35),
      fold2=c(-0.12, 10.92, 12.26, 12.99, 14.37, 16.95, 2.46)
    ),
    lin_pos_int=list(
      main=c(0.08, 10.79, 12.23, 13.09, 14.37, 16.78, 2.43, 0.00),
      fold1=c(0.35, 10.25, 11.76, 12.54, 13.75, 16.27, 0.14, 1.18),
      fold2=c(0.38, 10.54, 11.79, 12.47, 13.76, 16.57, 0.13, 1.25)
    )
  )
)

for (mean_spec in c("pow_law_ord", "log_ord", "lin_ord")) {
  for (noise_spec in c("const","lin_pos_int")) {
    for(run in c("main", "fold1", "fold2")) {
      if (run == "main") {
        fold <- NA
      } else if(run == "fold1") {
        fold <- 1
      } else {
        # fold2
        fold <- 2
      }
      file_path <- build_file_path(data_dir,
                                   analysis_name,
                                   "univariate_ord_soln",
                                   j=1,
                                   var_name="FH_EF",
                                   mean_spec=mean_spec,
                                   noise_spec=noise_spec,
                                   fold=fold)
      th_y <- ord_theta_y_list[[mean_spec]][[noise_spec]][[run]]
      solutiony <- list(th_y=th_y,
                        mod_spec = list(
                          mean_spec = mean_spec,
                          noise_spec = noise_spec,
                          J = 1,
                          M = 6,
                          cdep_spec = 'indep'
                        ))
      saveRDS(solutiony, file_path)
    }
  }
}

# Test fail_for_beta2
expect_true(
  fail_for_beta2(matrix(c(rep(0,10),rep(0,10),rep(4,10)),ncol=3),beta2_max=3)
)

expect_false(
  fail_for_beta2(matrix(c(rep(0,10),rep(0,10),rep(1,10)),ncol=3),beta2_max=3)
)

# Test get_num_training_problems [which does not use the file extensions]
writeLines('temp',
           file.path(data_dir,paste0('train_', analysis_name, '_fold1.txt')))
writeLines('temp',
           file.path(data_dir,paste0('train_', analysis_name, '_fold2.txt')))

expect_equal(
  get_num_training_problems(data_dir, analysis_name),
  2
)

writeLines('temp',
           file.path(data_dir,paste0('train_', analysis_name, '_fold3.txt')))

expect_equal(
  get_num_training_problems(data_dir, analysis_name),
  3
)

success <- file.remove(file.path(data_dir,paste0('train_',
                                                 analysis_name,
                                                 '_fold1.txt')))
success <- file.remove(file.path(data_dir,paste0('train_',
                                                 analysis_name,
                                                 '_fold2.txt')))
success <- file.remove(file.path(data_dir,paste0('train_',
                                                 analysis_name,
                                                 '_fold3.txt')))

# Test get_univariate_variable_cases
expect_error(
  get_univariate_variable_cases(data_dir, analysis_name, j=c(1, 2)),
  "j should be NA or a scalar"
)

expect_error(
  get_univariate_variable_cases(data_dir, analysis_name, k=c(2, 3)),
  "k should be NA or a scalar"
)

expect_error(
  get_univariate_variable_cases(data_dir, analysis_name),
  "At least one of j and k should be input"
)

expect_error(
  get_univariate_variable_cases(data_dir, analysis_name, j=1, k=1),
  "Only one of j and k should be input"
)

expect_equal(
  length(get_univariate_variable_cases(data_dir, analysis_name, j=1)),
  18
)

expect_equal(
  length(get_univariate_variable_cases(data_dir, analysis_name, k=1)),
  6
)

# Test get_num_folds
expect_equal(
  get_num_folds(data_dir, analysis_name),
  c(2,2)
)

expect_equal(
  get_num_folds(data_dir, analysis_name, j=1),
  2
)

expect_equal(
  get_num_folds(data_dir, analysis_name, k=1),
  2
)

# Test crossval_univariate_problems
solutiony_cont_pow_law_const_fold2 <-
  readRDS(build_file_path(data_dir,
                          analysis_name,
                          "univariate_cont_soln",
                          k=1,
                          var_name="FDL",
                          mean_spec="pow_law",
                          noise_spec="const",
                          fold=2))
saveRDS(solutiony_cont_pow_law_const_fold2,
        file.path(data_dir,
                  paste0('solutiony_',
                         analysis_name,
                         '_fold3_cont_k_1_FDL_pow_law_const.rds')))
expect_error(
  crossval_univariate_models(data_dir, analysis_name, 0.05, 0.1, 5),
  "All variables should have the same number of folds"
)
success <- file.remove(file.path(data_dir,
                                 paste0('solutiony_',
                                        analysis_name,
                                        '_fold3_cont_k_1_FDL_pow_law',
                                        '_const.rds')))
# TODO: consider changing input data so that NaN is not produced for model 4
#       of ordinal variable
expect_warning(
  cv_data <- crossval_univariate_models(data_dir, analysis_name, 0.05, 0.1, 5),
  'NaNs produced'
)

expect_equal(
  dim(cv_data$cv_array_ord),
  c(6, 2, 1)
)

expect_equal(
  dim(cv_data$cv_array_cont),
  c(2, 2, 1)
)

expect_equal(
  length(cv_data$ord_models),
  6
)

expect_equal(
  length(cv_data$cont_models),
  2
)

expect_equal(
  dim(cv_data$mod_select_ord[[1]]),
  c(6,6)
)

expect_equal(
  dim(cv_data$mod_select_cont[[1]]),
  c(2,6)
)

expect_equal(
  names(cv_data),
  c("cv_array_ord",
    "cv_array_cont",
    "num_folds",
    "param_list_ord",
    "param_list_cont",
    "num_obs_vect",
    "can_do_log_ord",
    "ord_models",
    "cont_models",
    "mod_select_ord",
    "mod_select_cont",
    "cand_tol",
    "scale_exp_min",
    "beta2_max")
)

# Test write_matrix
expect_error(
  write_matrix(cv_data$cv_array_ord,
               file.path(data_dir,
                         'US-analysis_ord_j_1_FH_EF.Rmd')),
  NA
)
expect_true(
  file.exists(file.path(data_dir,'US-analysis_ord_j_1_FH_EF.Rmd'))
)
success <- file.remove(file.path(data_dir,
                                 'US-analysis_ord_j_1_FH_EF.Rmd'))

expect_error(
  write_matrix(cv_data$cv_array_cont,
               file.path(data_dir,'US-analysis_cont_k_1_FDL.Rmd')),
  NA
)
expect_true(
  file.exists(file.path(data_dir,'US-analysis_cont_k_1_FDL.Rmd'))
)
success <- file.remove(file.path(data_dir,'US-analysis_cont_k_1_FDL.Rmd'))

# Test write_ordinal_report
expect_error(
  write_ordinal_report(data_dir, analysis_name, 1),
  NA
)
expect_true(
  file.exists(file.path(data_dir,'US-analysis_ord_j_1_FH_EF.Rmd'))
)
success <- file.remove(file.path(data_dir, 'US-analysis_ord_j_1_FH_EF.Rmd'))
expect_true(
  file.exists(file.path(data_dir,'US-analysis_ord_j_1_FH_EF.html'))
)
success <- file.remove(file.path(data_dir, 'US-analysis_ord_j_1_FH_EF.html'))

expect_false(
  file.exists(file.path(data_dir,'US-analysis_ord_j_5_FH_EF.Rmd'))
)

# Test write_continuous_report
expect_error(
  write_continuous_report(data_dir, analysis_name, 1),
  NA
)
expect_true(
  file.exists(file.path(data_dir,'US-analysis_cont_k_1_FDL.Rmd'))
)
success <- file.remove(file.path(data_dir,'US-analysis_cont_k_1_FDL.Rmd'))
expect_true(
  file.exists(file.path(data_dir,'US-analysis_cont_k_1_FDL.html'))
)
success <- file.remove(file.path(data_dir, 'US-analysis_cont_k_1_FDL.html'))

expect_false(
  file.exists(file.path(data_dir,'\\US-analysis_cont_k_5_FDL.html'))
)

# Test parse_joined_model
expect_error(
  parse_joined_model("pow_law_ord_bad_mean"),
  "Unrecognized joined_model = pow_law_ord_bad_mean"
)

for (mean_spec in  c("pow_law","pow_law_ord","log_ord","lin_ord")) {
  for (noise_spec in c("const","lin_pos_int")) {
    expect_equal(
      parse_joined_model(paste0(mean_spec,"_",noise_spec)),
      c(mean_spec,noise_spec)
    )
  }
}

# Test get_best_univariate_params
expect_false(
  file.exists(file.path(data_dir,
                        'US-analysis_univariate_model_parameters.rds'))
)

expect_error(
  params <- get_best_univariate_params(data_dir,
                           analysis_name,
                           save_file=TRUE),
  NA
)

expect_true(
  file.exists(file.path(data_dir,
                        'US-analysis_univariate_model_parameters.rds'))
)

success <-
  file.remove(file.path(data_dir,
                        'US-analysis_univariate_model_parameters.rds'))

expect_equal(
  dim(params),
  c(12,6)
)

expect_equal(
  params$type[1],
  "Ord"
)

expect_equal(
  params$mean_spec[1],
  "log_ord"
)

expect_equal(
  params$noise_spec[1],
  "const"
)

expect_equal(
  params$params,
  c("tau1","tau2","tau3","tau4","tau5","tau6",
    "beta1","c1","c2","c3","kappa1","kappa2")
)

expect_equal(
  length(params$param_val),
  12
)

# Test generate_mod_spec
expect_error(
  generate_mod_spec(problem, params, "dep", "FDL"),
  "cdep_spec does not match univariate model type"
)

expect_error(
  generate_mod_spec(problem, params, "indep"),
  "cdep_spec does not match multivariate model type"
)

expect_error(
  test_mod_spec <- generate_mod_spec(problem, params, "indep", "FDL"),
  NA
)

expect_equal(
  test_mod_spec$J,
  0
)

expect_equal(
  test_mod_spec$mean_spec,
  params$mean_spec[8]
)

expect_equal(
  test_mod_spec$noise_spec,
  params$noise_spec[8]
)

expect_error(
  test_mod_spec <- generate_mod_spec(problem, params, "dep"),
  NA
)

expect_equal(
  test_mod_spec$J,
  1
)

expect_equal(
  test_mod_spec$K,
  1
)

expect_equal(
  test_mod_spec$M,
  6
)

expect_equal(
  test_mod_spec$mean_spec,
  c(params$mean_spec[1], params$mean_spec[8])
)

expect_equal(
  test_mod_spec$noise_spec,
  c(params$noise_spec[1], params$noise_spec[8])
)

expect_equal(
  test_mod_spec$cdep_spec,
  "dep"
)

# Test load_best_univariate_model
expect_error(
  load_best_univariate_model(data_dir, analysis_name, "not_a_var"),
  "var_name should match exactly one entry in problem$var_names",
  fixed=T
)

expect_equal(
  load_best_univariate_model(data_dir, analysis_name, "FH_EF"),
  list(
    th_y     = params$param_val[params$var == "FH_EF"],
    mod_spec = list(J=1,
                    K=0,
                    mean_spec=params$mean_spec[1],
                    noise_spec=params$noise_spec[1],
                    M=6)
  )
)

# Removing missing data from expected data vectors
x <- problem$x
y <- problem$Y[1,]
ind_keep <- !is.na(x) & !is.na(y)
x <- x[ind_keep]
y <- y[ind_keep]
expect_equal(
  load_best_univariate_model(data_dir, analysis_name, "FH_EF", load_data=T),
  list(
    th_y     = params$param_val[params$var == "FH_EF"],
    mod_spec = list(J=1,
                    K=0,
                    mean_spec=params$mean_spec[1],
                    noise_spec=params$noise_spec[1],
                    M=6),
    x        = x,
    y        = y
  )
)

expect_equal(
  load_best_univariate_model(data_dir, analysis_name, "FDL"),
  list(
    th_y     = params$param_val[params$var == "FDL"],
    mod_spec = list(J=0,
                    K=1,
                    mean_spec=params$mean_spec[12],
                    noise_spec=params$noise_spec[12])
  )
)

# Removing missing data from expected data vectors
x <- problem$x
y <- problem$Y[2,]
ind_keep <- !is.na(x) & !is.na(y)
x <- x[ind_keep]
y <- y[ind_keep]
expect_equal(
  load_best_univariate_model(data_dir, analysis_name, "FDL", load_data=T),
  list(
    th_y     = params$param_val[params$var == "FDL"],
    mod_spec = list(J=0,
                    K=1,
                    mean_spec=params$mean_spec[12],
                    noise_spec=params$noise_spec[12]),
    x        = x,
    y        = y
  )
)

# Test generate_ord_ci
library(doParallel)
num_cores <- detectCores()
if (num_cores >= 6) {
  registerDoParallel(num_cores-4)
}

th_x <- list(fit_type="uniform",
             fit=c(0,80))

expect_error(
  ord_ci <- generate_ord_ci(data_dir, analysis_name, "FH_EF", th_x),
  NA
)
expect_equal(
  dim(ord_ci),
  c(7,6)
)

expect_error(
  ord_ci <- generate_ord_ci(data_dir,
                            analysis_name,
                            "FH_EF",
                            th_x,
                            save_file=TRUE,
                            j=2),
  NA
)
expect_equal(
  dim(ord_ci),
  c(7,6)
)

# Check functioning of the seed
expect_error(
  ord_ci_a <- generate_ord_ci(data_dir, analysis_name, "FH_EF", th_x,
                              input_seed=12),
  NA
)

expect_error(
  ord_ci_b <- generate_ord_ci(data_dir, analysis_name, "FH_EF", th_x,
                              input_seed=12),
  NA
)

expect_equal(
  ord_ci_a,
  ord_ci_b
)

# Test build_cindep_model
mod_spec <- generate_mod_spec(problem, params, "dep")
th_y <- params$param_val[c(8:10, 1:6, 7, 11:12)]
tf_cat_vect <- get_multivariate_transform_categories(mod_spec)
th_y_bar <- param_constr_to_unconstr(th_y, tf_cat_vect)
mod_spec$cdep_spec = "indep"

expect_error(
  cindep_model0 <- build_cindep_model(data_dir, analysis_name),
  NA
)

expect_equal(
  cindep_model0,
  list(
    th_y     = th_y,
    mod_spec = mod_spec,
    th_y_bar = th_y_bar
  )
)

expect_false(
  file.exists(file.path(tempdir(),"cindep_model_US-analysis.rds"))
)

# For the following tests, it's fine to use the main problem as the fold
# problem
saveRDS(problem,file.path(data_dir,paste0('train_',
                                          analysis_name,
                                          '_fold1.rds')))
expect_error(
  cindep_model2 <- build_cindep_model(data_dir, analysis_name, 
                                      fold=1, save_file=T),
  NA
)

expect_true(
  file.exists(file.path(tempdir(),"cindep_model_US-analysis_fold1.rds"))
)
success <- file.remove(file.path(tempdir(),
                                 "cindep_model_US-analysis_fold1.rds"))
success <- file.remove(file.path(data_dir,paste0('train_',
                                                 analysis_name,
                                                 '_fold1.rds')))

# Create and fit simulated data to test the functioning of the standard error
# calculation for build_cindep_model. Do so with a full simulation of the
# cross validation steps.
# TODO: check that the same convention is used for th_x throught yada
# TODO: consider making a stand-alone function for the Hessian calculation.
analysis_name <- "hessian"
th_x <- list(fit_type="uniform",xmin=0,xmax=2)

# Check with one ordinal and one continuous variable where the mean responses
# are lin_Ord and pow_law, respectively, and the noise models are const. Check
# both the normal functioning and the failures for (a) a singular Hessian and
# (b) negative values on the diagonal of the inverse Hessian.

# Simulate the data
N <- 100 # number of simulated observations
th_v_sim <- c(0.5,1.5)
mod_spec_j1 <- list(mean_spec = "lin_ord",
                    noise_spec = "const",
                    J = 1,
                    M = 1)
set.seed(400)
sim_ord <- sim_univariate_ord(th_v_sim,mod_spec_j1,N,th_x)
th_w_sim <- c(1,10,20,.5)
mod_spec_k1  <- list(mean_spec = "pow_law",
                    noise_spec = "const",
                    K = 1)
sim_cont <- sim_univariate_cont(th_w_sim,mod_spec_k1,th_x,x=sim_ord$x)
# Write the problem file
mod_spec <- list(mean_spec=c("lin_ord","pow_law"),
                 noise_spec=c("const","const"),
                 J=1,
                 K=1,
                 M=1)
problem <- list(x=sim_ord$x,
                Y=rbind(sim_ord$v,sim_cont$w),
                var_names=c("ord1","cont1"),
                mod_spec=mod_spec)
saveRDS(problem, build_file_path(data_dir, analysis_name, "main_problem"))

# Solve the problems
ord_prob_list <- build_univariate_ord_problems(data_dir, analysis_name)
ord_seeds <- 400 + 1:length(ord_prob_list)
ord_success <- rep(F,length(ord_prob_list))
for (i in 1:length(ord_prob_list)) {
  ord_success[i] <- yada::solve_ord_problem(data_dir,
                                            analysis_name,
                                            ord_prob_list[[i]],
                                            anneal_seed=ord_seeds[i])
}

# Modify the lin_pos_int solution to have a singular Hessian by making the
# solution nearly an edge solution
soln_const_file <- build_file_path(data_dir,
                         analysis_name,
                         "univariate_ord_soln",
                         j=1,
                         var_name="ord1",
                         mean_spec="lin_ord",
                         noise_spec="const")
soln_const <- readRDS(soln_const_file)
soln_lpi_file <- build_file_path(data_dir,
                         analysis_name,
                         "univariate_ord_soln",
                         j=1,
                         var_name="ord1",
                         mean_spec="lin_ord",
                         noise_spec="lin_pos_int")
soln_lpi <- readRDS(soln_lpi_file)
soln_lpi$th_y <- c(soln_const$th_y,.00000)
saveRDS(soln_lpi,soln_lpi_file)

cont_prob_list <- build_univariate_cont_problems(data_dir, analysis_name)
cont_success <- rep(F,length(cont_prob_list))
for (i in 1:length(cont_prob_list)) {
  cont_success[i] <- yada::solve_cont_problem(data_dir,
                                              analysis_name,
                                              cont_prob_list[[i]])
}

# Modify the lin_pos_int solution to not be an optimum
soln_lpi_file <- build_file_path(data_dir,
                         analysis_name,
                         "univariate_cont_soln",
                         k=1,
                         var_name="cont1",
                         mean_spec="pow_law",
                         noise_spec="lin_pos_int")
soln_lpi <- readRDS(soln_lpi_file)
soln_lpi$th_y <- c(.5,20,-10,.5,.001)
saveRDS(soln_lpi,soln_lpi_file)

# Define and write the (minimal) cross validation object (this will be modified
# for various tests
cv_data <- list()
ord_models <- c("lin_ord_const","lin_ord_lin_pos_int")
cv_data$mod_select_ord <- list()
cv_data$mod_select_ord[[1]] <-
  data.frame(model=ord_models,
             model_rank=c(1,2))
cv_data$ord_models <- ord_models
cv_data$param_list_ord <- list()

# Build param_list_ord (which is length 1 since J=1)
prob0 <- problem
j <- 1
param_list_j <- list()
for(n_mod in 1:length(ord_models)) {
  k_m <- ord_models[n_mod] # the known model
  # Load the solution.
  parsed_model <- parse_joined_model(k_m)
  soln0 <- readRDS(build_file_path(data_dir,
                                   analysis_name,
                                   "univariate_ord_soln",
                                   j=j,
                                   var_name=prob0$var_names[j],
                                   mean_spec=parsed_model[1],
                                   noise_spec=parsed_model[2]))
  mod_spec_j <- soln0$mod_spec


  num_b <- get_num_var_univariate_ord('b',mod_spec_j)
  num_tau <- get_num_var_univariate_ord('tau',mod_spec_j)
  num_beta <- get_num_var_univariate_ord('beta',mod_spec_j)
  rows <- c()
  if(num_b > 0) {
    for(n in 1:num_b) {
      rows <- c(rows,paste0('b',n))
    }
  }
  for(n in 1:num_tau) {
    rows <- c(rows,paste0('tau',n))
  }
  for(n in 1:num_beta) {
    rows <- c(rows,paste0('beta',n))
  }
  param_mat <- matrix(soln0$th_y,ncol=1)
  rownames(param_mat) <- rows
  colnames(param_mat) <- "main"
  param_list_j[[n_mod]] <- param_mat
}
cv_data$param_list_ord[[j]] <- param_list_j

cont_models <- c("pow_law_const","pow_law_lin_pos_int")
cv_data$mod_select_cont[[1]] <-
  data.frame(model=cont_models,
             model_rank=c(1,2))
cv_data$cont_models <- cont_models
cv_data$param_list_cont <- list()
# Build param_list_cont (which is length 1 since K=1)
prob0 <- problem
k <- 1
param_list_k <- list()
for(n_mod in 1:length(cont_models)) {
  k_m <- cont_models[n_mod] # the known model
  # Load the solution.
  parsed_model <- parse_joined_model(k_m)
  soln0 <- readRDS(build_file_path(data_dir,
                                   analysis_name,
                                   "univariate_cont_soln",
                                   k=k,
                                   var_name=prob0$var_names[1+k],
                                   mean_spec=parsed_model[1],
                                   noise_spec=parsed_model[2]))
  mod_spec_k <- soln0$mod_spec

  num_c <- get_num_var_univariate_cont('c',mod_spec_k)
  num_kappa <- get_num_var_univariate_cont('kappa',mod_spec_k)
  rows <- c()
  if(num_c > 0) {
    for(n in 1:num_c) {
      rows <- c(rows,paste0('c',n))
    }
  }
  for(n in 1:num_kappa) {
    rows <- c(rows,paste0('kappa',n))
  }
  param_mat <- matrix(soln0$th_y,ncol=1)
  rownames(param_mat) <- rows
  colnames(param_mat) <- "main"
  param_list_k[[n_mod]] <- param_mat
}
cv_data$param_list_cont[[k]] <- param_list_k

cv_data_file <- build_file_path(data_dir,analysis_name,"cv_data")
saveRDS(cv_data, cv_data_file)

expect_error(
  model <- build_cindep_model(data_dir,analysis_name,calc_se=T),
  NA
)

expect_equal(
  names(model),
  c("th_y","mod_spec","th_y_bar","th_y_bar_se")
)

expect_equal(
  any(is.na(model$th_y_bar_se)),
  FALSE
)

# Check singular Hessian error
cv_data$mod_select_ord[[j]]$model_rank <- c(2,1)
saveRDS(cv_data, cv_data_file)
expect_error(
  model <- build_cindep_model(data_dir,analysis_name,calc_se=T),
  "Hessians are singular for all ordinal variables"
)

# Check negative values on the diagonal error
cv_data$mod_select_ord[[j]]$model_rank <- c(1,2)
cv_data$mod_select_cont[[k]]$model_rank <- c(2,1)
saveRDS(cv_data, cv_data_file)
expect_error(
  model <- build_cindep_model(data_dir,analysis_name,calc_se=T),
  paste0("Not all diagonal entries of inv(H) are positive. Univariate ",
         "solution may be a corner solution or may may not be an optimum for ",
         "k=1 and var_name=cont1"),
  fixed=T
)
# Simulate data for which one ordinal variable is singular and another is not
# to ensure that substitution for the standard errors works (use the same
# parameter vector, th_v, and mod_spec, mod_spec_j1, but replace only one
# solution).

# Simulate the data
set.seed(410)
sim_ord2 <- sim_univariate_ord(th_v_sim,mod_spec_j1,th_x,x=sim_ord$x)
mod_spec <- list(mean_spec=c("lin_ord","lin_ord","pow_law"),
                 noise_spec=c("const","const","const"),
                 J=2,
                 K=1,
                 M=c(1,1))
problem <- list(x=sim_ord$x,
                Y=rbind(sim_ord$v,sim_ord2$v,sim_cont$w),
                var_names=c("ord1","ord2","cont1"),
                mod_spec=mod_spec)
saveRDS(problem, build_file_path(data_dir, analysis_name, "main_problem"))

# Solve the problems
ord_prob_list <- build_univariate_ord_problems(data_dir, analysis_name)
ord_seeds <- 400 + 1:length(ord_prob_list)
ord_success <- rep(F,length(ord_prob_list))
for (i in 1:length(ord_prob_list)) {
  ord_success[i] <- yada::solve_ord_problem(data_dir,
                                            analysis_name,
                                            ord_prob_list[[i]],
                                            anneal_seed=ord_seeds[i])
}

# Modify the lin_pos_int solution to have a singular Hessian for j=1 by making
# the solution nearly an edge solution
soln_const_file <- build_file_path(data_dir,
                         analysis_name,
                         "univariate_ord_soln",
                         j=1,
                         var_name="ord1",
                         mean_spec="lin_ord",
                         noise_spec="const")
soln_const <- readRDS(soln_const_file)
soln_lpi_file <- build_file_path(data_dir,
                         analysis_name,
                         "univariate_ord_soln",
                         j=1,
                         var_name="ord1",
                         mean_spec="lin_ord",
                         noise_spec="lin_pos_int")
soln_lpi <- readRDS(soln_lpi_file)
soln_lpi$th_y <- c(soln_const$th_y,.00000)
saveRDS(soln_lpi,soln_lpi_file)

cont_prob_list <- build_univariate_cont_problems(data_dir, analysis_name)
cont_success <- rep(F,length(cont_prob_list))
for (i in 1:length(cont_prob_list)) {
  cont_success[i] <- yada::solve_cont_problem(data_dir,
                                              analysis_name,
                                              cont_prob_list[[i]])
}
# Define and write the (minimal) cross validation object (this will be modified
# for various tests
cv_data <- list()
ord_models <- c("lin_ord_const","lin_ord_lin_pos_int")
cv_data$mod_select_ord <- list()
cv_data$mod_select_ord[[1]] <-
  data.frame(model=ord_models,
             model_rank=c(2,1))
cv_data$mod_select_ord[[2]] <-
  data.frame(model=ord_models,
             model_rank=c(1,2))
cv_data$ord_models <- ord_models
cv_data$param_list_ord <- list()

# Build param_list_ord (which is length 1 since J=1)
prob0 <- problem
for (j in 1:2) {
param_list_j <- list()
for(n_mod in 1:length(ord_models)) {
  k_m <- ord_models[n_mod] # the known model
  # Load the solution.
  parsed_model <- parse_joined_model(k_m)
  soln0 <- readRDS(build_file_path(data_dir,
                                   analysis_name,
                                   "univariate_ord_soln",
                                   j=j,
                                   var_name=prob0$var_names[j],
                                   mean_spec=parsed_model[1],
                                   noise_spec=parsed_model[2]))
  mod_spec_j <- soln0$mod_spec


  num_b <- get_num_var_univariate_ord('b',mod_spec_j)
  num_tau <- get_num_var_univariate_ord('tau',mod_spec_j)
  num_beta <- get_num_var_univariate_ord('beta',mod_spec_j)
  rows <- c()
  if(num_b > 0) {
    for(n in 1:num_b) {
      rows <- c(rows,paste0('b',n))
    }
  }
  for(n in 1:num_tau) {
    rows <- c(rows,paste0('tau',n))
  }
  for(n in 1:num_beta) {
    rows <- c(rows,paste0('beta',n))
  }
  param_mat <- matrix(soln0$th_y,ncol=1)
  rownames(param_mat) <- rows
  colnames(param_mat) <- "main"
  param_list_j[[n_mod]] <- param_mat
}
cv_data$param_list_ord[[j]] <- param_list_j
}

cont_models <- c("pow_law_const","pow_law_lin_pos_int")
cv_data$mod_select_cont[[1]] <-
  data.frame(model=cont_models,
             model_rank=c(1,2))
cv_data$cont_models <- cont_models
cv_data$param_list_cont <- list()
# Build param_list_cont (which is length 1 since K=1)
prob0 <- problem
k <- 1
param_list_k <- list()
for(n_mod in 1:length(cont_models)) {
  k_m <- cont_models[n_mod] # the known model
  # Load the solution.
  parsed_model <- parse_joined_model(k_m)
  soln0 <- readRDS(build_file_path(data_dir,
                                   analysis_name,
                                   "univariate_cont_soln",
                                   k=k,
                                   var_name=prob0$var_names[2+k], #J=2
                                   mean_spec=parsed_model[1],
                                   noise_spec=parsed_model[2]))
  mod_spec_k <- soln0$mod_spec

  num_c <- get_num_var_univariate_cont('c',mod_spec_k)
  num_kappa <- get_num_var_univariate_cont('kappa',mod_spec_k)
  rows <- c()
  if(num_c > 0) {
    for(n in 1:num_c) {
      rows <- c(rows,paste0('c',n))
    }
  }
  for(n in 1:num_kappa) {
    rows <- c(rows,paste0('kappa',n))
  }
  param_mat <- matrix(soln0$th_y,ncol=1)
  rownames(param_mat) <- rows
  colnames(param_mat) <- "main"
  param_list_k[[n_mod]] <- param_mat
}
cv_data$param_list_cont[[k]] <- param_list_k

cv_data_file <- build_file_path(data_dir,analysis_name,"cv_data")
saveRDS(cv_data, cv_data_file)

expect_error(
  model <- build_cindep_model(data_dir,analysis_name,calc_se=T),
  NA
)

expect_equal(
  names(model),
  c("th_y","mod_spec","th_y_bar","th_y_bar_se")
)

expect_equal(
  any(is.na(model$th_y_bar_se)),
  FALSE
)

# tau scales should all be equal (indices 4 and 5)
expect_equal(
  unique(model$th_y_bar_se[4:5]),
  model$th_y_bar_se[4]
)

# ordinal noise scales should all be equal (indices 6:8)
expect_equal(
  unique(model$th_y_bar_se[6:8]),
  model$th_y_bar_se[6]
)

# TODO: add tests for crossval_multivariate_models. We may adopt a different
#       approach in the near future, so wait to make the tests.
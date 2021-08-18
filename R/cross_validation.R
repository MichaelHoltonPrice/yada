#' @title Cross-validation of univariate models
#' 
#' @description
#' [crossval_univariate_models] utilizes the out-of-sample negative
#' log-likelihood to rank univariate models. The ranking involves three
#' considerations. First, ordinal models can be rejected for one of the reasons
#' outlined below. Second, the models are ordered from best to worst by their
#' out-of-sample negative log-likelihoods, which are stored in two arrays (see
#' below): cv_array_ord for ordinal variables and cv_array_cont for continuous
#' variables). Third, all models within cand_tol of the best model are considered
#' equally good, and such models are re-ordered (if necessary) by their
#' simplicity, where the ordering is lin_ord_const < lin_ord_lin_pos_int <
#' log_ord_const < log_ord_lin_pos_int < pow_law_ord_const <
#' pow_law_ord_lin_pos_int for ordinal variables and pow_law_const <
#' pow_law_lin_pos_int for continuous variables. This function assumes that the
#' full set of possible yada candidate models are used, which could be relaxed
#' in a future release (there are six candidate ordinal models and two candidate
#' continuous models).
#'
#' There are four reasons that ordinal models are rejected and not included in
#' the final ranking:
#'
#' (a) At least one of the folds failed to fit successfully
#' (b) A log_ord model could not be fit
#' (c) The scaling exponent is close to zero (less than scale_exp_min), which
#'     implies an identifiability problem
#' (d) The heteroskedastic noise term, beta2, is too large (greater than
#'     beta2_max), which implies that the noise at x=0 tends to zero (relative
#'     to the response).
#'
#' The preceding rejection reasons are discussed in greater detail in the
#' following publication:
#'
#' TODO: add the final citation and link once it is available
#'
#' Aside from the preceding four reasons, some models have a very small
#' heteroskedastic noise term, beta2, which could be added as another failure
#' term. However, such models are typically very close to constant models, and
#' thus typically rejected by the combination of using cand_tol and applying the
#' simplicity metric (this was the case for all variables in the publication
#' referenced above).
#'
#' There are no tailored rejection criteria for continuous models.
#'
#' [crossval_univariate_models] takes the following inputs:
#'
#' data_dir	      The directory with save files and in which to store the
#'                results of the cross-validation
#' analysis_name  A "analysis_name" that uniquely specifies this set of models
#' scale_exp_min  The minimum acceptable value of the scaling exponent
#' cand_tol	      Candidate model tolerance. The best models are considered
#'                equally good if their respective out-of-sample negative
#'                log-likelihoods lie within cand_tol of each other. Model
#'                implicity is then used as a "tie-breaker"
#' beta2_max      The maximum acceptable value of beta2, the heteroskedastic
#'                noise parameter
#'
#' The output of [crossval_univariate_models] is a list with the following
#' named elements:
#'
#' cv_array_ord		Ordinal out-of-sample cross-validation array with dimensions
#'                  num_models_ord x num_folds x J
#' cv_array_ord		Continuous out-of-sample cross-validation array with
#'                  dimensions num_models_cont x num_folds x K
#' num_folds        The number of cross validation folds (for the preceding
#'                  publication, there are 4 folds)
#' param_list_ord   A list of lists of lists with parameter value matrices. The
#'                  lengths of the lists are J then num_models_ord then
#'                  num_param x (1+num_folds)
#' param_list_cont  A list of lists of lists with parameter value matrices. The
#'                  lengths of the lists are K then num_models_cont then
#'                  num_param x (1+num_folds)
#' num_obs_vect     A vector with the total number of observations for each
#'                  variable (length J+K)
#' can_do_log_ord	A boolean vector indicating whether the log_ord fits could
#'                  be done (length J)
#' ord_models		A vector containing the six known ordinal models
#' cont_models		A vector containing the two known continuous models
#' mod_select_ord	A list of data frames giving the model selection
#'                  information for ordinal variables. The list has length J,
#'                  with each element of the list having dimensions
#'                  num_models_ord x 5 (see function documentation for column
#'                  definitions)
#' mod_select_cont  A list of data frames giving the model selection
#'                  information for continuous variables. The list has length K,
#'                  with each element of the list having dimensions
#'                  num_models_cont x 5 (see function documentation for column
#'                  definitions)
#' cand_tol         The value for this input parameter
#' scale_exp_min    The value for this input parameter
#' beta2_max        The value for this input parameter
#'
#' where the following definitions apply:
#'
#' num_models_ord   The number of candidate ordinal models (for the preceding
#'                  publication, there are six candidate models)
#' J                The number of ordinal variables
#' K                The number of continuous variables
#' num_models_cont  The number of candidate continuous models (for the preceding
#'                  publication, there are two candidate models)
#'
#' Aside form returning the preceding output list, it is saved to an .rds file
#' in the data_dir directory.
#'
#' @param data_dir The directory with save files and in which to store the
#'   results of the cross-validation
#' @param analysis_name A "analysis_name" that uniquely specifies this set of
#'   models
#' @param scale_exp_min The minimum acceptable value of the scaling exponent
#' @param cand_tol Candidate model tolerance. The best models are considered
#'   equally good if their respective out-of-sample negative log-likelihoods
#'   lie within cand_tol of each other. Model simplicity is then used as a
#'   "tie-breaker"
#' @param beta2_max The maximum acceptable value of beta2, the heteroskedastic
#'   noise parameter
#'
#' @return A list consisting of named elements that summarize the cross
#'   validation (see function description)
#'
#' @export
crossval_univariate_models <- function(data_dir,
                                       analysis_name,
                                       cand_tol,
                                       scale_exp_min,
                                       beta2_max) {
  # TODO: Consider adding lin as an option for continuous mean specs
  # TODO: Consider adding support for user-specified sets of candidate models

  # Call a helper function to get the number of folds for each variable
  num_folds <- get_num_folds(data_dir,analysis_name)

  # Make sure all variables have the same number of folds
  if(length(unique(num_folds)) != 1) {
    stop('All variables should have the same number of folds')
  }

  num_folds <- num_folds[1]

  # Build the full list of "known" ordinal models
  mean_models_ord  <- c('pow_law_ord','log_ord','lin_ord')
  noise_models_ord <- c('const','lin_pos_int')
  ord_models <- c()
  for(mean_model in mean_models_ord) {
    for(noise_model in noise_models_ord) {
       ord_models[length(ord_models)+1] <- paste0(mean_model,'_',noise_model)
    }
  }
  # the number of known ordinal models
  num_models_ord <- length(ord_models)

  # Load the base problem
  prob0 <- readRDS(build_file_path(data_dir,
                                   analysis_name,
                                   "main_problem"))



  # Extract the number of ordinal and continuous variables
  J <- yada::get_J(prob0$mod_spec)
  K <- yada::get_K(prob0$mod_spec)

  # Initialize an array, cv_array_ord, in which to store the out-of-sample
  # negative log-likelihoods. cvArray has dimensions
  # num_models_ord x num_folds x J
  cv_array_ord <- array(NA,c(num_models_ord,num_folds,J))

  # Initialize a list, param_list_ord, that stores all the observed parameter
  # vectors. Ideally, these would be stored in an object like a Matlab cell
  # array with dimensions num_models_ord x J. Sadly, R does not have such an
  # object, so instead use a list of lists, in which the first list has length
  # J and the elements of that list have length num_models_ord. Each element
  # of the second list is a matrix with dimensions num_param x (1+num_folds).
  param_list_ord <- list()

  # Initialize a "vector" of length J+K, num_obs_vect, in which to store the
  # number of observations for each variable.
  num_obs_vect <- rep(NA,J+K)

  # Initialize a "vector", can_do_log_ord, that indicates whether the log_ord
  # models can be fit for each ordinal variable
  can_do_log_ord <- rep(F,J)

  # Populate cv_array_ord by looping over
  #   (a) variables
  #   (b) models
  #   (c) folds
  if(J > 0) {
    for(j in 1:J) {
      # Extract the original data, determine the number of observations, and
      # assess whether the log_ord models can be fit
      x <- prob0$x
      v <- prob0$Y[j,]
      keep <- !is.na(v)
      x <- x[keep]
      v <- v[keep]

      num_obs_vect[j] <- length(v)
      can_do_log_ord[j] <- !any(v[x==0] > 0)

      # Calculate the out of samples negative log-likelihoods and populate
      # paramList
      param_sub_list <- list() # param_list_ord is comprised of J versions of
                               # param_sub_list
      for(n in 1:length(ord_models)) {
        k_m <- ord_models[n] # the known model
        # Load the solution.
        parsed_model <- parse_joined_model(k_m)
        soln0 <- readRDS(build_file_path(data_dir,
                                         analysis_name,
                                         "univariate_ord_soln",
                                         j=j,
                                         var_name=prob0$var_names[j],
                                         mean_spec=parsed_model[1],
                                         noise_spec=parsed_model[2]))

        # Failed fits, which are of class try-error, must be dealt with
        if(class(soln0) != 'try-error') {
          have_param_mat <- T
          param_mat <- matrix(NA,length(soln0$th_y),1+num_folds)
          param_mat[,1] <- soln0$th_y
          mod_spec <- soln0$mod_spec
        } else {
          param_mat <- matrix(NA,0,0)
          have_param_mat <- F
        }

        # Loop over folds to do the negative log-likelihood calculations. Again,
        # failed fits must be dealt with
        for(f in 1:num_folds) {
          # Read the solution (fit) for this fold
          soln_f <- readRDS(build_file_path(data_dir,
                                            analysis_name,
                                            "univariate_ord_soln",
                                            j=j,
                                            var_name=prob0$var_names[j],
                                            mean_spec=parsed_model[1],
                                            noise_spec=parsed_model[2],
                                            fold=f))

          # Read the test data for this fold
          test_f <- readRDS(build_file_path(data_dir,
                                            analysis_name,
                                            "test_problem",
                                            fold=f))

          # Extract the (out-of-sample) observations and handle missing values
          x <- test_f$x
          v <- test_f$Y[j,]
          keep <- !is.na(v)
          x <- x[keep]
          v <- v[keep]

          if(class(soln_f) != 'try-error') {
            if(!have_param_mat) {
              # Sometimes the main solution or preceding fold fits did not
              # work, but this one does.
              mod_spec <- soln_f$mod_spec
              have_param_mat <- T
              param_mat <- matrix(NA,length(soln_f$th_y),1+num_folds)
            }
            # Store the parameters and do the actual negative log-likelihood
            # calculation
            param_mat[,1+f] <- soln_f$th_y
            cv_array_ord[n,f,j] <- calc_neg_log_lik_ord(soln_f$th_y,
                                                      x,
                                                      v,
                                                      soln_f$mod_spec)
          } else {
            # have_param_mat
            cv_array_ord[n,f,j] <- NA
          }
        }

        # Add names for the rows and columns of param_mat, then add it to
        # param_sub_list
        if(nrow(param_mat) > 0) {
          rows <- c()
          # number of parameters for the mean
          num_b <- get_num_var_univariate_ord('b',mod_spec)
          if(num_b > 0) {
            for(n_b in 1:num_b) {
              rows <- c(rows,paste0('b',n_b))
            }
          }
          # number of tau parameters
          num_tau <- get_num_var_univariate_ord('tau',mod_spec)
          for(n_tau in 1:num_tau) {
            rows <- c(rows,paste0('tau',n_tau))
          }
          # number of parameters for the noise
          num_beta <- get_num_var_univariate_ord('beta',mod_spec)
          for(n_beta in 1:num_beta) {
            rows <- c(rows,paste0('beta',n_beta))
          }
          rownames(param_mat) <- rows
          colnames(param_mat) <-
            c('main',unlist(lapply(1:num_folds,function(f){paste0('fold',f)})))
        }
        param_sub_list[[length(param_sub_list)+1]] <- param_mat
      }
      # Add param_sub_list to the "parent" list
      param_list_ord[[j]] <- param_sub_list
    }
  }

  # Do model selection for ordinal variables, and store results in
  # mod_select_ord, a list of length J in which each element is a data.frame
  # with num_models_ord rows and the following five columns:
  #
  # reject          Whether or not to reject for some reason
  # reject_reason    The reason for rejection (if applicable)
  # neg_log_lik       Out-of-sample negative log-likelihood
  # delta_neg_log_lik  Out-of-sample negative log-likelihood with minimum
  #                 subtracted
  # model_rank       The model's relative rank (only non-rejected models)

  mod_select_ord <- list()
  if(J > 0) {
    for(j in 1:J) {
      # Initialize the dataframe with model selection information
      selection_df <- data.frame(model            = ord_models,
                                 reject           = rep(F, num_models_ord),
                                 reject_reason    = rep('',num_models_ord),
                                 neg_log_lik      = rowSums(cv_array_ord[,,j]),
                                 delta_neg_log_lik=
                                   rowSums(cv_array_ord[,,j]) -
                                     min(rowSums(cv_array_ord[,,j]),na.rm=T),
                                 model_rank       = rep(NA,num_models_ord),
                                 stringsAsFactors=F
                               )

      # Reject for failed fold fits
      ind_bad <- !is.finite(selection_df$neg_log_lik) # covers NA, -Inf, and Inf
      if(sum(ind_bad) > 0) {
        selection_df$reject[ind_bad] <- T
        selection_df$reject_reason[ind_bad] <- 'Fold fit(s)'
      }
      
      # Reject for failed main fit
      for(i in 1:6) {
        if(is.na(sum(param_list_ord[[j]][[i]]))){
          selection_df$reject[i] <- T
          selection_df$reject_reason[i] <- 'Main fit'
        }
      }
      
      # "Reject" log_ord models that cannot be fit. This supersedes failed fits.
      if(!can_do_log_ord[j]) {
          selection_df$reject[3:4] <- T
          selection_df$reject_reason[3:4] <- 'log_ord'
      }

      # Reject pow_law_ord models for which the scaling exponent term is too small
      # (relative to the input scale_exp_min). This supersedes failed fits.
      model1_failures <- which(param_list_ord[[j]][[1]][1,] < scale_exp_min)
      if(any(model1_failures)) {
        selection_df$reject[1] <- T
        selection_df$reject_reason[1] <- 'Scaling exponent'
      }

      model2_failures <- which(param_list_ord[[j]][[2]][1,] < scale_exp_min)
      if(any(model2_failures)) {
        selection_df$reject[2] <- T
        selection_df$reject_reason[2] <- 'Scaling exponent'
      }

      # Reject heteroskedastic models for which the intercept term is too large.
      for(m in c(2,4,6)) {
        if(!selection_df$reject[m]) {
          if(fail_for_beta2(param_list_ord[[j]][[m]],beta2_max)) {
            selection_df$reject[m] <- T
            selection_df$reject_reason[m] <- 'Large beta2'
          }
        }
      }

      # Rank non-rejected models by:
      # (1) ordering by out-of-sample negative log-likelihood
      # (2) identifying models within cand_tol of the best model
      # (3) ordering models from step (2) by simplicity

      # Remove rejected models
      ind_ok <- selection_df$reject == F
      neg_log_lik_ok <- selection_df$neg_log_lik[ind_ok]
      ord_models_ok <- ord_models[ind_ok]
      delta_neg_log_lik_ok <- neg_log_lik_ok - min(neg_log_lik_ok)
      model_order     <- order(delta_neg_log_lik_ok)
      model_order_inv <-  rank(delta_neg_log_lik_ok)
      delta_neg_log_lik_ok <- delta_neg_log_lik_ok[model_order]
      ord_models_ok <- ord_models_ok[model_order]

      # If necessary, apply the cand_tol criterion
      if(any(delta_neg_log_lik_ok[-1] <= cand_tol)) {
        # Then must consider model simplicity for the best models (only the
        # best models per cand_tol are re-ordered.
        ind_best <- which(delta_neg_log_lik_ok <= cand_tol)
        ind_other <- which(delta_neg_log_lik_ok > cand_tol)
        pref_ordering <- c('lin_ord_const',
                           'lin_ord_lin_pos_int',
                           'log_ord_const',
                           'log_ord_lin_pos_int',
                           'pow_law_ord_const',
                           'pow_law_ord_lin_pos_int')
        rank_best <- order(unlist(
          lapply(ord_models_ok[ind_best],
                 function(k_m){which(k_m==pref_ordering)})))
        rank_other <- length(rank_best) + 1:length(ind_other)
        rank_ok <- c(rank_best,rank_other)
      } else {
        rank_ok <- 1:length(model_order)
      }
      rank_ok <- rank_ok[model_order_inv]
      selection_df$model_rank[ind_ok] <- rank_ok
      
      mod_select_ord[[length(mod_select_ord)+1]] <- selection_df
    }
  }

  # Build the full list of "known" continuous models
  mean_models_cont  <- 'pow_law'
  noise_models_cont <- c('const','lin_pos_int')
  cont_models <- c()
  for(mean_model in mean_models_cont) {
    for(noise_model in noise_models_cont) {
       cont_models[length(cont_models)+1] <- paste0(mean_model,'_',noise_model)
    }
  }
  num_models_cont <- length(cont_models) # the number of known ordinal models

  # Initialize an array, cv_array_cont, in which to store the out-of-sample
  # negative log-likelihoods. cv_array_cont has dimensions
  # num_models_cont x num_folds x K
  cv_array_cont <- array(NA,c(num_models_cont,num_folds,K))

  # Initialize a list, param_list_cont, that stores all the observed parameter
  # vectors. Ideally, these would be stored in an object like a Matlab cell
  # array with dimensions num_models_cont x K. Sadly, R does not have such an
  # object, so instead use a list of lists, in which the first list has length
  # K and the elements of that list have length num_models_cont. Each element
  # of the second list is a matrix with dimensions num_param x (1+num_folds).
  param_list_cont <- list()

  # Populate cv_array_cont by looping over
  #   (a) variables
  #   (b) models
  #   (c) folds
  if(K > 0) {
    for(k in 1:K) {
      # Extract the original data and determine the number of observations
      x <- prob0$x
      w <- prob0$Y[J+k,]
      keep <- !is.na(w)
      x <- x[keep]
      w <- w[keep]

      num_obs_vect[J+k] <- length(w)

      # Calculate the out of samples negative log-likelihoods and populate
      # paramList
      param_sub_list <- list() # param_list_cont is comprised of k versions of
                             # param_sub_list
      for(n in 1:length(cont_models)) {
        k_m <- cont_models[n] # the known model
        # Load the solution.
        parsed_model <- parse_joined_model(k_m)
        soln0 <- readRDS(build_file_path(data_dir,
                                         analysis_name,
                                         "univariate_cont_soln",
                                         k=k,
                                         var_name=prob0$var_names[J+k],
                                         mean_spec=parsed_model[1],
                                         noise_spec=parsed_model[2]))

        # Failed fits, which are of class try-error, must be dealt with
        if(class(soln0) != 'try-error') {
          have_param_mat <- T
          param_mat <- matrix(NA,length(soln0$th_y),1+num_folds)
          param_mat[,1] <- soln0$th_y
          mod_spec <- soln0$mod_spec
        } else {
          param_mat <- matrix(NA,0,0)
          have_param_mat <- F
        }

        # Loop over folds to do the negative log-likelihood calculations. Again,
        # failed fits must be dealt with
        for(f in 1:num_folds) {

          # Read the solution (fit) for this fold
          soln_f <- readRDS(build_file_path(data_dir,
                                            analysis_name,
                                            "univariate_cont_soln",
                                            k=k,
                                            var_name=prob0$var_names[J+k],
                                            mean_spec=parsed_model[1],
                                            noise_spec=parsed_model[2],
                                            fold=f))

          # Read the test data for this fold
          test_f <- readRDS(build_file_path(data_dir,
                                            analysis_name,
                                            "test_problem",
                                            fold=f))

          # Extract the (out-of-sample) observations and handle missing values
          x <- test_f$x
          w <- test_f$Y[J+k,]
          keep <- !is.na(w)
          x <- x[keep]
          w <- w[keep]

          if(class(soln_f) != 'try-error') {
            if(!have_param_mat) {
              # Sometimes the main solution or preceding fold fits did not
              # work, but this one does.
              mod_spec <- soln_f$mod_spec
              have_param_mat <- T
              param_mat <- matrix(NA,length(soln_f$th_y),1+num_folds)
            }
            # Store the parameters and do the actual negative log-likelihood
            # calculation
            param_mat[,1+f] <- soln_f$th_y
            cv_array_cont[n,f,k] <- calc_neg_log_lik_cont(soln_f$th_y,
                                                          x,
                                                          w,
                                                          soln_f$mod_spec)
          } else {
            # 
            cv_array_cont[n,f,k] <- NA
          }
        }

        # Add names for the rows and columns of param_mat, then add it to
        # param_sub_list
        if(nrow(param_mat) > 0) {
          rows <- c()
          # number of parameters for the mean
          num_c <- get_num_var_univariate_cont('c',mod_spec)
          if(num_c > 0) {
            for(n_c in 1:num_c) {
              rows <- c(rows,paste0('c',n_c))
            }
          }
          # number of parameters for the noise
          num_kappa <- get_num_var_univariate_cont('kappa',mod_spec)
          for(n_kappa in 1:num_kappa) {
            rows <- c(rows,paste0('kappa',n_kappa))
          }
          rownames(param_mat) <- rows
          colnames(param_mat) <-
            c('main',unlist(lapply(1:num_folds,function(f){paste0('fold',f)})))
        }
        param_sub_list[[length(param_sub_list)+1]] <- param_mat
      }
      # Add param_sub_list to the "parent" list
      param_list_cont[[k]] <- param_sub_list
    }
  }

  # Do model selection for continuous variables, and store results in
  # mod_select_cont, a list of length K in which each element is a data.frame
  # with num_models_cont rows and the following five columns:
  #
  # reject          Whether or not to reject for some reason
  # reject_reason    The reason for rejection (if applicable)
  # neg_log_lik       Out-of-sample negative log-likelihood
  # delta_neg_log_lik  Out-of-sample negative log-likelihood with minimum
  #                 subtracted
  # model_rank       The model's relative rank (only non-rejected models)

  mod_select_cont <- list()
  if(K > 0) {
    for(k in 1:K) {
      # Initialize the dataframe with model selection information
      selection_df <- data.frame(model             = cont_models,
                                 reject            = rep(F, num_models_cont),
                                 reject_reason     = rep('',num_models_cont),
                                 neg_log_lik       =
                                   rowSums(cv_array_cont[,,k]),
                                 delta_neg_log_lik =
                                   rowSums(cv_array_cont[,,k]) -
                                     min(rowSums(cv_array_cont[,,k]),na.rm=T),
                                 model_rank        = rep(NA,num_models_cont),
                                 stringsAsFactors=F
                               )

      # Reject for failed fits
      ind_bad <- !is.finite(selection_df$neg_log_lik) # covers NA, -Inf, and Inf
      if(sum(ind_bad) > 0) {
        selection_df$reject[ind_bad] <- T
        selection_df$reject_reason[ind_bad] <- 'Fold fit(s)'
      }
    
      # Rank non-rejected models by:
      # (1) ordering by out-of-sample negative log-likelihood
      # (2) identifying models within cand_tol of the best model
      # (3) ordering models from step (2) by simplicity

      # Remove rejected models
      ind_ok <- selection_df$reject == F
      neg_log_lik_ok <- selection_df$neg_log_lik[ind_ok]
      cont_models_ok <- cont_models[ind_ok]
      delta_neg_log_lik_ok <- neg_log_lik_ok - min(neg_log_lik_ok)
      model_order     <- order(delta_neg_log_lik_ok)
      model_order_inv <-  rank(delta_neg_log_lik_ok)
      delta_neg_log_lik_ok <- delta_neg_log_lik_ok[model_order]
      cont_models_ok <- cont_models_ok[model_order]

      # If necessary, apply the cand_tol criterion
      if(any(delta_neg_log_lik_ok[-1] <= cand_tol)) {
        # Then must consider model simplicity for the best models (only the
        # best models per cand_tol are re-ordered.
        ind_best <- which(delta_neg_log_lik_ok <= cand_tol)
        ind_other <- which(delta_neg_log_lik_ok > cand_tol)
        pref_ordering <- c('pow_law_const','pow_law_lin_pos_int')
        rank_best <- order(unlist(
          lapply(cont_models_ok[ind_best],
                 function(k_m){which(k_m==pref_ordering)})))
        rank_other <- length(rank_best) + 1:length(ind_other)
        rank_ok <- c(rank_best,rank_other)
      } else {
        rank_ok <- 1:length(model_order)
      }
      rank_ok <- rank_ok[model_order_inv]
      selection_df$model_rank[ind_ok] <- rank_ok
      
      mod_select_cont[[length(mod_select_cont)+1]] <- selection_df
    }
  }

  # Create, save, and return the output list
  cv_data <- list(
                  cv_array_ord    = cv_array_ord,
                  cv_array_cont   = cv_array_cont,
                  num_folds       = num_folds,
                  param_list_ord  = param_list_ord,
                  param_list_cont = param_list_cont,
                  num_obs_vect    = num_obs_vect,
                  can_do_log_ord  = can_do_log_ord,
                  ord_models      = ord_models,
                  cont_models     = cont_models,
                  mod_select_ord  = mod_select_ord,
                  mod_select_cont = mod_select_cont,
                  cand_tol        = cand_tol,
                  scale_exp_min   = scale_exp_min,
                  beta2_max       = beta2_max
                )

  saveRDS(cv_data, build_file_path(data_dir, analysis_name, "cv_data"))
  return(cv_data)
}

#' @title
#' Make an Rmarkdown document in the results folder for an ordinal variable.
#' 
#' @description Wrapper function that reads in univariate ordinal information
#' and generates an rMarkdown report in the same directory where the files are
#' housed. (Typically this is the results folder).
#' 
#' @param data_dir Directory where the problem and CV files are saved
#' @param analysis_name analysis_name for file-naming
#' @param j The ordinal variable number
#' @param line_width User-defined line widths. Default is NA
#' 
#' @export

write_ordinal_report <- function(data_dir,analysis_name,j,line_width=NA) {

  # If necessary, set the line width to the input line_width, and store the
  # result to change back at the bottom of the function.
  if(!is.na(line_width)) {
    old_line_width <- getOption('width')
    options(width=line_width)
  }

  # Read the baseline problem
  prob0 <- readRDS(build_file_path(data_dir,
                                   analysis_name,
                                   "main_problem"))
  # Read the cross validation data
  cv_data <- readRDS(build_file_path(data_dir,
                                     analysis_name,
                                     "cv_data"))

  # Open an rmarkdown file to programatically generate a report (deleting the
  # file if it already exists)
  rm_file <- build_file_path(data_dir,
                             analysis_name,
                             "univariate_ord_rmd",
                             j=j,
                             var_name=prob0$var_names[j])
  
  # rm_file <- file.path(data_dir,paste0('cindep_ord_j_',j,
  #                                      '_',prob0$var_names[j],'.Rmd'))
  if (file.exists(rm_file)) {
    file.remove(rm_file)
  }

  # Write "header" information
  write('---',file=rm_file,append=T)
  write(paste0('title: "Cross-validation report for ',
               prob0$var_names[j],
               ' (j = ',j,')"'),
        file=rm_file,append=T)
  write('output: html_document',file=rm_file,append=T)
  write('---',file=rm_file,append=T)

  # Add summary information
  write('# Summary information',file=rm_file,append=T)
  write(paste0(cv_data$num_obs_vect[j],
               ' non-missing observations  '),
        file=rm_file,append=T)
  write(paste0(prob0$mod_spec$M[j]+1,' ordinal categories  '),
        file=rm_file,append=T)
  write(paste0('Candidate model tolerance is cand_tol = ',
               cv_data$cand_tol,
               '  '),
        file=rm_file,
        append=T)
  write(paste0('Minimum scaling exponent size is scale_exp_min = ',
               cv_data$scale_exp_min,'  '),
        file=rm_file,
        append=T)
  write(paste0('Maxiumum value for beta2 is beta2_max = ',
               cv_data$beta2_max,'  '),
        file=rm_file,
        append=T)

  if(!cv_data$can_do_log_ord[j]) {
    write(paste0('log_ord models cannot be fit since cases ',
                 'exist for which v > 0 when x=0'),
          file=rm_file,
          append=T)
  }
  write('',file=rm_file,append=T)

  write('# Negative log-likelihood by model and fold',file=rm_file,append=T)
  cv_matrix <- cv_data$cv_array_ord[,,j]
  colnames(cv_matrix) <- unlist(lapply(1:cv_data$num_folds,
                                       function(f){paste0('fold',f)}))
  rownames(cv_matrix) <- unlist(lapply(cv_data$ord_models,
                                       function(k_m){gsub('_','-',k_m)}))
  write_matrix(cv_matrix,rm_file)

  write('# Automated model selection',file=rm_file,append=T)
  write_matrix(cv_data$mod_select_ord[[j]],rm_file)

  write('# Univariate fits for each model',file=rm_file,append=T)
  # For each model, output the matrix of parameter values, which has dimensions
  # num_param x (1+num_folds)
  for(m in 1:length(cv_data$ord_models)) {
    k_m <- cv_data$ord_models[m]
    write(paste0('## ',k_m,'  '),file=rm_file,append=T)
    param_matrix <- cv_data$param_list_ord[[j]][[m]]
    if(nrow(param_matrix) != 0) {
      write_matrix(param_matrix,rm_file)
    } else {
      write('Cannot fit  ',file=rm_file,append=T)
      write('',file=rm_file,append=T)
    }
  }

  rmarkdown::render(rm_file)
  if(!is.na(line_width)) {
    options(width=old_line_width)
  }
}

#' @title A helper function to write a matrix to an Rmd file
#' 
#' @param mat Matrix to be written
#' @param rm_file rMarkdown file name
#' 
#' @export
write_matrix <- function(mat,rm_file) {
  # preserve the pre-formatted text (i.e., don't remove white space)
  write('<pre>',file=rm_file,append=T)

  lines <- gsub('_','-',capture.output(print(mat)))
  # Write each line, adding two spaces since that makes new lines in rmarkdown
  for(l in lines) {
    write(paste0(l,'  '),file=rm_file,append=T)
  }
  write('</pre>',file=rm_file,append=T)
}

#' @title
#' Make an Rmarkdown document in the results folder for a continuous variable.
#' 
#' @param data_dir Directory where problem and CV files are located.
#' @param analysis_name analysis_name for file-naming
#' @param k Number of current continuous variable
#' @param line_width Line width of rMarkdown file. Default is NA.
#' 
#' @export
write_continuous_report <- function(data_dir,analysis_name,k,line_width=NA) {

  # If necessary, set the line width to the input line_width, and store the
  # result to change back at the bottom of the function.
  if(!is.na(line_width)) {
    old_line_width <- getOption('width')
    options(width=line_width)
  }
  # Read the baseline problem
  prob0 <- readRDS(build_file_path(data_dir,
                                   analysis_name,
                                   "main_problem"))
  # Read the cross validation data
  cv_data <- readRDS(build_file_path(data_dir,
                                     analysis_name,
                                     "cv_data"))

  # Open an rmarkdown file to programatically generate a report (deleting the
  # file if it already exists)
  # TODO: consider adding the following file to build_file_path
  J <- yada::get_J(prob0$mod_spec)
  
  rm_file <- build_file_path(data_dir,
                             analysis_name,
                             "univariate_cont_rmd",
                             k=k,
                             var_name=prob0$var_names[J+k])
  
  # rm_file <- file.path(data_dir,
  #                      paste0('cindep_cont_k_',k,'_',
  #                             prob0$var_names[J+k],'.Rmd'))
  if (file.exists(rm_file)) {
    file.remove(rm_file)
  }

  # Write "header" information
  write('---',file=rm_file,append=T)
  write(paste0('title: "Cross-validation report for ',prob0$var_names[J+k],' (k = ',k,')"'),file=rm_file,append=T)
  write('output: html_document',file=rm_file,append=T)
  write('---',file=rm_file,append=T)

  # Add summary information
  write('# Summary information',file=rm_file,append=T)
  write(paste0(cv_data$num_obs_vect[J+k],' non-missing observations  '),file=rm_file,append=T)
  write(paste0('Candidate model tolerance is cand_tol = ',cv_data$cand_tol,'  '),file=rm_file,append=T)

  write('',file=rm_file,append=T)

  write('# Negative log-likelihood by model and fold',file=rm_file,append=T)
  cv_matrix <- cv_data$cv_array_cont[,,k]
  colnames(cv_matrix) <- unlist(lapply(1:cv_data$num_folds,
                                       function(f){paste0('fold',f)}))
  rownames(cv_matrix) <- unlist(lapply(cv_data$cont_models,
                                       function(k_m){gsub('_','-',k_m)}))
  write_matrix(cv_matrix,rm_file)

  write('# Automated model selection',file=rm_file,append=T)
  write_matrix(cv_data$mod_select_cont[[k]],rm_file)

  write('# Univariate fits for each model',file=rm_file,append=T)
  # For each model, output the matrix of parameter values, which has dimensions
  # num_param x (1+num_folds)
  for(m in 1:length(cv_data$cont_models)) {
    k_m <- cv_data$cont_models[m]
    write(paste0('## ',k_m,'  '),file=rm_file,append=T)
    param_matrix <- cv_data$param_list_cont[[k]][[m]]
    if(nrow(param_matrix) != 0) {
      write_matrix(param_matrix,rm_file)
    } else {
      write('Cannot fit  ',file=rm_file,append=T)
      write('',file=rm_file,append=T)
    }
  }

  rmarkdown::render(rm_file)
  if(!is.na(line_width)) {
    options(width=old_line_width)
  }
}

#' @title A helper function to check whether the fits in TH_y fail for beta2
#' 
#' @param TH_y TH_y is a matrix with dimensions num_param x (1+num_folds)
#' @param beta2_max beta2 threshold
#' 
#' @export
fail_for_beta2 <- function(TH_y,beta2_max) {
  beta2_vect <- TH_y[nrow(TH_y),]
  return(any(beta2_vect > beta2_max))
}

#' @title
#' A helper function to identify the number of training problems (folds)
#'
#' @description
#' The training problem files begin with "train_analysis_name_fold". Count the
#' number of such files.
#'
#' @param data_dir The data directory with problems and results
#' @param analysis_name A unique analysis name (for the input data directory)
#'
#' @return Number of test problems (folds)
#'
#' @export
get_num_training_problems <- function(data_dir,analysis_name) {
  start_string <- paste0("train_",analysis_name,"_fold")
  matches <- unlist(lapply(dir(data_dir),
                           function(file_name){
                             startsWith(file_name,start_string)}))
  return(sum(matches))
}

#' @title
#' A helper function to identify univariate fits in a folder and determine the
#' number of folds for each fit.
#' 
#' @param data_dir Directory where the files are located.
#' @param analysis_name analysis_name for file-searching
#' @param j number of current ordinal variable
#' @param k number of current continuous variable
#' 
#' @export
get_num_folds <- function(data_dir,analysis_name,j=NA,k=NA) {
  # Return the number of folds for one or more variables
  # If neither j nor k are input, return the number of folds for all variables
  # If j is input but not k, return the number for just j (and similarly for k)
  # Both and j and k can be vectors
  # get_num_folds(data_dir,analysis_name) is equivalent to
  # get_num_folds(data_dir,analysis_name,j=1:J,k=1:K)

  # Base cases	(a) j has length one and k is NA
  #	            (b) k has length one and j is NA

  have_base_case <- ( !all(is.na(j)) && length(j) == 1 && is.na(k) ) ||
                  ( !all(is.na(k)) && length(k) == 1 && is.na(j) )

  if(have_base_case) {
    cases <- get_univariate_variable_cases(data_dir,analysis_name,j=j,k=k)

    # Determine the number of folds. First, subset to the fold cases, which
    # begin with, e.g., fold2_*' for the second fold
    fold_cases <- cases[unlist(lapply(cases,function(f){startsWith(f,'fold')}))]

    # Extract each fold number as an integer
    fold_num_vect <- unlist(lapply(fold_cases,
                                    function(f){as.numeric(
                                      strsplit(substr(f,5,nchar(f)),'_')[[1]][1])
                                    }))
    num_folds <- max(fold_num_vect) # the number of folds
    return(num_folds)
  }

  # This is not a base case

  # Neither j nor k is input. Calculate for all variables
  if(all(is.na(j)) && all(is.na(k))) {
    prob0 <- readRDS(build_file_path(data_dir,analysis_name,"main_problem"))
    J <- yada::get_J(prob0$mod_spec)
    K <- yada::get_K(prob0$mod_spec)
    if( (J == 0) && (K == 0) ) {
      stop('Both J and K cannot be zero')
    }

    if(J > 0) {
      j <- 1:J
    }

    if(K > 0) {
      k <- 1:K
    }
  }

  num_folds <- c()
  if(!all(is.na(j))) {
    for(jj in j) {
       num_folds <- c(num_folds,get_num_folds(data_dir,analysis_name,j=jj))
    }
  }

  if(!all(is.na(k))) {
    for(kk in k) {
       num_folds <- c(num_folds,get_num_folds(data_dir,analysis_name,k=kk))
    }
  }
  return(num_folds)
}

#' @title A helper function to check the files in data_dir to get the cases (fit
#' univariate models). Exactly one of j and k should be input.
#' 
#' @param data_dir Directory where files are located
#' @param analysis_name analysis_name for file-searching
#' @param j Number of current ordinal variable
#' @param k Number of current continuous variable
#' 
#' @export

get_univariate_variable_cases <- function(data_dir,analysis_name,j=NA,k=NA) {

  if(length(j) != 1) {
    stop('j should be NA or a scalar')
  }

  if(length(k) != 1) {
    stop('k should be NA or a scalar')
  }

  if(!is.na(j) && !is.na(k)) {
    stop('Only one of j and k should be input')
  }

  if(is.na(j) && is.na(k)) {
    stop('At least one of j and k should be input')
  }

  files <- dir(data_dir)
  # Subset to univariate fits for this analysis_name, which begin with
  # 'solutiony_analysis_name'
  files <- files[unlist(lapply(files,
                               function(f){
                                 startsWith(f,
                                            paste0('solutiony_',
                                                   analysis_name))
                               }))]
  # Subset to fits for this variable
  if(!is.na(j)) {
    # The pattern is 'ord_j_2_' for j=2.
    files <- files[unlist(lapply(files,
                                 function(f){
                                   grepl(paste0('ord_j_',
                                                j,
                                                '_'),
                                         f)
                                 }))]
  } else {
    # The pattern is 'cont_k_2_' for k=2.
    files <- files[unlist(lapply(files,
                                 function(f){
                                   grepl(paste0('cont_k_',
                                                k,
                                                '_'),
                                         f)
                                 }))]
  }
  # Remove the leading part of each file and the file extension ('.rds')
  # Number of leading characters
  num_leading <- nchar(paste0('solutiony_',analysis_name,'_'))
  cases <- unlist(lapply(files,function(f){substr(f,num_leading+1,nchar(f))}))
  cases <- unlist(lapply(cases,function(f){substr(f,1,nchar(f)-4)}))
  return(cases)
}

#' @title
#' Parse a joined model to get the individual mean and noise specs
#'
#' @description
#' The input is a string representing a mean/noise pair such as
#' "pow_law_ord_const". Return a vector in which the first element is the
#' mean spec and the second element the noise spec, c("pow_law_ord","const").
#'
#' @param joined_model The string representing the joined model.
#'
#' @return A vecto consisting of the mean spec and noise spec
#'
#' @export
parse_joined_model <- function(joined_model) {
  # The recognized mean specs are:
  mean_specs <- c("pow_law","pow_law_ord","log_ord","lin_ord")
  # The recognized noise specs are:
  noise_specs <- c("const","lin_pos_int")

  for (mean_spec in mean_specs) {
    for (noise_spec in noise_specs) {
      if (joined_model == paste0(mean_spec, "_", noise_spec)) {
        return(c(mean_spec, noise_spec))
      }
    }
  }
  stop(paste0("Unrecognized joined_model = ",joined_model))
}

#'
#' @title Get the parameters of the best cross-validated fits
#'
#' @description
#' Wrapper function that takes object cv_data, stores the highest ranking model
#' specification, noise specification, and model variables, and outputs all
#' results as a RDS file in dataframe format.
#' 
#' @param data_dir Directory to save univariate model parameters
#' @param analysis_name analysis_name for file-naming
#' @param save_file Logical whether to save file or not as .rds
#'
#' @return Data frame with variable (var), type, mean_spec, noise_spec, parameter
#'   name (params), and parameter value (param_val).
#'
#' @export
get_best_univariate_params <- function(data_dir,
                                       analysis_name,
                                       save_file=TRUE) {

  problem0 <- readRDS(build_file_path(data_dir, analysis_name, "main_problem"))
  cv_data <- readRDS(build_file_path(data_dir, analysis_name, "cv_data"))
  ## Ordered list of variables
  variables <- problem0$var_names
  
  ## Extract ordinal model parameters
  ord_output <- NULL
  for(i in 1:length(cv_data$mod_select_ord)) {
    var_ord <- variables[i]
    type_ord <- 'Ord'
    select_idx_ord <- which(cv_data$mod_select_ord[[i]]$model_rank == 1)
    if(length(select_idx_ord) == 0) {
      print(paste0('Variable ', var_ord, ' yielded no model selection.'))
      mean_spec_ord <- NA
      noise_spec_ord <- NA
      param_names_ord <- NA
      best_params_ord <- NA
      temp_output_ord <- cbind(var_ord,
                               type_ord,
                               mean_spec_ord,
                               noise_spec_ord,
                               param_names_ord,
                               best_params_ord)
      ord_output <- rbind(ord_output, temp_output_ord)
      if(i == length(cv_data$mod_select_ord)) {
        break
      } else {
        next
      }
    }
    best_model_ord <- parse_joined_model(cv_data$ord_models[[select_idx_ord]])
    mean_spec_ord <- best_model_ord[1]
    noise_spec_ord <- best_model_ord[2]
    # main model params
    best_params_ord <- cv_data$param_list_ord[[i]][[select_idx_ord]][,1]
    param_names_ord <- names(best_params_ord)
    temp_output_ord <- cbind(var_ord,
                             type_ord,
                             mean_spec_ord,
                             noise_spec_ord,
                             param_names_ord,
                             best_params_ord)
    ord_output <- rbind(ord_output, temp_output_ord)
  }
  
  # Extract continuous model parameters
  cont_output <- NULL
  for(j in 1:length(cv_data$mod_select_cont)) {
    var_cont <- variables[j+(length(cv_data$mod_select_ord))]
    type_cont <- 'Cont'
    select_idx_cont <- which(cv_data$mod_select_cont[[j]]$model_rank == 1)
    if(length(select_idx_cont) == 0) {
      print(paste0('Variable ', var_cont, ' yielded no model selection.'))
      mean_spec_cont <- NA
      noise_spec_cont <- NA
      param_names_cont <- NA
      best_params_cont <- NA
      temp_output_cont <- cbind(var_cont,
                                type_cont,
                                mean_spec_cont,
                                noise_spec_cont,
                                param_names_cont,
                                best_params_cont)
      cont_output <- rbind(cont_output, temp_output_cont)
      if(i == length(cv_data$mod_select_cont)) {
        break
      } else {
        next
      }
    }
    best_model_cont <- parse_joined_model(cv_data$cont_models[[select_idx_cont]])
    mean_spec_cont <- best_model_cont[1]
    noise_spec_cont <- best_model_cont[2]
    # main model params
    best_params_cont <- cv_data$param_list_cont[[j]][[select_idx_cont]][,1]
    param_names_cont <- names(best_params_cont)
    temp_output_cont <- cbind(var_cont,
                              type_cont,
                              mean_spec_cont,
                              noise_spec_cont,
                              param_names_cont,
                              best_params_cont)
    cont_output <- rbind(cont_output, temp_output_cont)
  }
  
  final_output <- rbind(ord_output, cont_output)
  rownames(final_output) <- NULL
  colnames(final_output) <- c('var',
                              'type',
                              'mean_spec',
                              'noise_spec',
                              'params',
                              'param_val')
  final_output <- as.data.frame(final_output,stringsAsFactors=FALSE)
  
  if(!is.numeric(final_output$param_val)) {
    final_output$param_val <- as.numeric(final_output$param_val)
  }

  # remove non-selected traits
  final_output <- na.omit(final_output)

  if(save_file){
    saveRDS(final_output, file.path(data_dir,
                                   paste0(analysis_name,
                                          '_univariate_model_parameters.rds')))
  }
  
  return(final_output)
}

#' @title Generate mod_spec list for use in yada functions
#'
#' @description A simple function that provides assistance in generating the
#' list-format for mod_spec expected by yada functions
#'
#' @param problem Object containing the problem file for current mod_spec
#' @param model_params Data frame containing variable parameters for the best
#'   performing models (generated using `get_best_univariate_params()`)
#' @param cdep_spec Distinction of a conditionally independent ('indep') or
#'   conditionally dependent ('dep') model.
#' @param uni_variable Trait name used for univariate model. If left as 'NA', the
#'   mod_spec for multivariate models is generated.
#'
#' @export
generate_mod_spec <- function(problem,
                             model_params,
                             cdep_spec,
                             uni_variable=NA) {
  if(is.na(uni_variable)) {
    if(cdep_spec != 'dep') {
      stop("cdep_spec does not match multivariate model type")
    }
    mod_spec0 <- problem$mod_spec
    var_names <- problem$var_names
    
    mean_spec <- NULL
    noise_spec <- NULL
    
    for(j in 1:length(var_names)) {
      mean_spec[length(mean_spec)+1] <-
        as.character(model_params$mean_spec[model_params$var==var_names[j]][1])
      noise_spec[length(noise_spec)+1] <-
        as.character(model_params$noise_spec[model_params$var==var_names[j]][1])
    }
    
    mod_spec0$mean_spec <- mean_spec
    mod_spec0$noise_spec <- noise_spec
    mod_spec0$cdep_spec <- cdep_spec
  } else {  # univariate model with defined variable 'uni_variable'
    if(cdep_spec != 'indep') {
      stop("cdep_spec does not match univariate model type")
    }
    model_params_sub <- model_params[model_params$var==uni_variable,]
    mod_spec0 <- list(mean_spec = model_params_sub$mean_spec[1])
    mod_spec0$noise_spec <- model_params_sub$noise_spec[1]
    if(model_params_sub$type[1]=='Ord') {
      mod_spec0$J <- 1
      mod_spec0$K <- 0
      mod_spec0$M <- length(grep('tau',model_params_sub$params))
    } else {
      mod_spec0$J <- 0
      mod_spec0$K <- 1
    }
    
    mod_spec0$cdep_spec <- cdep_spec
    
  }
  
  return(mod_spec0)
}

#' @title
#' Load the best univariate model using saved univariate cross-validation
#' results
#'
#' @description The cross validation data file (cv_data) must already exists
#' in the analysis directory and the input variable name, var_name, must be
#' a variable in cv_data.
#'
#' @param data_dir Directory to save univariate model parameters
#' @param analysis_name analysis_name for file-naming
#' @param var_name The variable name for the model to load
#' @param fold Fold number. If NA, pull from main solution.
#' @param load_data Whether to also load the data vector (x and y) (default:
#'   FALSE)
#'
#' @return A list with the parameter vector (th_y), model specification
#'   (mod_spec). If load_data is TRUE, the data vectors (x and y) are also in
#'   the list.
#'
#' @export
load_best_univariate_model <- function(data_dir,
                                       analysis_name,
                                       var_name,
                                       fold=NA,
                                       load_data=FALSE) {
  # Read the cross-validation data and main problem
  cv_data <- readRDS(build_file_path(data_dir, analysis_name, "cv_data"))
  if (is.na(fold)) {
    problem <- readRDS(build_file_path(data_dir,
                                       analysis_name,
                                       "main_problem"))
  } else {
    problem <- readRDS(build_file_path(data_dir,
                                       analysis_name,
                                       "training_problem",
                                       fold=fold))
  }

  # Identify the index of the variable of interest
  ind_var <- which(problem$var_names == var_name)
  if (length(ind_var) != 1) {
    stop("var_name should match exactly one entry in problem$var_names")
  }
  
  # Identify whether main or fold parameters should be extracted
  if(is.na(fold)) {
    th_y_col <- "main"
  } else {
    th_y_col <- paste0("fold",fold)
  }

  # Ordinal variables run from 1 to J and continuous variable run from J+1 to
  # J+K.
  is_ord <- ind_var <= problem$mod_spec$J

  if(is_ord) {
    j <- ind_var
    ind_best_model <- which(cv_data$mod_select_ord[[j]]$model_rank == 1)
    if (length(ind_best_model) != 1) {
      stop("There should be exactly one best model")
    }
    mean_noise <- parse_joined_model(cv_data$ord_models[[ind_best_model]])
    mean_spec <- mean_noise[1]
    noise_spec <- mean_noise[2]
    # Extract the main solution (first column) for the best model
    th_y <- as.vector(cv_data$param_list_ord[[j]][[ind_best_model]][,th_y_col])
    M <- problem$mod_spec$M[j]
    mod_spec <- list(J=1,
                     K=0,
                     mean_spec=mean_spec,
                     noise_spec=noise_spec,
                     M=M)
  } else {
    k <- ind_var - problem$mod_spec$J
    ind_best_model <- which(cv_data$mod_select_cont[[k]]$model_rank == 1)
    if (length(ind_best_model) != 1) {
      stop("There should be exactly one best model")
    }
    mean_noise <- parse_joined_model(cv_data$cont_models[[ind_best_model]])
    mean_spec <- mean_noise[1]
    noise_spec <- mean_noise[2]
    # Extract the main solution (first column) for the best model
    th_y <- as.vector(cv_data$param_list_cont[[k]][[ind_best_model]][,th_y_col])
    mod_spec <- list(J=0,
                     K=1,
                     mean_spec=mean_spec,
                     noise_spec=noise_spec)
  }

  if(load_data) {
    x <- problem$x
    y <- problem$Y[ind_var,]
    ind_keep <- !is.na(x) & !is.na(y)
    x <- x[ind_keep]
    y <- y[ind_keep]
    return(list(th_y=th_y, mod_spec=mod_spec, x=x, y=y))
  } else {
    return(list(th_y=th_y, mod_spec=mod_spec))
  }
}

#' @title Generate the confidence intervals for a given a cross-validated
#' ordinal variable
#'
#' @description This function returns the point estimate, 95% and 99%
#' confidence intervals for each ordinal stage of a given response variable.
#'
#' @param data_dir Directory where files are saved
#' @param analysis_name Unique identifier for current analysis
#' @param var_name Response variable name
#' @param th_x Parameterization for prior on x
#' @param input_seed An optional seed that can be used to make results
#'   reproducible. The input_seed must be either (1) NA / not provided (the
#'   default), (2) a single integer, or (3) a vector with length M+1 (see
#'   Description of calc_ci_ord for further details).
#' @param save_file Logical whether the resulting data frame should be saved
#' as an .rds file
#'
#' @export
generate_ord_ci <- function(data_dir, analysis_name, var_name,
                            th_x, input_seed=NA, save_file=F) {

  # Load the best ordinal model
  ord_model <- load_best_univariate_model(data_dir, analysis_name,
                                          var_name)

  # Calculate the confidence intervals and point estimate
  ci_df <- calc_ci_ord(ord_model, th_x, input_seed=input_seed)

  if(save_file) {
    saveRDS(ci_df, build_file_path(data_dir,
                                   analysis_name,
                                   "ordinal_ci",
                                   var_name=var_name))
  }
  
  return(ci_df)
}

#' @title Clear all the files in the temporary directory
#'
#' @description
#' Clear all the files in the temporary directory. Directories are not deleted.
#' clear_temp_dir is used in testing.
#'
#' @export
clear_temp_dir <- function() {
  for (f in dir(tempdir())) {
    full_path <- file.path(tempdir(),f)
    if (!dir.exists(full_path)) {
      success <- file.remove(full_path)
    }
  }
}

#' @title
#' Build a multivariate, conditionally independent model using the univariate
#' cross-validation results.
#'
#' @description
#' Build a multivariate, conditionally independent model using the univariate
#' cross-validation results. Optionally, the standard error of the
#' unconstrained parameter vector (th_y_bar) is calculated to support the
#' re-scaling done in fit_multivariate.
#'
#' @param data_dir Directory to save univariate model parameters
#' @param analysis_name analysis_name for file-naming
#' @param fold Fold number. If NA, build model from main solution.
#' @param calc_se Whether to calculate the standard error for th_y_bar
#' @param save_file Logical whether to save cindep model as an .rds file
#' @return A list with the parameter vector (th_y), model specification
#'   (mod_spec), and transformed parameter vector (th_y_bar). If calc_se is
#'   TRUE, the standard error for th_y_bar is added (th_y_bar_se).
#'
#' @export

build_cindep_model <- function(data_dir, analysis_name, fold=NA, calc_se=FALSE,
                               save_file=F) {
  problem_file <- build_file_path(data_dir,
                                  analysis_name,
                                  "main_problem")
  problem <- readRDS(problem_file)
  mod_spec <- problem$mod_spec

  J <- mod_spec$J
  K <- mod_spec$K

  a     <- c()
  tau   <- c()
  alpha <- c()
  mean_spec <- c()
  noise_spec <- c()


  if (calc_se) {
    a_scale_ord      <- c()
    tau_scale_ord    <- c()
    alpha_scale_ord  <- c()
    a_scale_cont     <- c()
    alpha_scale_cont <- c()
  }

  if (J > 0) {
    # ind_fix tracks which variables, if any, have problems with the Hessian
    ind_fix <- rep(NA,J)
    for (j in 1:J) {
      var_name <- problem$var_names[j]
      best_model <- load_best_univariate_model(data_dir,
                                               analysis_name,
                                               var_name,
                                               fold,
                                               load_data=calc_se)
      th_v       <- best_model$th_y
      mod_spec_j <- best_model$mod_spec
      a     <- c(a,th_v[get_var_index_univariate_ord("b",
                                                     mod_spec_j)])
      tau   <- c(tau, th_v[get_var_index_univariate_ord("tau",
                                                        mod_spec_j)])
      alpha <- c(alpha, th_v[get_var_index_univariate_ord("beta",
                                                          mod_spec_j)])
      mean_spec <- c(mean_spec, mod_spec_j$mean_spec)
      noise_spec <- c(noise_spec, mod_spec_j$noise_spec)

      if (calc_se) {
        ord_dummy <- function(th_v_bar, input_list) {
          return(calc_neg_log_lik_ord(th_v_bar,
                                      input_list$x,
                                      input_list$v,
                                      input_list$mod_spec,
                                      input_list$tf_cat_vect))
        }

        tf_cat_vect_j <-  get_univariate_ord_transform_categories(mod_spec_j)
        th_v_bar <- param_constr_to_unconstr(th_v, tf_cat_vect_j)
        input_list <- list(x=best_model$x,
                           v=best_model$y,
                           mod_spec=mod_spec_j,
                           tf_cat_vect=tf_cat_vect_j)
        H <- numDeriv::hessian(ord_dummy,
                               th_v_bar,
                               input_list=input_list)
        # If there are NA or NaN entries in H, there is some chance there was
        # a problem with the solution. For now, however, just try to fix the
        # scale rather than throwing an error.
        if(any(is.na(H))) {
          ind_fix[j] <- T
        } else {
          # Try to invert the matrix. If an error is encountered, add the
          # variable to ind_fix
          Hinv <- try(solve(H),silent=T)
          # TODO: consider checking the exact type of error message at this step
          #       to ensure that the error is because the matrix is singular.
          ind_fix[j] <- "try-error" %in% class(Hinv)
        }
        if (ind_fix[j]) {
          se <- rep(NA,length(th_v))
        } else {
          if (any(diag(Hinv) <= 0)) {
            stop(paste0("Not all diagonal entries of inv(H) are positive. ",
                        "Univariate solution may be a corner solution or may ",
                        "may not be an optimum for j=",j," and var_name=",
                        problem$var_names[j]))
          }
          se <- sqrt(diag(Hinv))
        }

        a_scale_ord   <-
           c(a_scale_ord,se[get_var_index_multivariate("a",
                                                       mod_spec_j,
                                                       j=1)])
        tau_scale_ord <-
           c(tau_scale_ord,se[get_var_index_multivariate("tau",
                                                         mod_spec_j,
                                                         j=1)])
        alpha_scale_ord <-
          c(alpha_scale_ord,se[get_var_index_multivariate("alpha",
                                                          mod_spec_j,
                                                          j=1)])
      }
    } # end for loop over j
    if (calc_se) {
      if (sum(ind_fix) == J) {
        # If all J ordinal variables need fixing, throw an error
        stop("Hessians are singular for all ordinal variables")
      }

      if (sum(ind_fix) > 0) {
        if (length(a_scale_ord) > 0) {
          if (all(is.na(a_scale_ord))) {
            stop("Cannot fix a_scale_ord")
          }
        }
        if (all(is.na(tau_scale_ord))) {
          stop("Cannot fix tau_scale_ord")
        }
        if (all(is.na(alpha_scale_ord))) {
          stop("Cannot fix alpha_scale_ord")
        }
        for (j in 1:J) {
          if(ind_fix[j]) {
            a_scale_ord[is.na(a_scale_ord)] <-
              median(a_scale_ord[!is.na(a_scale_ord)])
            tau_scale_ord[is.na(tau_scale_ord)] <-
              median(tau_scale_ord[!is.na(tau_scale_ord)])
            alpha_scale_ord[is.na(alpha_scale_ord)] <-
              median(alpha_scale_ord[!is.na(alpha_scale_ord)])
          }
        }
      }
    }
  } # end J>0 block

  if (K > 0) {
    # ind_fix tracks which variables, if any, have problems with the Hessian
    ind_fix <- rep(NA,K)
    for (k in 1:K) {
      var_name <- problem$var_names[J+k]
      best_model <- load_best_univariate_model(data_dir,
                                               analysis_name,
                                               var_name,
                                               fold,
                                               load_data=calc_se)
      th_w       <- best_model$th_y
      mod_spec_k <- best_model$mod_spec
      a     <- c(a,th_w[get_var_index_univariate_cont("c",
                                                     mod_spec_k)])
      alpha <- c(alpha, th_w[get_var_index_univariate_cont("kappa",
                                                          mod_spec_k)])
      mean_spec <- c(mean_spec, mod_spec_k$mean_spec)
      noise_spec <- c(noise_spec, mod_spec_k$noise_spec)

      if (calc_se) {
        cont_dummy <- function(th_w_bar, input_list) {
          return(calc_neg_log_lik_cont(th_w_bar,
                                       input_list$x,
                                       input_list$w,
                                       input_list$mod_spec,
                                       input_list$tf_cat_vect))
        }

        tf_cat_vect_k <-  get_univariate_cont_transform_categories(mod_spec_k)
        th_w_bar <- param_constr_to_unconstr(th_w, tf_cat_vect_k)
        input_list <- list(x=best_model$x,
                           w=best_model$y,
                           mod_spec=mod_spec_k,
                           tf_cat_vect=tf_cat_vect_k)
        H <- numDeriv::hessian(cont_dummy,
                               th_w_bar,
                               input_list=input_list)
        # If there are NA or NaN entries in H, there is some chance there was
        # a problem with the solution. For now, however, just try to fix the
        # scale rather than throwing an error.
        if(any(is.na(H))) {
          ind_fix[k] <- T
        } else {
          # Try to invert the matrix. If an error is encountered, add the
          # variable to ind_fix
          Hinv <- try(solve(H),silent=T)
          # TODO: consider checking the exact type of error message at this step
          #       to ensure that the error is because the matrix is singular.
          ind_fix[k] <- "try-error" %in% class(Hinv)
        }
        if (ind_fix[k]) {
          se <- rep(NA,length(th_v))
        } else {
            if (any(diag(Hinv) <= 0)) {
            stop(paste0("Not all diagonal entries of inv(H) are positive. ",
                        "Univariate solution may be a corner solution or may ",
                        "may not be an optimum for k=",k," and var_name=",
                        problem$var_names[J+k]))
          }
          se <- sqrt(diag(Hinv))
        }

        a_scale_cont <-
           c(a_scale_cont,se[get_var_index_multivariate("a",
                                                        mod_spec_k,
                                                        k=1)])
        alpha_scale_cont <-
          c(alpha_scale_cont,se[get_var_index_multivariate("alpha",
                                                           mod_spec_k,
                                                           k=1)])
      }
    } # end for loop over k
    if (calc_se) {
      if (sum(ind_fix) == K) {
        # If all K continuous variables need fixing, throw an error
        stop("Hessians are singular for all continuous variables")
      }

      if (sum(ind_fix) > 0) {
        if (all(is.na(a_scale_cont))) {
          stop("Cannot fix a_scale_cont")
        }
        if (all(is.na(alpha_scale_cont))) {
          stop("Cannot fix alpha_scale_cont")
        }
        for (k in 1:K) {
          if(ind_fix[k]) {
            a_scale_cont[is.na(a_scale_cont)] <-
              median(a_scale_cont[!is.na(a_scale_cont)])
            alpha_scale_cont[is.na(alpha_scale_cont)] <-
              median(alpha_scale_cont[!is.na(alpha_scale_cont)])
          }
        }
      }
    }
  } # end K>0 block

  mod_spec$mean_spec <- mean_spec
  mod_spec$noise_spec <- noise_spec
  mod_spec$cdep_spec <- "indep"

  tf_cat_vect <- get_multivariate_transform_categories(mod_spec)
  th_y <- c(a,tau,alpha)
  th_y_bar <- param_constr_to_unconstr(th_y, tf_cat_vect)
  output <- list(th_y=th_y,mod_spec=mod_spec,th_y_bar=th_y_bar)

  if(calc_se) {
    output$th_y_bar_se = c(a_scale_ord,
                           a_scale_cont,
                           tau_scale_ord,
                           alpha_scale_ord,
                           alpha_scale_cont)
  }

  if(save_file) {
    saveRDS(output, build_file_path(data_dir,analysis_name,"cindep_model",fold=fold))
  }

  return(output)
}

#' @title Calculate multivariate (cindep and cdep) negative-log-likelihoods
#' for cross-validation 
#' 
#' @description Function that loads the cindep and cdep models folds,  
#' performs negative-log-likelihood calculations, and returns vectors for
#' each corresponding model
#' 
#' @param data_dir Directory where the problem and model files are saved
#' @param analysis_name Unique identifier for current analysis
#' @param fold Fold number
#' 
#' @export

crossval_multivariate_models <- function(data_dir, analysis_name, fold) {
  # TODO: consider renaming this function to something like
  #       calc_out_of_sample_neg_log_liks
  cindep_soln <- readRDS(build_file_path(data_dir,
                                         analysis_name,
                                         "cindep_model",
                                         fold=fold))
  mod_spec_cindep <- cindep_soln$mod_spec
  th_y_cindep <- cindep_soln$th_y
  
  cdep_soln <- readRDS(build_file_path(data_dir,
                                      analysis_name,
                                      "cdep_model",
                                      fold=fold))
  mod_spec_cdep <- cdep_soln$mod_spec
  th_y_cdep <- cdep_soln$th_y

  # Load test data
  file_path <- build_file_path(data_dir,
                               analysis_name,
                               "test_problem",
                               fold=fold)
  problem <- readRDS(file_path)
  
  calc_data_cindep <- prep_for_neg_log_lik_multivariate(problem$x,
                                                        problem$Y,
                                                        mod_spec_cindep,
                                                        remove_log_ord=TRUE)
  calc_data_cdep   <- prep_for_neg_log_lik_multivariate(problem$x,
                                                        problem$Y,
                                                        mod_spec_cdep,
                                                        remove_log_ord=TRUE)

  # TODO: consider setting remove_log_ord=FALSE in the preceding step but
  #       requiring all entries to be non-null since the log_ord cases should
  #       already have been handled when creating the cross-validation training
  #       and test folds.
  eta_vect_cindep <- calc_neg_log_lik_vect_multivariate(th_y_cindep,
                                                        calc_data_cindep)
  eta_vect_cdep   <- calc_neg_log_lik_vect_multivariate(th_y_cdep,
                                                        calc_data_cdep)

  return(list(eta_vect_cindep=eta_vect_cindep,
              eta_vect_cdep=eta_vect_cdep))
}


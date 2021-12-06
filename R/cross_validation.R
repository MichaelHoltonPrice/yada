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
#' A helper function to identify univariate fit in a folder and determine the
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

#' @title Evaluation of univariate models
#' 
#' @description
#' [evaluate_univariate_models] utilizes either out-of-sample negative 
#' log-likelihood ("cv") or Akaike's Information Criterion ("aic") to 
#' rank univariate models. The ranking involves three considerations. First, 
#' ordinal models can be rejected for one of the reasons outlined below. 
#' Second, the models are ordered from best to worst by their evaluation 
#' values ("cv" or "aic"), which are stored in two arrays (see below): 
#' ord_array for ordinal variables and cont_array for continuous variables. 
#' Third, all models within cand_tol of the best model are considered 
#' equally good, and such models are re-ordered (if necessary) by their 
#' simplicity, where the order is lin_ord_const < lin_ord_lin_pos_int < 
#' log_ord_const < log_ord_lin_pos_int < pow_law_ord_const < 
#' pow_law_ord_lin_pos_int for ordinal variables and pow_law_const < 
#' pow_law_lin_pos_int for continuous variables. This function assumes that 
#' 
#' There are four reasons that ordinal models are rejected and not included 
#' in the final ranking:
#' (a) At least one of the folds failed to fit successfully (cv only)
#' (b) A log_ord model could not be fit
#' (c) The scaling exponent is close to zero (less than scale_exp_min), which 
#'     implies an identifiability problem
#' (d) The heteroskedastic noise term, beta2, is too large (greater than 
#'     beta2_max), which implies that the noise at x=0 tends to zero (relative 
#'     to the response)
#' 
#' The preceding rejection reasons are discussed in greater detail in the 
#' following publication:
#' 
#' TODO: add the final citation and link once available
#' 
#' Aside from the preceding four reasons, some models have a very small
#' heteroskedastic noise term, beta2, which could be added as another failure
#' term. However, such models are typically very close to constant models, and
#' thus typically rejected by the combination of using cand_tol and applying the
#' simplicity metric (this was the case for all variables in the publication
#' referenced above).
#'
#' There are no tailored rejection criteria for continuous models, except for 
#' failed fold fits (a).
#'
#' [evaluate_univariate_models] takes the following inputs:
#'
#' data_dir	      The directory with save files and in which to store the
#'                results of the evaluation
#' analysis_name  A "analysis_name" that uniquely specifies this set of models
#' eval_type      Whether the univariate models should be evaulated based on 
#'                out-of-sample cross-validation ("cv") or Akaike's 
#'                Information Criterion ("aic"). Default is "aic"
#' ord_models     A vector with the ordinal models to be evaluated
#' cont_models    A vector with the continuous models to be evaluated
#' scale_exp_min  The minimum acceptable value of the scaling exponent
#' cand_tol	      Candidate model tolerance. The best models are considered
#'                equally good if their respective out-of-sample negative
#'                log-likelihoods lie within cand_tol of each other. Model
#'                simplicity is then used as a "tie-breaker"
#' beta2_max      The maximum acceptable value of beta2, the heteroskedastic
#'                noise parameter for ordinal models
#'
#' The output of [evaluate_univariate_models] is a list with the following
#' named elements:
#' 
#' eval_type      Whether the univariate models should be evaulated based on 
#'                  out-of-sample cross-validation ("cv") or Akaike's 
#'                  Information Criterion ("aic"). Default is "aic".
#' ord_array   		Ordinal array with dimensions num_models_ord x num_folds x J 
#'                  for out-of-sample cross-validation ("cv") or dimensions 
#'                  num_models_ord x 1 x J for "aic"
#' cont_array 		Continuous array with dimensions num_models_cont x num_folds
#'                  x K for "cv" or dimensions num_models_cont x 1 x K for "aic"
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
#' ord_models		  A vector containing the known ordinal models
#' cont_models		A vector containing the known continuous models
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
#' @param eval_type Evaluation technique to use, either "cv" (out-of-sample 
#'   negative log-likelihood cross-validation) or "aic" (Akaike's Information 
#'   Criterion). Default is "aic".
#' @param ord_models A vector with the ordinal models to be evaluated
#' @param cont_models A vector with the continuous models to be evaluated
#' @param scale_exp_min The minimum acceptable value of the scaling exponent
#' @param cand_tol Candidate model tolerance. The best models are considered
#'   equally good if their respective out-of-sample negative log-likelihoods
#'   lie within cand_tol of each other. Model simplicity is then used as a
#'   "tie-breaker"
#' @param beta2_max The maximum acceptable value of beta2, the heteroskedastic
#'   noise parameter
#' @param save_file Logical, whether to save the list to data_dir. 
#'   (Default is TRUE)
#'
#' @return A list consisting of named elements that summarize the cross
#'   validation (see function description)
#'
#' @export
evaluate_univariate_models <- function(data_dir,
                                       analysis_name,
                                       eval_type,
                                       ord_models,
                                       cont_models,
                                       cand_tol,
                                       scale_exp_min,
                                       beta2_max,
                                       save_file=T) {
  
  # Begin Function
  if (!(eval_type %in% c("cv","aic"))) {
    stop("Current model evaluation type is not supported. Check variable 'eval_type'")
  }
  
  # Load base problem 
  prob0 <- readRDS(build_file_path(data_dir,
                                   analysis_name,
                                   "main_problem"))
  
  # Extract number of ordinal(J) and continuous(K) variables
  J <- get_J(prob0$mod_spec)
  K <- get_K(prob0$mod_spec)
  
  # Initialize a "vector" of length J+K, num_obs_vect, in which to store the
  # number of observations for each variable.
  num_obs_vect <- rep(NA,J+K)
  
  # Evaluate ordinal(J) models first
  if (J > 0) {
    num_models_ord <- length(ord_models)  # number of ordinal models
    
    # Initialize an array, ord_array, for ordinal evaluations:
    # aic: ord_array has dimensions num_models_ord x 1 x J and 
    #      stores the calculated AIC
    # cv: ord_array has dimensions num_models_ord x num_folds x J and 
    #     stores the out-of-sample negative log_likelihoods
    if (eval_type == "cv") {
      num_folds <- get_num_folds(data_dir, analysis_name)
      
      # Check that all variables have the same number of folds
      if (length(unique(num_folds)) != 1) {
        stop("All variables should have the same number of folds")
      } else {
        num_folds <- num_folds[1]
      }
      ord_array <- array(NA,c(num_models_ord,num_folds,J))
    } else {
      num_folds <- 0
      ord_array <- array(NA,c(num_models_ord,1,J))
    }
    
    # Initialize a list, param_list_ord, that stores all the observed parameter
    # vectors. Ideally, these would be stored in an object like a Matlab cell
    # array with dimensions num_models_ord x J. Sadly, R does not have such an
    # object, so instead use a list of lists, in which the first list has length
    # J and the elements of that list have length num_models_ord. Each element
    # of the second list is a matrix with dimensions num_param x (1+num_folds).
    param_list_ord <- list()
    
    # Initialize a "vector", can_do_log_ord, that indicates whether the log_ord
    # models can be fit for each ordinal variable
    can_do_log_ord <- rep(F,J)
    
    
    # Populate ord_array by looping over
    # (a) variables
    # (b) models
    # (c) folds, if applicable
    
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
      
      # Calculate either the out-of-sample negative log-likelihood (cv) or 
      # AIC for each model in ord_models
      # param_list_ord is comprised of J versions of param_sub_list
      param_sub_list <- list()
      
      for(n in 1:num_models_ord) {
        k_m <- ord_models[n]  # k_m == known model
        # Load the univariate model solution, soln0, initialize the parameter
        # matrix, and store the parameter vector from the main solution
        parsed_model <- parse_joined_model(k_m)
        soln0 <- readRDS(build_file_path(data_dir,
                                         analysis_name,
                                         "univariate_ord_soln",
                                         j=j,
                                         var_name=prob0$var_names[j],
                                         mean_spec=parsed_model[1],
                                         noise_spec=parsed_model[2]))
        
        # Check for failed fits, which are of class 'try-error'
        if (class(soln0) != "try-error") {
          have_param_mat <- T  # did not fail
          param_mat <- matrix(NA, length(soln0$th_y), 1+num_folds)
          param_mat[,1] <- soln0$th_y
          mod_spec <- soln0$mod_spec
        } else {
          have_param_mat <- F  # failed fit, no parameters to store
          param_mat <- matrix(NA,0,0)  # empty parameter matrix
        }
        
        # If eval_type=="cv", loop over folds to calculate the negative 
        # log-likelihood calculations, again dealing with failed fits
        if (eval_type == "cv") {
          for(f in 1:num_folds) {
            # Load the univariate solution for the current fold, f
            soln_f <- readRDS(build_file_path(data_dir,
                                              analysis_name,
                                              "univariate_ord_soln",
                                              j=j,
                                              var_name=prob0$var_names[j],
                                              mean_spec=parsed_model[1],
                                              noise_spec=parsed_model[2],
                                              fold=f))
            
            # Load the test data for this fold
            test_f <- readRDS(build_file_path(data_dir,
                                              analysis_name,
                                              "test_problem",
                                              fold=f))
            
            # Extract the out-of-sample observations and handle missing values
            x <- test_f$x
            v <- test_f$Y[j,]
            keep <- !is.na(v)
            x <- x[keep]
            v <- v[keep]
            
            if (class(soln_f) != "try-error") {
              # Sometimes the main solution or preceeding fold fits failed, 
              # but this one did not
              if (!have_param_mat) {
                have_param_mat <- T  # did not fail
                param_mat <- matrix(NA, length(soln_f$th_y), 1+num_folds)
                mod_spec <- soln_f$mod_spec
                
              }
              
              # Store the parameter vector and calculate negative log-likelihood
              # for the current fold
              param_mat[,1+f] <- soln_f$th_y
              ord_array[n,f,j] <- calc_neg_log_lik_ord(soln_f$th_y,
                                                       x,
                                                       v,
                                                       mod_spec)
            } else {
              ord_array[n,f,j] <- NA
            }
          }
        } else {  # calculate AIC for the main univariate solution
          # Check for a failed main solution
          if (class(soln0) != "try-error") {
            eta <- calc_neg_log_lik_ord(soln0$th_y,
                                        x,
                                        v,
                                        mod_spec)
            ord_array[n,1,j] <- calc_aic(length(soln0$th_y), eta)
          } else {
            ord_array[n,1,j] <- NA
          }
        }
        
        # Add names for the rows and columns of param_mat, add to param_sub_list
        if (nrow(param_mat) > 0) {
          rows <- c()
          # number of parameters for the mean, b
          num_b <- get_num_var_univariate_ord("b", mod_spec)
          if (num_b > 0) {
            for(n_b in 1:num_b) {
              rows <- c(rows,paste0("b",n_b))
            }
          }
          # number of tau parameters 
          num_tau <- get_num_var_univariate_ord("tau", mod_spec)
          for(n_tau in 1:num_tau) {
            rows <- c(rows, paste0("tau",n_tau))
          }
          # number of parameters for the noise, beta
          num_beta <- get_num_var_univariate_ord("beta", mod_spec)
          for(n_beta in 1:num_beta) {
            rows <- c(rows, paste0("beta",n_beta))
          }
          rownames(param_mat) <- rows
          if (eval_type == "cv") {
            colnames(param_mat) <- 
              c("main",unlist(lapply(1:num_folds,function(f){paste0("fold",f)})))
          } else {
            colnames(param_mat) <- "main"
          }
        }
        param_sub_list[[length(param_sub_list)+1]] <- param_mat
      }
      # Add param_sub_list to the "parent" list
      param_list_ord[[j]] <- param_sub_list
    }
    
    # Model selection for ordinal models using either cv or AIC, storing 
    # results in mod_select_ord, a list of length J in which each element is 
    # a data.frame with num_models_ord as rows and the following columns:
    # 
    # reject            Whether or not to reject the model for some reason
    # reject_reason     The reason for rejection (if applicable)
    # neg_log_lik       Out-of-sample negative log-likelihood (cv only)
    # delta_neg_log_lik Out-of-sample negative log-likelihood with minimum 
    #                       value subtracted (cv only)
    # aic               AIC calculation (aic only)
    # model_rank        The model's relative rank (only non-rejected models)
    
    mod_select_ord <- list()
    for(j in 1:J) {
      # Initialize the selection data.frame depending on evaluation type
      if (eval_type == "cv") {
        selection_df <- data.frame(model = ord_models,
                                   reject = rep(F, num_models_ord),
                                   reject_reason = rep("", num_models_ord),
                                   neg_log_lik = rowSums(ord_array[,,j]),
                                   delta_neg_log_lik = rowSums(ord_array[,,j]) - 
                                     min(rowSums(ord_array[,,j]), na.rm=T),
                                   model_rank = rep(NA, num_models_ord))
      } else {
        selection_df <- data.frame(model = ord_models, 
                                   reject = rep(F, num_models_ord),
                                   reject_reason = rep("", num_models_ord),
                                   aic = ord_array[,,j],
                                   delta_aic = ord_array[,,j] - 
                                     min(ord_array[,,j], na.rm=T),
                                   model_rank = rep(NA, num_models_ord))
      }
      
      # Reject for failed fold fits (cv only)
      if (eval_type == "cv") {
        ind_bad <- !is.finite(selection_df$neg_log_lik)  # covers NA, -Inf, Inf
        if(sum(ind_bad) > 0) {
          selection_df$reject[ind_bad] <- T
          selection_df$reject_reason[ind_bad] <- "Fold fit(s)"
        }
      }
      
      # Reject for failed main fit
      for(i in 1:num_models_ord) {
        if (is.na(sum(param_list_ord[[j]][[i]]))){
          selection_df$reject[i] <- T
          selection_df$reject_reason[i] <- "Main fit"
        }
      }
      
      # "Reject" log_ord models that cannot be fit. This supersedes failed fits.
      if (!can_do_log_ord[j]) {
        log_idx <- grep("log", ord_models)
        selection_df$reject[log_idx] <- T
        selection_df$reject_reason[log_idx] <- "log_ord"
      }
      
      # Reject pow_law_ord models for which the scaling exponent term is too small
      # (relative to the input scale_exp_min). This supersedes failed fits.
      pow_law_idx <- grep("pow_law", ord_models)
      if (any( dim(param_list_ord[[j]][[pow_law_idx[1]]]) != c(0,0)) ) {
        model1_failures <- which(param_list_ord[[j]][[pow_law_idx[1]]][1,] < scale_exp_min)
        if(any(model1_failures)) {
          selection_df$reject[pow_law_idx[1]] <- T
          selection_df$reject_reason[pow_law_idx[1]] <- "Scaling exponent"
        }
      }
      
      if (any( dim(param_list_ord[[j]][[pow_law_idx[2]]]) != c(0,0)) ) {
        model2_failures <- which(param_list_ord[[j]][[pow_law_idx[2]]][1,] < scale_exp_min)
        if(any(model2_failures)) {
          selection_df$reject[pow_law_idx[2]] <- T
          selection_df$reject_reason[pow_law_idx[2]] <- "Scaling exponent"
        }
      }
      # Reject heteroskedastic models for which the intercept term is too large.
      lin_pos_int_idx <- grep("lin_pos_int", ord_models)
      for(m in lin_pos_int_idx) {
        if (!selection_df$reject[m]) {
          if (fail_for_beta2(param_list_ord[[j]][[m]],beta2_max)) {
            selection_df$reject[m] <- T
            selection_df$reject_reason[m] <- 'Large beta2'
          }
        }
      }
      
      # Rank non-rejected models by:
      # (1) ordering by out-of-sample negative log-likelihood or aic
      # (2) identifying models within cand_tol of the best model
      # (3) ordering models from step (2) by simplicity
      
      # Remove rejected models, rank by smallest out-of-sample negative
      # log-likelihood or aic
      ind_ok <- selection_df$reject == F
      selection_df_sub <- selection_df[ind_ok,]
      ord_models_ok <- ord_models[ind_ok]
      if (eval_type == "cv") {
        delta_vec <- selection_df_sub$delta_neg_log_lik
      } else {
        delta_vec <- selection_df_sub$delta_aic
      }
      model_rank <- rank(delta_vec, na.last="keep")
      selection_df$model_rank[ind_ok] <- model_rank
      
      # If necessary, apply the cand_tol criterion
      if (any(delta_vec[-which(delta_vec == 0)] <= cand_tol)) {
        # Then must consider model simplicity for the best models (only the
        # best models per cand_tol are re-ordered.
        ind_best <- which(delta_vec <= cand_tol)
        ind_other <- which(delta_vec > cand_tol)
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
        rank_ok <- 1:length(model_rank)
      }
      rank_ok <- rank_ok[model_rank]
      selection_df$model_rank[ind_ok] <- rank_ok
      
      mod_select_ord[[length(mod_select_ord)+1]] <- selection_df  # add to list
    }
  }  # finish ordinal models
  
  if (K > 0) {
    num_models_cont <- length(cont_models)  # number of continuous models
    
    # Initialize an array, cont_array, for continuous evaluations:
    # aic: cont_array has dimensions num_models_cont x 1 x K and 
    #      stores the calculated AIC
    # cv: cont_array has dimensions num_models_cont x num_folds x K and 
    #     stores the out-of-sample negative log_likelihoods
    if (eval_type == "cv") {
      num_folds <- get_num_folds(data_dir, analysis_name)
      
      # Check that all variables have the same number of folds
      if (length(unique(num_folds)) != 1) {
        stop("All variables should have the same number of folds")
      } else {
        num_folds <- num_folds[1]
      }
      cont_array <- array(NA,c(num_models_cont,num_folds,K))
    } else {
      num_folds <- 0
      cont_array <- array(NA,c(num_models_cont,1,K))
    }
    
    # Initialize a list, param_list_cont, that stores all the observed parameter
    # vectors. Ideally, these would be stored in an object like a Matlab cell
    # array with dimensions num_models_cont x K. Sadly, R does not have such an
    # object, so instead use a list of lists, in which the first list has length
    # J and the elements of that list have length num_models_cont. Each element
    # of the second list is a matrix with dimensions num_param x (1+num_folds).
    param_list_cont <- list()
    
    # Populate cont_array by looping over
    # (a) variables
    # (b) models
    # (c) folds, if applicable
    for(k in 1:K) {
      # Extract the original data and determine the number of observations
      x <- prob0$x
      w <- prob0$Y[J+k,]
      keep <- !is.na(w)
      x <- x[keep]
      w <- w[keep]
      
      num_obs_vect[J+k] <- length(w)
      
      # Calculate either the out-of-sample negative log-likelihood (cv) or 
      # AIC for each model in cont_models
      # param_list_cont is comprised of K versions of param_sub_list 
      param_sub_list <- list()
      
      for(n in 1:num_models_cont) {
        k_m <- cont_models[n]  # k_m == known model
        # Load the univariate model solution, soln0, initialize the parameter
        # matrix, and store the parameter vector from the main solution
        parsed_model <- parse_joined_model(k_m)
        soln0 <- readRDS(build_file_path(data_dir,
                                         analysis_name,
                                         "univariate_cont_soln",
                                         k=k,
                                         var_name=prob0$var_names[J+k],
                                         mean_spec=parsed_model[1],
                                         noise_spec=parsed_model[2]))
        
        # Check for failed fits, which are of class 'try-error'
        if (class(soln0) != "try-error") {
          have_param_mat <- T  # did not fail
          param_mat <- matrix(NA, length(soln0$th_y), 1+num_folds)
          param_mat[,1] <- soln0$th_y
          mod_spec <- soln0$mod_spec
        } else {
          have_param_mat <- F  # failed fit, no parameters to store
          param_mat <- matrix(NA,0,0)  # empty parameter matrix
        }
        
        # If eval_type=="cv", loop over folds to calculate the negative 
        # log-likelihood calculations, again dealing with failed fits
        if (eval_type == "cv") {
          for(f in 1:num_folds) {
            # Load the univariate solution for the current fold, f
            soln_f <- readRDS(build_file_path(data_dir,
                                              analysis_name,
                                              "univariate_cont_soln",
                                              k=k,
                                              var_name=prob0$var_names[J+k],
                                              mean_spec=parsed_model[1],
                                              noise_spec=parsed_model[2],
                                              fold=f))
            
            # Load the test data for this fold
            test_f <- readRDS(build_file_path(data_dir,
                                              analysis_name,
                                              "test_problem",
                                              fold=f))
            
            # Extract the out-of-sample observations and handle missing values
            x <- test_f$x
            w <- test_f$Y[J+k,]
            keep <- !is.na(w)
            x <- x[keep]
            w <- w[keep]
            
            if (class(soln_f) != "try-error") {
              # Sometimes the main solution or preceding fold fits failed, 
              # but this one did not
              if (!have_param_mat) {
                have_param_mat <- T  # did not fail
                param_mat <- matrix(NA, length(soln_f$th_y), 1+num_folds)
                mod_spec <- soln_f$mod_spec
                
              }
              
              # Store the parameter vector and calculate negative log-likelihood
              # for the current fold
              param_mat[,1+f] <- soln_f$th_y
              cont_array[n,f,k] <- calc_neg_log_lik_cont(soln_f$th_y,
                                                         x,
                                                         w,
                                                         mod_spec)
            } else {
              cont_array[n,f,k] <- NA
            }
          }
        } else {  # calculate AIC for the main univariate solution
          # Check for a failed main solution
          if (class(soln0) != "try-error") {
            eta <- calc_neg_log_lik_cont(soln0$th_y,
                                         x,
                                         w,
                                         mod_spec)
            cont_array[n,1,k] <- calc_aic(length(soln0$th_y), eta)
          } else {
            cont_array[n,1,k] <- NA
          }
        }
        
        # Add names for the rows and columns of param_mat, add to param_sub_list
        if (nrow(param_mat) > 0) {
          rows <- c()
          # number of parameters for the mean, c
          num_c <- get_num_var_univariate_cont("c", mod_spec)
          if (num_c > 0) {
            for(n_c in 1:num_c) {
              rows <- c(rows,paste0("c",n_c))
            }
          }
          # number of parameters for the noise, kappa
          num_kappa <- get_num_var_univariate_cont("kappa", mod_spec)
          for(n_kappa in 1:num_kappa) {
            rows <- c(rows, paste0("kappa",n_kappa))
          }
          rownames(param_mat) <- rows
          if (eval_type == "cv") {
            colnames(param_mat) <- 
              c("main",unlist(lapply(1:num_folds,function(f){paste0("fold",f)})))
          } else {
            colnames(param_mat) <- "main"
          }
        }
        param_sub_list[[length(param_sub_list)+1]] <- param_mat
      }
      # Add param_sub_list to the "parent" list
      param_list_cont[[k]] <- param_sub_list
    }
    
    # Model selection for continuous models using either cv or aic, storing 
    # results in mod_select_cont, a list of length J in which each element is 
    # a data.frame with num_models_cont as rows and the following columns:
    # 
    # reject            Whether or not to reject the model for some reason
    # reject_reason     The reason for rejection (if applicable)
    # neg_log_lik       Out-of-sample negative log-likelihood (cv only)
    # delta_neg_log_lik Out-of-sample negative log-likelihood with minimum 
    #                       value subtracted (cv only)
    # aic               AIC calculation (aic only)
    # model_rank        The model's relative rank (only non-rejected models)
    
    mod_select_cont <- list()
    for(k in 1:K) {
      # Initialize the selection data.frame depending on evaluation type
      if (eval_type == "cv") {
        selection_df <- data.frame(model = cont_models,
                                   reject = rep(F, num_models_cont),
                                   reject_reason = rep("", num_models_cont),
                                   neg_log_lik = rowSums(cont_array[,,k]),
                                   delta_neg_log_lik = rowSums(cont_array[,,k]) - 
                                     min(rowSums(cont_array[,,k]), na.rm=T),
                                   model_rank = rep(NA, num_models_cont))
      } else {
        selection_df <- data.frame(model = cont_models, 
                                   reject = rep(F, num_models_cont),
                                   reject_reason = rep("", num_models_cont),
                                   aic = cont_array[,,k],
                                   delta_aic = cont_array[,,k] - 
                                     min(cont_array[,,k], na.rm=T),
                                   model_rank = rep(NA, num_models_cont))
      }
      
      # Reject for failed fold fits (cv only)
      if (eval_type == "cv") {
        ind_bad <- !is.finite(selection_df$neg_log_lik)  # covers NA, -Inf, Inf
        if(sum(ind_bad) > 0) {
          selection_df$reject[ind_bad] <- T
          selection_df$reject_reason[ind_bad] <- "Fold fit(s)"
        }
      }
      
      # Reject heteroskedastic models for which the intercept term is too large.
      lin_pos_int_idx <- grep("lin_pos_int", cont_models)
      for(m in lin_pos_int_idx) {
        if (!selection_df$reject[m]) {
          if (fail_for_beta2(param_list_cont[[k]][[m]],beta2_max)) {
            selection_df$reject[m] <- T
            selection_df$reject_reason[m] <- 'Large beta2'
          }
        }
      }
      
      # Rank non-rejected models by:
      # (1) ordering by out-of-sample negative log-likelihood or aic
      # (2) identifying models within cand_tol of the best model
      # (3) ordering models from step (2) by simplicity
      
      # Remove rejected models, rank by smallest out-of-sample negative
      # log-likelihood or aic
      ind_ok <- selection_df$reject == F
      selection_df_sub <- selection_df[ind_ok,]
      ord_models_ok <- ord_models[ind_ok]
      if (eval_type == "cv") {
        delta_vec <- selection_df_sub$delta_neg_log_lik
      } else {
        delta_vec <- selection_df_sub$delta_aic
      }
      model_rank <- rank(delta_vec, na.last="keep")
      selection_df$model_rank[ind_ok] <- model_rank
      
      # If necessary, apply the cand_tol criterion
      if (any(delta_vec[-which(delta_vec == 0)] <= cand_tol)) {
        # Then must consider model simplicity for the best models (only the
        # best models per cand_tol are re-ordered.
        ind_best <- which(delta_vec <= cand_tol)
        ind_other <- which(delta_vec > cand_tol)
        pref_ordering <- c('pow_law_ord_const',
                           'pow_law_ord_lin_pos_int')
        rank_best <- order(unlist(
          lapply(ord_models_ok[ind_best],
                 function(k_m){which(k_m==pref_ordering)})))
        rank_other <- length(rank_best) + 1:length(ind_other)
        rank_ok <- c(rank_best,rank_other)
      } else {
        rank_ok <- 1:length(model_rank)
      }
      rank_ok <- rank_ok[model_rank]
      selection_df$model_rank[ind_ok] <- rank_ok
      
      mod_select_cont[[length(mod_select_cont)+1]] <- selection_df
    }
  }
  
  # Create, save, and return the output list
  eval_data <- list(
    eval_type       = eval_type,
    ord_array       = ord_array,
    cont_array      = cont_array,
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
  
  if (save_file) {
    saveRDS(eval_data, build_file_path(data_dir, analysis_name, "eval_data"))
  }
  
  return(eval_data)
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
  eval_data <- readRDS(build_file_path(data_dir,
                                     analysis_name,
                                     "eval_data"))
  eval_type <- eval_data$eval_type

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
  write(paste0('title: "Model evaluation report for ',
               prob0$var_names[j],
               ' (j = ',j,')"'),
        file=rm_file,append=T)
  write('output: html_document',file=rm_file,append=T)
  write('---',file=rm_file,append=T)

  # Add summary information
  write('# Summary information',file=rm_file,append=T)
  write(paste0(eval_data$num_obs_vect[j],
               ' non-missing observations  '),
        file=rm_file,append=T)
  write(paste0(prob0$mod_spec$M[j]+1,' ordinal categories  '),
        file=rm_file,append=T)
  write(paste0('Candidate model tolerance is cand_tol = ',
               eval_data$cand_tol,
               '  '),
        file=rm_file,
        append=T)
  write(paste0('Minimum scaling exponent size is scale_exp_min = ',
               eval_data$scale_exp_min,'  '),
        file=rm_file,
        append=T)
  write(paste0('Maxiumum value for beta2 is beta2_max = ',
               eval_data$beta2_max,'  '),
        file=rm_file,
        append=T)

  if(!eval_data$can_do_log_ord[j]) {
    write(paste0('log_ord models cannot be fit since cases ',
                 'exist for which v > 0 when x=0'),
          file=rm_file,
          append=T)
  }
  write('',file=rm_file,append=T)
  
  if (eval_type == "cv") {
    write('# Negative log-likelihood by model and fold',file=rm_file,append=T)
    cv_matrix <- eval_data$ord_array[,,j]
    colnames(cv_matrix) <- unlist(lapply(1:eval_data$num_folds,
                                         function(f){paste0('fold',f)}))
    rownames(cv_matrix) <- unlist(lapply(eval_data$ord_models,
                                         function(k_m){gsub('_','-',k_m)}))
    write_matrix(cv_matrix,rm_file)
  } else {
    write("# Akaike's Information Criterion by model",file=rm_file,append=T)
    aic_matrix <- data.frame(eval_data$ord_array[,,j])
    colnames(aic_matrix) <- "main"
    rownames(aic_matrix) <- unlist(lapply(eval_data$ord_models,
                                          function(k_m){gsub('_','-',k_m)}))
    write_matrix(aic_matrix,rm_file)
  }

  write('# Automated model selection',file=rm_file,append=T)
  write_matrix(eval_data$mod_select_ord[[j]],rm_file)

  write('# Univariate fits for each model',file=rm_file,append=T)
  # For each model, output the matrix of parameter values, which has dimensions
  # num_param x (1+num_folds)
  for(m in 1:length(eval_data$ord_models)) {
    k_m <- eval_data$ord_models[m]
    write(paste0('## ',k_m,'  '),file=rm_file,append=T)
    param_matrix <- eval_data$param_list_ord[[j]][[m]]
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
  eval_data <- readRDS(build_file_path(data_dir,
                                     analysis_name,
                                     "eval_data"))
  eval_type <- eval_data$eval_type

  # Open an rmarkdown file to programatically generate a report (deleting the
  # file if it already exists)
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
  write(paste0('title: "Model evaluation report for ',prob0$var_names[J+k],' (k = ',k,')"'),file=rm_file,append=T)
  write('output: html_document',file=rm_file,append=T)
  write('---',file=rm_file,append=T)

  # Add summary information
  write('# Summary information',file=rm_file,append=T)
  write(paste0(eval_data$num_obs_vect[J+k],' non-missing observations  '),file=rm_file,append=T)
  write(paste0('Candidate model tolerance is cand_tol = ',eval_data$cand_tol,'  '),file=rm_file,append=T)

  write('',file=rm_file,append=T)
  
  if (eval_type == "cv") {
    write('# Negative log-likelihood by model and fold',file=rm_file,append=T)
    cv_matrix <- eval_data$cont_array[,,k]
    colnames(cv_matrix) <- unlist(lapply(1:eval_data$num_folds,
                                         function(f){paste0('fold',f)}))
    rownames(cv_matrix) <- unlist(lapply(eval_data$cont_models,
                                         function(k_m){gsub('_','-',k_m)}))
    write_matrix(cv_matrix,rm_file)
  } else {
    write("# Akaike's Information Criterion by model",file=rm_file,append=T)
    aic_matrix <- data.frame(eval_data$cont_array[,,k])
    colnames(aic_matrix) <- "main"
    rownames(aic_matrix) <- unlist(lapply(eval_data$cont_models,
                                          function(k_m){gsub('_','-',k_m)}))
    write_matrix(aic_matrix,rm_file)
  }

  write('# Automated model selection',file=rm_file,append=T)
  write_matrix(eval_data$mod_select_cont[[k]],rm_file)

  write('# Univariate fits for each model',file=rm_file,append=T)
  # For each model, output the matrix of parameter values, which has dimensions
  # num_param x (1+num_folds)
  for(m in 1:length(eval_data$cont_models)) {
    k_m <- eval_data$cont_models[m]
    write(paste0('## ',k_m,'  '),file=rm_file,append=T)
    param_matrix <- eval_data$param_list_cont[[k]][[m]]
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

  # Get main problem
  problem0 <- readRDS(build_file_path(data_dir, analysis_name, "main_problem"))
  
  # Initialize empty vectors
  variable <- c()
  var_type <- c()
  mean_spec <- c()
  noise_spec <- c()
  param_names <- c()
  best_params <- c()
  
  for (i in 1:length(problem0$var_names)) {
    var_name <- problem0$var_names[i]
    is_ord <- i <= problem0$mod_spec$J
    param_vec <- c()
    
    best_model <- tryCatch(
      {
        load_best_univariate_model(data_dir,
                                   analysis_name,
                                   var_name)
      },
      error=function(cond) {
        message(paste0(var_name," yielded no best model."))
        return("next")
      }
    )
    
    if (is.character(best_model)) {
      next
    }
    
    n_params <- length(best_model$th_y)
    if (is_ord) {
      num_b <- get_num_var_univariate_ord("b",best_model$mod_spec)
      if (num_b > 0) {
        for(n_b in 1:num_b) {
          param_vec <- c(param_vec, paste0("b",n_b))
        }
      }
      num_tau <- get_num_var_univariate_ord("tau",best_model$mod_spec)
      if (num_tau > 0) {
        for(n_tau in 1:num_tau) {
          param_vec <- c(param_vec, paste0("tau",n_tau))
        }
      }
      num_beta <- get_num_var_univariate_ord("beta",best_model$mod_spec)
      if (num_beta > 0) {
        for(n_beta in 1:num_beta) {
          param_vec <- c(param_vec, paste0("beta",n_beta))
        }
      }
    } else {
      num_c <- get_num_var_univariate_cont("c",best_model$mod_spec)
      if (num_c > 0) {
        for(n_c in 1:num_c) {
          param_vec <- c(param_vec, paste0("c",n_c))
        }
      }
      num_kappa <- get_num_var_univariate_cont("kappa",best_model$mod_spec)
      if (num_kappa > 0) {
        for(n_kappa in 1:num_kappa) {
          param_vec <- c(param_vec, paste0("kappa",n_kappa))
        }
      }
    }
    
    variable <- c(variable, rep(var_name, n_params))
    var_type <- c(var_type, rep(ifelse(is_ord, "Ord", "Cont"), n_params))
    mean_spec <- c(mean_spec, rep(best_model$mod_spec$mean_spec, n_params))
    noise_spec <- c(noise_spec, rep(best_model$mod_spec$noise_spec, n_params))
    param_names <- c(param_names, param_vec)
    best_params <- c(best_params, best_model$th_y)
    
  }
  
  final_output <- cbind(variable, var_type, mean_spec,
                        noise_spec, param_names, best_params)
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
#' @param load_data Whether to also load the data vector (x and y), and 
#'   mean-noise specifications (default: FALSE)
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
  # Load problem
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
  
  J <- get_J(problem$mod_spec)  # get number of ordinal variables
  
  # Load evaluation file
  eval_data <- readRDS(build_file_path(data_dir,
                                       analysis_name,
                                       "eval_data"))
  eval_type <- eval_data$eval_type
  
  # Identify the index of the variable of interest
  ind_var <- which(problem$var_names == var_name)
  if (length(ind_var) != 1) {
    stop("var_name should match exactly one entry in problem$var_names")
  }
  
  # Ordinal variables run from 1 to J and continuous variable run from J+1 to
  # J+K.
  is_ord <- ind_var <= J
  
  if (eval_type == "aic" & !is.na(fold)) {
    stop("AIC does not use folds. Check that fold=NA")
  }
  
  # Extract the correct selection data.frame from eval_data
  if (is_ord) {
    selection_df <- eval_data$mod_select_ord[[ind_var]]
  } else {
    selection_df <- eval_data$mod_select_cont[[ind_var-J]]
  }
  
  ind_best_model <- which(selection_df$model_rank == 1)  # best model
  
  # Checks that there is only one best model or if there are none
  if (length(ind_best_model) > 1) {
    stop("There should be exactly one best model")
  }
  if (length(ind_best_model) == 0) {
    stop("There is no best model to select")
  }
  
  # If AIC, check that fold==NA
  if (eval_type == "aic") {
    if (!is.na(fold)) {
      stop("AIC does not use folds")  # check that fold=NA
    }
  }

  # Parse the mean_noise model
  mean_noise <- parse_joined_model(selection_df$model[[ind_best_model]])
  
  # Load best solution, store th_y and mod_spec
  if (is_ord) {
    file_path <- build_file_path(data_dir, analysis_name,
                                 "univariate_ord_soln",
                                 j=ind_var,
                                 var_name=var_name,
                                 mean_spec=mean_noise[1],
                                 noise_spec=mean_noise[2])
  } else {
    file_path <- build_file_path(data_dir, analysis_name,
                                 "univariate_cont_soln",
                                 k=ind_var-J,
                                 var_name=var_name,
                                 mean_spec=mean_noise[1],
                                 noise_spec=mean_noise[2])
  }
  
  best_model <- readRDS(file_path)  # load best solution
  th_y <- best_model$th_y  # store th_y
  mod_spec <- best_model$mod_spec  # store mod_spec
  

  if(load_data) {
    x <- problem$x
    y <- problem$Y[ind_var,]
    ind_keep <- !is.na(x) & !is.na(y)
    x <- x[ind_keep]
    y <- y[ind_keep]
    return(list(th_y=th_y, mod_spec=mod_spec, x=x, y=y, mean_noise=mean_noise))
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
                            th_x, input_seed=NA, save_file=F, j=NA) {

  # Load the best ordinal model
  ord_model <- load_best_univariate_model(data_dir, analysis_name,
                                          var_name)

  # Calculate the confidence intervals and point estimate
  ci_df <- calc_ci_ord(ord_model, th_x, input_seed=input_seed)

  if(save_file) {
    saveRDS(ci_df, build_file_path(data_dir,
                                   analysis_name,
                                   "ordinal_ci",
                                   j=j,
                                   var_name=var_name))
  }
  
  return(ci_df)
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
#' @param allow_corner (Default: FALSE) If FALSE, an error is thrown if any 
#'   standard errors have negative values, which usually happens because an 
#'   optimum is a corner solution. If TRUE, an attempt is made to use 
#'   pertinent median values from other variables to set the scale.
#' @param remove_var (Default=FALSE) If FALSE, an error is thrown if any 
#'   univariate models yielded no best solution. If TRUE, variables are
#'   removed from the multivariate model and a record is kept.
#'   
#' @return A list with the parameter vector (th_y), model specification
#'   (mod_spec), and transformed parameter vector (th_y_bar). If calc_se is
#'   TRUE, the standard error for th_y_bar is added (th_y_bar_se).
#'
#' @export
build_cindep_model <- function(data_dir, analysis_name, fold=NA, calc_se=FALSE,
                               save_file=F, allow_corner=FALSE, 
                               remove_var=F) {
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
  
  if (remove_var) {
    var_gone <- c()
  }

  if (J > 0) {
    # ind_fix tracks which variables, if any, have problems with the Hessian
    ind_fix <- rep(NA,J)
    for (j in 1:J) {
      var_name <- problem$var_names[j]
      best_model <- tryCatch(
        { # Try
          load_best_univariate_model(data_dir, analysis_name, 
                                     var_name, fold,
                                     load_data=calc_se)
          
        },
        error=function(cond) {  # if Error
          if (remove_var) {
            return("next")
          } else {
            message(cond)
            return(cond)
          }
        }
      )
      
      if (is.character(best_model)) {
        if(best_model=="next") {
          var_gone <- c(var_gone, var_name)
          mod_spec$J <- mod_spec$J-1
          mod_spec$M <- mod_spec$M[-j]
          ind_fix[j] <- NA
          next
        } else {
          stop(best_model)
        }
      }
    
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
          if (any(diag(Hinv) <= 0) | any(is.nan(diag(Hinv)))) {
            msg <- paste0("Not all diagonal entries of inv(H) are positive. ",
                          "Univariate solution may be a corner solution or ",
                          "may not be an optimum for j=",j," and var_name=",
                          problem$var_names[j])
            if(!allow_corner) {
              stop(msg)
            } else {
              warning(msg)
              ind_fix[j] <- TRUE
              se <- rep(NA,length(th_v))
            }
          } else {
            # Hinv is fine
            se <- sqrt(diag(Hinv))
          }
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
    if (remove_var) {
      J <- mod_spec$J
    }
    
    if (calc_se) {
      ind_fix <- ind_fix[-which(is.na(ind_fix))]
      
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
  
  if (remove_var) {
    
    if (length(mean_spec) != J) {
      stop("Length of mean spec does not match number of kept variables.")
    }
  }

  if (K > 0) {
    # ind_fix tracks which variables, if any, have problems with the Hessian
    ind_fix <- rep(NA,K)
    for (k in 1:K) {
      var_name <- problem$var_names[J+k]
      best_model <- tryCatch(
        { # Try
          load_best_univariate_model(data_dir, analysis_name, 
                                     var_name, fold,
                                     load_data=calc_se)
          
        },
        error=function(cond) {  # if Error
          if (remove_var) {
            return("next")
          } else {
            message(cond)
            return(cond)
          }
        }
      )
      
      if (is.character(best_model)) {
        if(best_model=="next") {
          var_gone <- c(var_gone, var_name)
          mod_spec$K <- mod_spec$K-1
          ind_fix[k] <- NA
          next
        } else {
          stop(best_model)
        }
      }

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
              msg <- paste0("Not all diagonal entries of inv(H) are positive. ",
                            "Univariate solution may be a corner solution or may ",
                            "may not be an optimum for k=",k," and var_name=",
                            problem$var_names[J+k])
              if (!allow_corner) {
                stop(msg)
              } else {
                warning(msg)
                ind_fix[k] <- TRUE              }
          } else {
              # Hinv is fine
              se <- sqrt(diag(Hinv))
            }
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
    if (remove_var) {
      K <- mod_spec$K
    }
    
    if (calc_se) {
      ind_fix <- ind_fix[-which(is.na(ind_fix))]
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
  
  if (remove_var) {
    if (length(noise_spec) != J+K) {
      stop("Length of mean spec does not match number of kept variables.")
    }
  }

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
  
  if (remove_var) {
    output$removed_vars = var_gone
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

#' @title Calculate AIC 
#' 
#' @description Function that calculates the Akaike's Information Criterion 
#' given the number of parameters (k) and negative log-likelihood (eta)
#' 
#' @param k Number of parameters in model
#' @param eta Negative log-likelihood
#' 
#' @return AIC value
#' 
#' @export

calc_aic <- function(k, eta) {
  if (k%%1 != 0) {
    stop('k should be an integer')
  }
  if (k <= 0) {
    stop('k should be greater than 0')
  }
  
  aic <- (2*eta) + (2*k)
  return(aic)
}

#' @title Model selection using AIC
#'
#' @description
#' A wrapper function that imports all models given a variable, 
#' calculates the AIC for each model, and stores it in a list or dataframe. 
#'
#' @param data_dir Directory in which the model files are stored
#' @param analysis_name Unique ID for the given analysis
#' @param var_name Character string of the variable name for 
#' searching the directory
#' @param format_df Logical on whether the calculated AICs should be 
#' formatted as a dataframe. Default is format_df=FALSE, which maintains 
#' a list structure.
#' @param save_file Logical on whether the calculated AIC should be saved in
#' the directory as an .RDS file. Default is save_file=F.
#'
#' @return List or dataframe of the calculated AICs.
#'
#' @export

build_aic_output <- function(data_dir, analysis_name, var_name,
                           format_df=F, save_file=F) {
  # Check for solutiony files with var_name and import into list
  var_files <- intersect(list.files(path=data_dir,
                                    pattern=paste0('solutiony_',analysis_name)),
                         list.files(path=data_dir,pattern=var_name))
  fold_files <- grep('fold', var_files)  # find fold files
  if (length(fold_files) != 0) {
    sol_files <- var_files[-fold_files]  # remove fold files
  } else {
    sol_files <- var_files
  }
  
  # Load problem file, initialize values
  problem <- readRDS(build_file_path(data_dir, analysis_name,
                                     "main_problem", var_name=var_name))
  var_idx <- which(problem$var_names == var_name)
  keep <- !is.na(problem$Y[var_idx,])
  x <- problem$x[keep]
  y <- problem$Y[var_idx,keep]
  
  # Initialize empty list
  aic_list <- list()
  
  for (j in 1:length(sol_files)) {
    model <- readRDS(paste0(data_dir,'/',sol_files[j]))
    mean_noise <- strsplit(sol_files[j],paste0(var_name,'_|.rds'))[[1]][2]
    if (class(model) == 'try-error') {
      aic_list[mean_noise] <- NA
    } else {
      J <- get_J(model$mod_spec)
      K <- get_K(model$mod_spec)
      th_y <- model$th_y
      mod_spec <- model$mod_spec
      
      if (J+K > 1) {
        calc_data <- prep_for_neg_log_lik_multivariate(x,
                                                       y,
                                                       mod_spec,
                                                       remove_log_ord=TRUE)
        eta <- calc_neg_log_lik_multivariate(th_y, calc_data)
      } else {
        if (J == 1) {
          eta <- calc_neg_log_lik_ord(th_y, x, y, mod_spec)
        }
        if (K == 1) {
          eta <- calc_neg_log_lik_cont(th_y, x, y, mod_spec)
        }
      }
      
      # Calculate AIC
      aic_list[mean_noise] <- calc_aic(length(th_y), eta)
    }
  }
  
  if (format_df) {
    aic_df <- matrix(NA, nrow=length(aic_list), ncol=3)
    
    for (i in 1:nrow(aic_df)) {
      aic_df[i,1] <- names(aic_list[i])
      aic_df[i,2] <- aic_list[[i]]
    }
    
    aic_df <- as.data.frame(aic_df)
    colnames(aic_df) <- c('model', 'AIC', 'rank')
    aic_df$AIC <- as.numeric(aic_df$AIC)
    aic_df$rank <- floor(rank(aic_df$AIC, na.last='keep'))
    aic_out <- aic_df
  } else {
    aic_out <- aic_list
  }
  
  if (save_file) {
    save_path <- build_file_path(data_dir, analysis_name,
                                 "aic", var_name)
    saveRDS(aic_out, save_path)
  }
  
  return(aic_out)
}




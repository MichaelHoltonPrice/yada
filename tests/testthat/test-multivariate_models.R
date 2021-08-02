# It is most effective to test the functions in multivariate_models in groups
# for a range of models. The groups and models used are:

# Group 0 [these groups do not require a model]
# ------------
# renumber_groups
# get_non_singleton_groups
#
# ----------------
# Group 1 [3 models]
#   get_J
#   get_K
#   get_z_length
#   is_cdep
#   get_num_var_multivariate
#
# Models:
#   1. One variable ordinal
#   2. One variable continuous
#   3. Six variable, conditionally dependent
# ----------------
# Group 2 [1 model]
# get_var_index_multivariate
# get_var_index_multivariate_mapping
# get_var_index_multivariate_fast
# get_univariate_indices
# get_var_index_multivarate_mapping
# get_var_index_multivariate_fast
# remove_missing_variables
# prep_for_neg_log_lik_multivariate

# Models:
#   1. Six variable, conditionally dependent
# ----------------
# Group 3 [3 models]
# get_z_full_fast

# Models:
#   1. Four variable, conditionally dependent
#   2. Another four variable, conditionally dependent model
#   3. Yet another four variable, conditionally dependent model
# ----------------
# Group 4 [2 models]
# calc_cond_gauss_int_inputs
# calc_neg_log_lik_vect_multivariate
# calc_neg_log_lik_multivariate
# hjk_nll_wrapper
# calc_neg_log_lik_vect_multivariate_chunk_outer
# calc_neg_log_lik_vect_multivariate_chunk_inner [tested indirectly]
# calc_neg_log_lik_scalar_multivariate [tested indirectly]
# calc_joint
# calc_x_posterior

# Models:
#   1. A univariate ordinal model
#   2. A univariate continuous model
#   3. One ordinal and one continuous variable
#   4. Two ordinal and two continuous variable
#   5. Two ordinal and two continuous variable (a different four variable model)
# ----------------
# Group 5 [1 model]
# sim_multivariate
# fit_multivariate
# sample_x_posterior
# calc_x_posterior [additional tests]
# analyze_x_posterior
#
# Models:
#   1. A four variable model

# TODO: Add test of cdep_groups when there is exactly one group
# TODO: test with [1,NA,NA,NA,NA,NA] on a model where the first variable has no
#       mean parameter terms

# Set the number of cores
library(doParallel)
registerDoParallel(detectCores()-4)

# Clear the temporary directory
clear_temp_dir()

# Group 0 [these groups do not require a model]
# ------------
# renumber_groups
# get_non_singleton_groups

# Test renumber_groups
expect_equal(
  renumber_groups(1:10),
  1:10
)

expect_equal(
  renumber_groups(c(2,NA,2,1,NA,4,NA)),
  c(2,NA,2,1,NA,3,NA)
)

# Test get_non_singleton_groups
expect_equal(
  get_non_singleton_groups(1:10),
  numeric()
)

expect_equal(
  get_non_singleton_groups(c(1,2,NA,4,1,3,3)),
  c(1,3)
)

# Group 1
# ------------
# get_J
# get_K
# get_z_length
# is_cdep
# get_num_var_multivariate

# Group1: Model1
# A one variable, ordinal model -- pow_law_ord / const
mod_spec <- list()
mod_spec$mean_spec  <- 'pow_law_ord'
mod_spec$noise_spec <- 'const'
mod_spec$J <- 1
mod_spec$M <- 2

expect_equal(
  get_J(mod_spec),
  1
)

expect_equal(
  get_K(mod_spec),
  0
)

expect_equal(
  get_z_length(mod_spec),
  0
)

expect_equal(
  is_cdep(mod_spec),
  F 
)

expect_equal(
  get_num_var_multivariate('a',mod_spec),
  1
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,preceding=T),
  0
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,j=1),
  1
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,j=1,preceding=T),
  0
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,i=1),
  1
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,i=1,preceding=T),
  0
)

expect_error(
  get_num_var_multivariate('a',mod_spec,j=1,k=1),
  'If var_name is a or alpha and j is specified, k should not be specified'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,j=1,k=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,j=1,i=1),
  'If var_name is a or alpha and j is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,j=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,j=2),
  'j = 2 is greater than the number of ordinal variables J = 1'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,k=1),
  'k = 1 is greater than the number of continuous variables K = 0'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,i=2),
  'i = 2 is greater than the number of variables J+K = 1',
  fixed=TRUE
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec),
  2
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,preceding=T),
  1
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec),
  2
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,preceding=T),
  1
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,j=1),
  2
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,j=1,preceding=T),
  1
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,i=1),
  2
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,i=1,preceding=T),
  1
)

expect_error(
  get_num_var_multivariate('tau',mod_spec,j=1,k=1),
  'If var_name is tau, k should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',mod_spec,j=1,i=1),
  'If var_name is tau and j is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',mod_spec,j=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('tau',mod_spec,k=1),
  'If var_name is tau, k should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',mod_spec,j=2),
  'j = 2 is greater than the number of ordinal variables J = 1'
)
expect_equal(
  get_num_var_multivariate('alpha',mod_spec),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,preceding=T),
  3
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,j=1),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,j=1,preceding=T),
  3
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,i=1),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,i=1,preceding=T),
  3
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,j=1,k=1),
  'If var_name is a or alpha and j is specified, k should not be specified'
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,j=1,k=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,j=1,i=1),
  'If var_name is a or alpha and j is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,j=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,j=2),
  'j = 2 is greater than the number of ordinal variables J = 1'
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,k=1),
  'k = 1 is greater than the number of continuous variables K = 0'
)

expect_equal(
  get_num_var_multivariate('z',mod_spec),
  0
)

expect_equal(
  get_num_var_multivariate('z',mod_spec,preceding=T),
  4
)

expect_error(
  get_num_var_multivariate('notAVar',mod_spec),
  paste('Unrecognized variable notAVar')
)

# Group1: Model2
# A one variable, continuous model -- pow_law / const
mod_spec <- list()
mod_spec$mean_spec  <- 'pow_law'
mod_spec$noise_spec <- 'const'
mod_spec$K <- 1

expect_equal(
  get_J(mod_spec),
  0
)

expect_equal(
  get_K(mod_spec),
  1
)

expect_equal(
  get_z_length(mod_spec),
  0
)

expect_equal(
  is_cdep(mod_spec),
  F 
)

expect_equal(
  get_num_var_multivariate('a',mod_spec),
  3
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,preceding=T),
  0
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,k=1),
  3
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,k=1,preceding=T),
  0
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,i=1),
  3
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,i=1,preceding=T),
  0
)

expect_error(
  get_num_var_multivariate('a',mod_spec,j=1,k=1),
  'If var_name is a or alpha and j is specified, k should not be specified'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,j=1,k=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,j=1,i=1),
  'If var_name is a or alpha and j is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,j=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,k=2),
  'k = 2 is greater than the number of continuous variables K = 1'
)

expect_error(
  get_num_var_multivariate('a',mod_spec,j=1),
  'j = 1 is greater than the number of ordinal variables J = 0'
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec),
  0
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,preceding=T),
  3
)

expect_error(
  get_num_var_multivariate('tau',mod_spec,j=1,k=1),
  'If var_name is tau, k should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',mod_spec,j=1,i=1),
  'If var_name is tau and j is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',mod_spec,j=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('tau',mod_spec,k=1),
  'If var_name is tau, k should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',mod_spec,j=2),
  'j = 2 is greater than the number of ordinal variables J = 0'
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,preceding=T),
  3
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,k=1),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,k=1,preceding=T),
  3
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,i=1),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,i=1,preceding=T),
  3
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,j=1,k=1),
  'If var_name is a or alpha and j is specified, k should not be specified'
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,j=1,k=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,k=1,i=1),
  'If var_name is a or alpha and k is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,k=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,j=1),
  'j = 1 is greater than the number of ordinal variables J = 0'
)

expect_error(
  get_num_var_multivariate('alpha',mod_spec,k=2),
  'k = 2 is greater than the number of continuous variables K = 1'
)

expect_equal(
  get_num_var_multivariate('z',mod_spec),
  0
)

expect_equal(
  get_num_var_multivariate('z',mod_spec,preceding=T),
  4
)

expect_error(
  get_num_var_multivariate('notAVar',mod_spec),
  paste('Unrecognized variable notAVar')
)

# Group1: Model3
# A six variable, conditionally dependent model with log_ord
mod_spec <- list()
mod_spec$mean_spec=c('pow_law_ord','log_ord','pow_law_ord','pow_law','pow_law','pow_law')
mod_spec$noise_spec <- c('const','lin_pos_int','lin_pos_int',
                       'lin_pos_int','const','const')
mod_spec$J <- 3
mod_spec$K <- 3
mod_spec$M <- c(2,3,2)
mod_spec$cdep_spec <- 'dep'
mod_spec$cdep_groups <- c(1,2,1,3,NA,2)
# The low and high indices for each variable and index in the preceding model
# specification (blank indicates none/NA). ns-1 stands for non-singleton group
# 1 (etc.). inter-12 stands for intergroup correlation between 1 and 2 (etc.):
#
#
# low	high		Variable	j	k	i	i1	i2
# 1	1		a		1		1
# 			a		2		2
# 2	2		a		3		3
# 3	5		a			1	4
# 6	8		a			2	5
# 9	11		a			3	6
# 12	13		tau		1		1
# 14	16		tau		2		2
# 17	18		tau		3		3
# 19	19		alpha		1		1
# 20	21		alpha		2		2
# 22	23		alpha		3		3
# 24	25		alpha			1	4
# 26	26		alpha			2	5
# 27	27		alpha			3	6
# 28	28		z [non-singleton group 1]	1	3
# 29	29		z [non-singleton group 2]	2	6
# 30	30		z [inter 1-2            ]	1	2
# 31	31		z [inter 1-3            	1	4
# 32	32		z [inter 2-3            ]	2	4
expect_equal(
  get_J(mod_spec),
  3
)

expect_equal(
  get_K(mod_spec),
  3
)

expect_equal(
  get_z_length(mod_spec),
  5
)

expect_equal(
  is_cdep(mod_spec),
  T 
)

expect_equal(
  get_num_var_multivariate('a',mod_spec),
  11
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,preceding=T),
  0
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,j=3),
  1
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,j=3,preceding=T),
  1
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,i=3),
  1
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,i=3,preceding=T),
  1
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,k=3),
  3
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,k=3,preceding=T),
  8
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,i=6),
  3
)

expect_equal(
  get_num_var_multivariate('a',mod_spec,i=6,preceding=T),
  8
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec),
  7
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,preceding=T),
  11
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,j=3),
  2
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,j=3,preceding=T),
  16
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,i=3),
  2
)

expect_equal(
  get_num_var_multivariate('tau',mod_spec,i=3,preceding=T),
  16
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec),
  9
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,preceding=T),
  18
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,j=3),
  2
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,j=3,preceding=T),
  21
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,i=3),
  2
)

expect_equal(
  get_num_var_multivariate('alpha',mod_spec,i=3,preceding=T),
  21
)

expect_equal(
  get_num_var_multivariate('z',mod_spec),
  5
)

expect_equal(
  get_num_var_multivariate('z',mod_spec,preceding=T),
  27
)

# Group 2
# ------------
# get_var_index_multivariate
# get_var_index_multivariate_mapping
# get_var_index_multivariate_fast
# get_univariate_indices
# get_var_index_multivarate_mapping
# get_var_index_multivariate_fast
# remove_missing_variables
# prep_for_neg_log_lik_multivariate

# Group2: Model 1 (the only model used for this group)
# A six variable, conditionally dependent model
mod_spec <- list(mean_spec=c('log_ord','pow_law_ord','lin_ord',
                           'pow_law','pow_law','pow_law'))
mod_spec$noise_spec <- c('const','lin_pos_int','lin_pos_int',
                       'const','const','lin_pos_int')
mod_spec$J <- 3
mod_spec$K <- 3
mod_spec$M <- c(2,3,2)
mod_spec$cdep_spec <- 'dep' # conditionally dependent
mod_spec$cdep_groups <- c(1,2,1,3,NA,2)
# The low and high indices for each variable and index in the preceding model
# specification (blank indicates none/NA). ns-1 stands for non-singleton group
# 1 (etc.). inter-12 stands for intergroup correlation between 1 and 2 (etc.):
#
# low	high		Variable	j	k	i	i1	i2
#  	 		a		1		1
# 1	1		a		2		2
#  	 		a		3		3
# 2	4		a			1	4
# 5	7		a			2	5
# 8	10		a			3	6
# 11	12		tau		1		1
# 13	15		tau		2		2
# 16	17		tau		3		3
# 18	18		alpha		1		1
# 19	20		alpha		2		2
# 21	22		alpha		3		3
# 23	23		alpha			1	4
# 24	24		alpha			2	5
# 25	26		alpha			3	6
# 27	27		z-ns-1			1	3
# 28	28		z-ns-2			2	6
# 29	29		z-inter-12		1	2
# 30	30		z-inter-13		1	4
# 31	31		z-inter 23		2	4

expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

expect_equal(
  get_var_index_multivariate('a',mod_spec),
  1:10
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping),
  1:10
)

expect_equal(
  get_var_index_multivariate('tau',mod_spec),
  11:17
)

expect_equal(
  get_var_index_multivariate_fast('tau',mapping),
  11:17
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec),
  18:26
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping),
  18:26
)

expect_equal(
  get_var_index_multivariate('z',mod_spec),
  27:31
)

expect_equal(
  get_var_index_multivariate_fast('z',mapping),
  27:31
)

# Check functioning of index pattern errors

# The following are valid input patterns:
#
# j   k   i  i1  i2
# 0   0   0   0   0    no index
# 1   0   0   0   0    j specified
# 0   1   0   0   0    k specified
# 0   0   1   0   0    i specified
# 0   0   0   1   1    i1 and i2 specified

# 0 0 0 0 0
expect_error(
  get_var_index_multivariate('a',mod_spec),
  NA
)

expect_error(
  get_var_index_multivariate_fast('a',mapping),
  NA
)

expect_error(
  get_var_index_multivariate('tau',mod_spec),
  NA
)

expect_error(
  get_var_index_multivariate_fast('tau',mapping),
  NA
)

expect_error(
  get_var_index_multivariate('alpha',mod_spec),
  NA
)

expect_error(
  get_var_index_multivariate_fast('alpha',mapping),
  NA
)

# 1 0 0 0 0
expect_error(
  get_var_index_multivariate('a',mod_spec,j=2),
  NA
)

expect_error(
  get_var_index_multivariate_fast('a',mapping,j=2),
  NA
)


expect_error(
  get_var_index_multivariate('tau',mod_spec,j=2),
  NA
)

expect_error(
  get_var_index_multivariate_fast('tau',mapping,j=2),
  NA
)

expect_error(
  get_var_index_multivariate('alpha',mod_spec,j=2),
  NA
)

expect_error(
  get_var_index_multivariate_fast('alpha',mapping,j=2),
  NA
)

# 0 1 0 0 0
expect_error(
  get_var_index_multivariate('a',mod_spec,k=3),
  NA
)

expect_error(
  get_var_index_multivariate_fast('a',mapping,k=3),
  NA
)

expect_error(
  get_var_index_multivariate('alpha',mod_spec,k=3),
  NA
)

expect_error(
  get_var_index_multivariate_fast('alpha',mapping,k=3),
  NA
)

# 0 0 1 0 0
expect_error(
  get_var_index_multivariate('a',mod_spec,i=5),
  NA
)

expect_error(
  get_var_index_multivariate_fast('a',mapping,i=5),
  NA
)

expect_error(
  get_var_index_multivariate('tau',mod_spec,i=2),
  NA
)

expect_error(
  get_var_index_multivariate_fast('tau',mapping,i=2),
  NA
)

expect_error(
  get_var_index_multivariate('alpha',mod_spec,i=5),
  NA
)

expect_error(
  get_var_index_multivariate_fast('alpha',mapping,i=5),
  NA
)

# 0 0 0 1 1
expect_error(
  get_var_index_multivariate('z',mod_spec,i1=2,i2=4),
  NA
)

expect_error(
  get_var_index_multivariate_fast('z',mapping,i1=2,i2=4),
  NA
)

expect_error(
  get_var_index_multivariate('a',mod_spec,j=2,k=3),
  'Unsupported input pattern for index variables. See yada documentation'
)

expect_error(
  get_var_index_multivariate_fast('a',mapping,j=2,k=3),
  'Unsupported input pattern for index variables. See yada documentation'
)

expect_error(
  get_var_index_multivariate('tau',mod_spec,j=2,i=5),
  'Unsupported input pattern for index variables. See yada documentation'
)

expect_error(
  get_var_index_multivariate_fast('tau',mapping,j=2,i=5),
  'Unsupported input pattern for index variables. See yada documentation'
)

expect_error(
  get_var_index_multivariate('alpha',mod_spec,i=2,k=3),
  'Unsupported input pattern for index variables. See yada documentation'
)

expect_error(
  get_var_index_multivariate_fast('alpha',mapping,i=2,k=3),
  'Unsupported input pattern for index variables. See yada documentation'
)

# Check a for indices being specified
expect_equal(
  get_var_index_multivariate('a',mod_spec,j=1),
  c()
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,j=1),
  c()
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,j=2),
  1
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,j=2),
  1
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,j=3),
  c()
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,j=3),
  c()
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,k=1),
  2:4
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,k=1),
  2:4
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,k=2),
  5:7
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,k=2),
  5:7
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,k=3),
  8:10
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,k=3),
  8:10
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,i=1),
  c()
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,i=1),
  c()
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,i=2),
  1
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,i=2),
  1
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,i=3),
  c()
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,i=3),
  c()
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,i=4),
  2:4
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,i=4),
  2:4
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,i=5),
  5:7
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,i=5),
  5:7
)

expect_equal(
  get_var_index_multivariate('a',mod_spec,i=6),
  8:10
)

expect_equal(
  get_var_index_multivariate_fast('a',mapping,i=6),
  8:10
)

# Check tau for indices being specified
expect_equal(
  get_var_index_multivariate('tau',mod_spec,j=1),
  11:12
)

expect_equal(
  get_var_index_multivariate_fast('tau',mapping,j=1),
  11:12
)

expect_equal(
  get_var_index_multivariate('tau',mod_spec,j=2),
  13:15
)

expect_equal(
  get_var_index_multivariate_fast('tau',mapping,j=2),
  13:15
)

expect_equal(
  get_var_index_multivariate('tau',mod_spec,j=3),
  16:17
)

expect_equal(
  get_var_index_multivariate_fast('tau',mapping,j=3),
  16:17
)

expect_equal(
  get_var_index_multivariate('tau',mod_spec,i=1),
  11:12
)

expect_equal(
  get_var_index_multivariate_fast('tau',mapping,i=1),
  11:12
)

expect_equal(
  get_var_index_multivariate('tau',mod_spec,i=2),
  13:15
)

expect_equal(
  get_var_index_multivariate_fast('tau',mapping,i=2),
  13:15
)

expect_equal(
  get_var_index_multivariate('tau',mod_spec,i=3),
  16:17
)

expect_equal(
  get_var_index_multivariate_fast('tau',mapping,i=3),
  16:17
)

# Check alpha for indices being specified
expect_equal(
  get_var_index_multivariate('alpha',mod_spec,j=1),
  18
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,j=1),
  18
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,j=2),
  19:20
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,j=2),
  19:20
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,j=3),
  21:22
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,j=3),
  21:22
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,k=1),
  23
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,k=1),
  23
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,k=2),
  24
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,k=2),
  24
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,k=3),
  25:26
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,k=3),
  25:26
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,i=1),
  18
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,i=1),
  18
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,i=2),
  19:20
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,i=2),
  19:20
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,i=3),
  21:22
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,i=3),
  21:22
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,i=4),
  23
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,i=4),
  23
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,i=5),
  24
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,i=5),
  24
)

expect_equal(
  get_var_index_multivariate('alpha',mod_spec,i=6),
  25:26
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping,i=6),
  25:26
)

# Check z for indices being specified
expect_equal(
  get_var_index_multivariate('z',mod_spec,i1=1,i2=3),
  27
)

expect_equal(
  get_var_index_multivariate_fast('z',mapping,i1=1,i2=3),
  27
)

expect_equal(
  get_var_index_multivariate('z',mod_spec,i1=2,i2=6),
  28
)

expect_equal(
  get_var_index_multivariate_fast('z',mapping,i1=2,i2=6),
  28
)

expect_equal(
  get_var_index_multivariate('z',mod_spec,i1=1,i2=2),
  29
)

expect_equal(
  get_var_index_multivariate_fast('z',mapping,i1=1,i2=2),
  29
)


expect_equal(
  get_var_index_multivariate('z',mod_spec,i1=1,i2=4),
  30
)

expect_equal(
  get_var_index_multivariate_fast('z',mapping,i1=1,i2=4),
  30
)

expect_equal(
  get_var_index_multivariate('z',mod_spec,i1=6,i2=4),
  31
)

expect_equal(
  get_var_index_multivariate_fast('z',mapping,i1=6,i2=4),
  31
)

expect_error(
  get_var_index_multivariate('z',mod_spec,i1=2,i2=2),
  'i1 should not equal i2'
)

expect_error(
  get_var_index_multivariate_fast('z',mapping,i1=2,i2=2),
  'i1 should not equal i2'
)

expect_error(
  get_var_index_multivariate('z',mod_spec,i1=2,i2=5),
  'Correlation requested for a variable with no correlations'
)

expect_error(
  get_var_index_multivariate_fast('z',mapping,i1=2,i2=5),
  'Correlation requested for a variable with no correlations'
)

# Test get_univariate_indices with the same six variable model

# Check functioning of index pattern errors
# The following are valid input patterns:
#
# j   k   i
# 1   0   0    j specified
# 0   1   0    k specified
# 0   0   1    i specified

for(j in c(NA,2)) {
  for(k in c(NA,3)) {
    for(i in c(NA,6)) {
      if(!is.na(j) && is.na(k) && is.na(i)) {
        # 1 0 0
        expect_error(
          get_univariate_indices(mod_spec,j=j),
          NA
        )
      } else if(is.na(j) && !is.na(k) && is.na(i)) {
        # 0 1 0
        expect_error(
          get_univariate_indices(mod_spec,k=k),
          NA
        )
       } else if(is.na(j) && is.na(k) && !is.na(i)) {
        # 0 0 1
        expect_error(
          get_univariate_indices(mod_spec,i=i),
          NA
        )
      } else {
        # All other patterns
        expect_error(
          get_univariate_indices(mod_spec,j=j,k=k,i=i),
          'Unsupported input pattern for index variables. See yada documentation'
        )
      }
    }
  }
}

expect_equal(
  get_univariate_indices(mod_spec,j=1),
  c(11:12,18)
)

expect_equal(
  get_univariate_indices(mod_spec,i=1),
  c(11:12,18)
)

expect_equal(
  get_univariate_indices(mod_spec,j=2),
  c(1,13:15,19:20)
)

expect_equal(
  get_univariate_indices(mod_spec,i=2),
  c(1,13:15,19:20)
)

expect_equal(
  get_univariate_indices(mod_spec,j=3),
  c(16:17,21:22)
)

expect_equal(
  get_univariate_indices(mod_spec,i=3),
  c(16:17,21:22)
)

expect_equal(
  get_univariate_indices(mod_spec,k=1),
  c(2:4,23)
)

expect_equal(
  get_univariate_indices(mod_spec,i=4),
  c(2:4,23)
)

expect_equal(
  get_univariate_indices(mod_spec,k=2),
  c(5:7,24)
)

expect_equal(
  get_univariate_indices(mod_spec,i=5),
  c(5:7,24)
)

expect_equal(
  get_univariate_indices(mod_spec,k=3),
  c(8:10,25:26)
)

expect_equal(
  get_univariate_indices(mod_spec,i=6),
  c(8:10,25:26)
)

# Test get_multivariate_transform_categories on the preceding model
expect_equal(
  get_multivariate_transform_categories(mod_spec),
  c(1,         # a2
    1,1,0,     # a4
    1,1,0,     # a5
    1,1,0,     # a6
    0,3,       # tau1
    0,3,3,     # tau2
    0,3,       # tau3
    1,         # alpha1
    1,1,       # alpha2
    1,1,       # alpha3
    1,         # alpha4
    1,         # alpha5
    1,1,       # alpha6
    2,2,2,2,2) # z
)

# Test get_var_index_multivarate_mapping and get_var_index_multivariate_fast on
# the preceding model
# TODO: clarify the following note
# [Some of the preceding tests of get_var_index_multivariate_fast also checked
#  below.]

# Build the mapping, checking that no error is thrown
expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

# Check that correct error is thrown with an invalid input pattern
expect_error(
  get_var_index_multivariate_fast('a',mapping,j=1,k=1),
  'Unsupported input pattern for index variables. See yada documentation'
)

# Check for input pattern 0 0 0 0 0, no index
expect_equal(
  get_var_index_multivariate_fast('a',mapping),
  get_var_index_multivariate     ('a',mod_spec)
)

expect_equal(
  get_var_index_multivariate_fast('tau',mapping),
  get_var_index_multivariate     ('tau',mod_spec)
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping),
  get_var_index_multivariate     ('alpha',mod_spec)
)

expect_equal(
  get_var_index_multivariate_fast('z',mapping),
  get_var_index_multivariate     ('z',mod_spec)
)

# Check for input pattern 1 0 0 0 0, j specified
J <- get_J(mod_spec)
for(j in 1:J) {
  expect_equal(
    get_var_index_multivariate_fast('a',mapping,j=j),
    get_var_index_multivariate     ('a',mod_spec,j=j)
  )

  expect_equal(
    get_var_index_multivariate_fast('tau',mapping,j=j),
    get_var_index_multivariate     ('tau',mod_spec,j=j)
  )

  expect_equal(
    get_var_index_multivariate_fast('alpha',mapping,j=j),
    get_var_index_multivariate     ('alpha',mod_spec,j=j)
  )
}

# Check for input pattern 0 1 0 0 0, k specified
K <- get_J(mod_spec)
for(k in 1:K) {
  expect_equal(
    get_var_index_multivariate_fast('a',mapping,k=k),
    get_var_index_multivariate     ('a',mod_spec,k=k)
  )

  expect_equal(
    get_var_index_multivariate_fast('alpha',mapping,k=k),
    get_var_index_multivariate     ('alpha',mod_spec,k=k)
  )
}

# Check for input pattern 0 0 1 0 0, i specified
for(i in 1:(J+K)) {
  expect_equal(
    get_var_index_multivariate_fast('a',mapping,i=i),
    get_var_index_multivariate     ('a',mod_spec,i=i)
  )

  if(i <= J) {
    expect_equal(
      get_var_index_multivariate_fast('tau',mapping,i=i),
      get_var_index_multivariate     ('tau',mod_spec,i=i)
    )
  }

  expect_equal(
    get_var_index_multivariate_fast('alpha',mapping,i=i),
    get_var_index_multivariate     ('alpha',mod_spec,i=i)
  )
}

# Check for input pattern 0 0 0 1 1, i1 and i2 specified

counter <- 0 # using a counter is probably clearer than using combinadic indexing
for(i1 in 1:(J+K-1)) {
  for(i2 in (i1+1):(J+K)) {
    counter <- counter + 1
    if(!is.na(mod_spec$cdep_groups[i1]) && !is.na(mod_spec$cdep_groups[i2])) {
      expect_equal(
        get_var_index_multivariate_fast('z',mapping,i1=i1,i2=i2),
        get_var_index_multivariate     ('z',mod_spec,i1=i1,i2=i2)
      )
      expect_equal(
        get_var_index_multivariate_fast('z',mapping,i1=i1,i2=i2),
        get_var_index_multivariate_fast('z',mapping,i1=i2,i2=i1)
      )
    } else {
      expect_error(
        get_var_index_multivariate_fast('z',mapping,i1=i1,i2=i2),
        'Correlation requested for a variable with no correlations'
      )
    }
  }
}

# Test remove_missing_variables and prep_for_neg_log_lik_multivariate on the
# preceding model
y0_1 <- c(1,NA,0,10.5,11.5,NA)
mod_spec0 <- mod_spec
expect_error(
  reducedData1 <- remove_missing_variables(y0_1,mod_spec0),
  NA
)

expect_equal(
  names(reducedData1),
  c("y","mod_spec","mapping","mapping0","ind","keep")
)

expect_equal(
  reducedData1$y,
  c(1,0,10.5,11.5)
)

expect_equal(
  reducedData1$mod_spec,
  list(
       mean_spec    = c('log_ord','lin_ord','pow_law','pow_law'),
       noise_spec   = c('const','lin_pos_int','const','const'),
       J           = 2,
       K           = 2,
       M           = c(2,2),
       cdep_spec    = 'dep',
       cdep_groups  = c(1,1,2,NA)
      )
)

expect_equal(
  reducedData1$ind,
  c(
     2: 4, # a4,
     5: 7, # a5
    11:12, # tau1
    16:17, # tau3
       18, # alpha1
    21:22, # alpha3
       23, # alpha4
       24, # alpha5
       27, # z-cross-1
       30  # z-inter-13
   )
)

# The functioning of get_var_index_multivariate_mapping is separately checked,
# and can thus be used to check subsequent mappings
expect_equal(
  reducedData1$mapping,
  get_var_index_multivariate_mapping(reducedData1$mod_spec)
)

expect_equal(
  reducedData1$mapping0,
  get_var_index_multivariate_mapping(mod_spec0)
)

y0_2 <- c(0,1,NA,9.5,NA,NA)
expect_error(
  reducedData2 <- remove_missing_variables(y0_2,mod_spec0),
  NA
)

expect_equal(
  names(reducedData2),
  c("y","mod_spec","mapping","mapping0","ind","keep")
)

expect_equal(
  reducedData2$y,
  c(0,1,9.5)
)

expect_equal(
  reducedData2$mod_spec,
  list(
       mean_spec    = c('log_ord','pow_law_ord','pow_law'),
       noise_spec   = c('const','lin_pos_int','const'),
       J           = 2,
       K           = 1,
       M           = c(2,3),
       cdep_spec    = 'dep',
       cdep_groups  = c(1,2,3)
      )
)

expect_equal(
  reducedData2$ind,
  c(
        1, # a2,
     2: 4, # a4,
    11:12, # tau1
    13:15, # tau2
       18, # alpha1
    19:20, # alpha2
       23, # alpha4
    29:31  # z-inter-all
   )
)

expect_equal(
  reducedData2$mapping,
  get_var_index_multivariate_mapping(reducedData2$mod_spec)
)

expect_equal(
  reducedData2$mapping0,
  get_var_index_multivariate_mapping(mod_spec0)
)

# Check functioning of log_ord
y0_3 <- c(1,1,NA,9.5,NA,NA)
Y0 <- cbind(y0_1,y0_3)

# Expect error if x is zero for first observation
x <- c(3,0)

expect_error(
  calc_data <- prep_for_neg_log_lik_multivariate(x,Y0,mod_spec),
  'For variable j=1, cases exist where mean_spec is log_ord, x=0, and m>0'
)


# It's OK if x is zero for log_ord if m is zero
y0_4 <- c(0,1,NA,9.5,NA,NA)
Y0 <- cbind(y0_1,y0_4)

expect_error(
  calc_data <- prep_for_neg_log_lik_multivariate(x,Y0,mod_spec),
  NA
)

expect_equal(
  length(calc_data),
  2
)

for(n in 1:length(calc_data)) {
  expect_equal(
    names(calc_data[[n]]),
    c("x","y","mod_spec","mapping","mapping0","ind","keep")
  )
}

# update y0_4 for the special case with log_ord, then call
# remove_missing_variables directly (this is to check calc_data)
y0_4[1] <- NA 
expect_error(
  reducedData4 <- remove_missing_variables(y0_4,mod_spec0),
  NA
)

expect_equal(
  unlist(lapply(calc_data,function(cd){cd$x})),
  x
)

expect_equal(
  lapply(calc_data,function(cd){cd$y}),
  list(reducedData1$y,reducedData4$y)
)

expect_equal(
  lapply(calc_data,function(cd){cd$mod_spec}),
  list(reducedData1$mod_spec,reducedData4$mod_spec)
)

expect_equal(
  lapply(calc_data,function(cd){cd$mapping}),
  list(reducedData1$mapping,reducedData4$mapping)
)

expect_equal(
  lapply(calc_data,function(cd){cd$mapping0}),
  list(reducedData1$mapping0,reducedData4$mapping0)
)

expect_equal(
  lapply(calc_data,function(cd){cd$ind}),
  list(reducedData1$ind,reducedData4$ind)
)

expect_equal(
  lapply(calc_data,function(cd){cd$keep}),
  list(reducedData1$keep,reducedData4$keep)
)

# Group 3
# ------------
# get_z_full_fast

# Group3: Model 1
# A four variable, conditionally dependent model
mod_spec <- list(mean_spec=c('log_ord','pow_law_ord','pow_law','pow_law'))
mod_spec$noise_spec <- c('const','lin_pos_int','lin_pos_int','const')
mod_spec$J <- 2
mod_spec$M <- c(2,3)
mod_spec$K <- 2
mod_spec$cdep_spec <- 'dep' # conditionally dependent
mod_spec$cdep_groups <- c(1,2,1,2)

expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

zns      <- c(-.1,.2)
zcr      <- c(.05)
z        <- c(zns,zcr)
th_y <- c( 
  c(0.75),         # a2
  c(0.3,0.1,-.25), # a3
  c(0.8,1.2, .25), # a4
  c(-2.0,2.5),     # tau1
  c(-1.0,3.5,4.5), # tau2
  c(.2),           # alpha1
  c(.4,.03),       # alpha2
  c(.3,.02),       # alpha3
  c(.5),           # alpha4
  z                # z
)

# Test get_z_full_fast
z_full_direct <- c(zcr,zns[1],zcr,zcr,zns[2],zcr)

expect_equal(
  get_z_full_fast(th_y,mapping),
  z_full_direct
)

z_mat_direct <- diag(mod_spec$J+mod_spec$K)
z_mat_direct[1,2] <- zcr
z_mat_direct[1,3] <- zns[1]
z_mat_direct[1,4] <- zcr
z_mat_direct[2,3] <- zcr
z_mat_direct[2,4] <- zns[2]
z_mat_direct[3,4] <- zcr
z_mat_direct[2,1] <- z_mat_direct[1,2]
z_mat_direct[3,1] <- z_mat_direct[1,3]
z_mat_direct[4,1] <- z_mat_direct[1,4]
z_mat_direct[3,2] <- z_mat_direct[2,3]
z_mat_direct[4,2] <- z_mat_direct[2,4]
z_mat_direct[4,3] <- z_mat_direct[3,4]

expect_equal(
  get_z_full_fast(th_y,mapping,asMatrix=T),
  z_mat_direct
)

# Group 3: Model 2
# Another four variable, conditionally dependent model
mod_spec <- list(mean_spec=c('log_ord','pow_law_ord','pow_law','pow_law'))
mod_spec$noise_spec <- c('const','lin_pos_int','lin_pos_int','const')
mod_spec$J <- 2
mod_spec$M <- c(2,3)
mod_spec$K <- 2
mod_spec$cdep_spec <- 'dep' # conditionally dependent
mod_spec$cdep_groups <- c(1,1,2,2)

expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

zns      <- c(-.1,.2)
zcr      <- c(.05)
z        <- c(zns,zcr)
th_y <- c( 
  c(0.75),         # a2
  c(0.3,0.1,-.25), # a3
  c(0.8,1.2, .25), # a4
  c(-2.0,2.5),     # tau1
  c(-1.0,3.5,4.5), # tau2
  c(.2),           # alpha1
  c(.4,.03),       # alpha2
  c(.3,.02),       # alpha3
  c(.5),           # alpha4
  z                # z
)

# Test get_z_full
z_full_direct <- c(zns[1],zcr,zcr,zcr,zcr,zns[2])

expect_equal(
  get_z_full_fast(th_y,mapping),
  z_full_direct
)

z_mat_direct <- diag(mod_spec$J+mod_spec$K)
z_mat_direct[1,2] <- zns[1]
z_mat_direct[1,3] <- zcr
z_mat_direct[1,4] <- zcr
z_mat_direct[2,3] <- zcr
z_mat_direct[2,4] <- zcr
z_mat_direct[3,4] <- zns[2]
z_mat_direct[2,1] <- z_mat_direct[1,2]
z_mat_direct[3,1] <- z_mat_direct[1,3]
z_mat_direct[4,1] <- z_mat_direct[1,4]
z_mat_direct[3,2] <- z_mat_direct[2,3]
z_mat_direct[4,2] <- z_mat_direct[2,4]
z_mat_direct[4,3] <- z_mat_direct[3,4]

expect_equal(
  get_z_full_fast(th_y,mapping,asMatrix=T),
  z_mat_direct
)

# Group 3: Model 3
# Yet another four variable, conditionally dependent model
mod_spec <- list(mean_spec=c('log_ord','pow_law_ord','pow_law','pow_law'))
mod_spec$noise_spec <- c('const','lin_pos_int','lin_pos_int','const')
mod_spec$J <- 2
mod_spec$M <- c(2,3)
mod_spec$K <- 2
mod_spec$cdep_spec <- 'dep' # conditionally dependent
mod_spec$cdep_groups <- c(1,2,3,4)

expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

zns      <- c()
zcr      <- c(.1,.2,.3,.4,.5,.6)
z        <- c(zns,zcr)
th_y <- c( 
  c(0.75),         # a2
  c(0.3,0.1,-.25), # a3
  c(0.8,1.2, .25), # a4
  c(-2.0,2.5),     # tau1
  c(-1.0,3.5,4.5), # tau2
  c(.2),           # alpha1
  c(.4,.03),       # alpha2
  c(.3,.02),       # alpha3
  c(.5),           # alpha4
  z                # z
)

# Test get_z_full
z_full_direct <- z

expect_equal(
  get_z_full_fast(th_y,mapping),
  z_full_direct
)

z_mat_direct <- diag(mod_spec$J+mod_spec$K)
z_mat_direct[1,2] <- z[1]
z_mat_direct[1,3] <- z[2]
z_mat_direct[1,4] <- z[3]
z_mat_direct[2,3] <- z[4]
z_mat_direct[2,4] <- z[5]
z_mat_direct[3,4] <- z[6]
z_mat_direct[2,1] <- z_mat_direct[1,2]
z_mat_direct[3,1] <- z_mat_direct[1,3]
z_mat_direct[4,1] <- z_mat_direct[1,4]
z_mat_direct[3,2] <- z_mat_direct[2,3]
z_mat_direct[4,2] <- z_mat_direct[2,4]
z_mat_direct[4,3] <- z_mat_direct[3,4]

expect_equal(
  get_z_full_fast(th_y,mapping,asMatrix=T),
  z_mat_direct
)

# Group 4 functions relate to the negative log-likelihood
# ----------------
# Group 4
# calc_cond_gauss_int_inputs
# calc_neg_log_lik_vect_multivariate
# calc_neg_log_lik_multivariate
# hjk_nll_wrapper
# calc_neg_log_lik_vect_multivariate_chunk_outer
# calc_neg_log_lik_vect_multivariate_chunk_inner [tested indirectly]
# calc_neg_log_lik_scalar_multivariate [tested indirectly]
# calc_joint
# calc_x_posterior

# Define parameters that are used across models
# TODO: Use symbols that are consitent with the new notation
rho1 <- .65
rho2 <- .75
tau1 <- c(1,1.5)
tau2 <- c(2,3)
a1   <- 110
a2   <- 40
r1   <- .45
r2   <- .55
b1   <- -45
b2   <- 15
s1 <- .25
s2 <- .5
s3 <- 20
s4 <- 10
z <- c(.25,-.2,.3,.5,.05,.15)
kappa1 <- .01
kappa2 <- .02
kappa3 <- .01
kappa4 <- .02

x1 <- 1
x2 <- 2

# Group 4: Model 1
# A univarate ordinal model
mod_spec <- list(mean_spec='pow_law_ord')
mod_spec$J <- 1
mod_spec$K <- 0
mod_spec$M <- 2
mod_spec$noise_spec  <- 'const'
mod_spec$cdep_spec <- 'indep' # necessary for prep_for_neg_log_lik_multivariate

th_y <- c(rho1,tau1,s1)

expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

expect_error(
  tf_cat_vect <- get_multivariate_transform_categories(mapping$mod_spec),
  NA
)

expect_error(
  th_y_bar <- param_constr_to_unconstr(th_y,tf_cat_vect),
  NA
)

eta_vect_direct <- c(
                    -log(pnorm((tau1[1]-x1^rho1)/s1)),
                    -log(pnorm((tau1[1]-x2^rho1)/s1)),
                    -log(pnorm((tau1[2]-x1^rho1)/s1) - pnorm((tau1[1]-x1^rho1)/s1)),
                    -log(pnorm((tau1[2]-x2^rho1)/s1) - pnorm((tau1[1]-x2^rho1)/s1)),
                    -log(1-pnorm((tau1[2]-x1^rho1)/s1)),
                    -log(1-pnorm((tau1[2]-x2^rho1)/s1))
                   )

eta_vect <- c()
xcalc <- c(x1,x2,x1,x2,x1,x2)
mcalc <- c(0,0,1,1,2,2)
for(n in 1:length(xcalc)) {
  expect_error(
    cgiInputs <- calc_cond_gauss_int_inputs(th_y,xcalc[n],mcalc[n],mapping),
    NA
  )

  expect_equal(
    names(cgiInputs),
    c("mean_vect","cov_mat","lo","hi","y_giv")
  )

  if(xcalc[n] == x1) {
    expect_equal(
      cgiInputs$mean_vect,
      x1^rho1
    )
  } else {
    expect_equal(
      cgiInputs$mean_vect,
      x2^rho1
    )
  }

  expect_equal(
    cgiInputs$cov_mat,
    as.matrix(s1^2)
  )

  if(mcalc[n] == 0) {
    expect_equal(
      cgiInputs$lo,
      -Inf
    )

    expect_equal(
      cgiInputs$hi,
      tau1[1]
    )
  } else if(mcalc[n] == 1) {
    expect_equal(
      cgiInputs$lo,
      tau1[1]
    )

    expect_equal(
      cgiInputs$hi,
      tau1[2]
    )
  } else {
    expect_equal(
      cgiInputs$lo,
      tau1[2]
    )

    expect_equal(
      cgiInputs$hi,
      Inf
    )
  }

  expect_equal(
    cgiInputs$y_giv,
    c()
  )

  expect_error(
    calc_data <- prep_for_neg_log_lik_multivariate(xcalc[n],matrix(mcalc[n]),mod_spec),
    NA
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )


  expect_equal(
    calc_neg_log_lik_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  th_y_bar_scale <- rep(0.5, length(th_y_bar))
  param <- (th_y_bar - th_y_bar) / th_y_bar_scale
  expect_equal(
    hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
    eta_vect_direct[n],
  )
}

expect_error(
  calc_data <- prep_for_neg_log_lik_multivariate(xcalc,t(matrix(mcalc)),mod_spec),
  NA
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y,calc_data),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y,calc_data),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y_bar,calc_data,tf_cat_vect),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y,calc_data),
  sum(eta_vect_direct)
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
  sum(eta_vect_direct)
)

th_y_bar_scale <- rep(0.5, length(th_y_bar))
param <- (th_y_bar - th_y_bar) / th_y_bar_scale
expect_equal(
  hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
  sum(eta_vect_direct)
)

# Test calc_x_density here
# [Testing calc_x_density here is somewhat of an interlude, but the next tests
# use functions that rely on calc_x_density.]
# The x-vector to use for the calculations relevant to the posterior density:
dx <- .1
xpost <- seq(0,23,by=dx)

# Test when th_x is exponential
th_x_exp <- list(fit_type='exponential',fit=2)

f_exp_direct <- dexp(xpost,2)
expect_equal(
  calc_x_density(xpost,th_x_exp),
  f_exp_direct
)

# Test when th_x is an offset Weibull mixture
th_x_weib <- list(fit_type='offset_weib_mix',
                  fit=list(lambda=c(.28,.22,.50),
                           scale=c(2.1,3.6,18.4),
                           shape=c(.63,.78,8.56)),
                  weib_offset=0.002)

f_weib_direct <- rep(NA,length(xpost))
for(n in 1:length(xpost)) {
  f_weib_direct[n] <- dweibull(xpost[n]+.002,shape=.63,scale=2.1)*.28 +
    dweibull(xpost[n]+.002,shape=.78,scale=3.6)*.22 +
    dweibull(xpost[n]+.002,shape=8.56,scale=18.4)*.50
}

# Test when th_x is uniform
expect_equal(
  calc_x_density(xpost,th_x_weib),
  f_weib_direct
)

th_x_unif <- list(fit_type='uniform',fit=c(2,13.5))

f_unif_direct <- rep(0,length(xpost))
for(n in 1:length(xpost)) {
  if( (2 <= xpost[n]) && (xpost[n] <= 13.5) ) {
    f_unif_direct[n] <- 1/(13.5-2)
  }
}

expect_equal(
  calc_x_density(xpost,th_x_unif),
  f_unif_direct
)

# Test calc_joint, calc_x_posterior, and calc_kl_div
log_prior_vect_exp  <- log(f_exp_direct)  # does not depend on m or th_y
log_prior_vect_weib <- log(f_weib_direct) # does not depend on m or th_y
log_prior_vect_unif <- log(f_unif_direct) # does not depend on m or th_y
for(v in 0:mod_spec$M) {
  y <- v
  Y <- matrix(y,nrow=length(y),ncol=length(xpost))
  calc_data_v <- prep_for_neg_log_lik_multivariate(xpost,Y,mod_spec)
  log_lik_vect <- -calc_neg_log_lik_vect_multivariate(th_y,calc_data_v)

  # Exponential prior
  log_joint_vect_exp <- log_lik_vect + log_prior_vect_exp
  jointVect_exp <- exp(log_joint_vect_exp)
  jointVect_exp[!is.finite(jointVect_exp)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_exp,th_y,mod_spec),
    jointVect_exp
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_exp,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    names(x_post_obj),
    c("x","density","x_samp")
  )

  expect_equal(
    x_post_obj$x,
    xpost
  )

  expect_equal(
    x_post_obj$density,
    jointVect_exp / sum(jointVect_exp) / dx
  )

  expect_equal(
    x_post_obj$x_samp,
    c()
  )

  expect_error(
    kl_div <- calc_kl_div(x_post_obj,th_x_exp),
    NA
  )

  expect_equal(
    length(kl_div),
    1
  )

  expect_equal(
    is.na(kl_div),
    FALSE
  )

  # Weibull mixture prior
  log_joint_vect_weib <- log_lik_vect + log_prior_vect_weib
  jointVect_weib <- exp(log_joint_vect_weib)
  jointVect_weib[!is.finite(jointVect_weib)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_weib,th_y,mod_spec),
    jointVect_weib
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_weib,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    names(x_post_obj),
    c("x","density","x_samp")
  )

  expect_equal(
    x_post_obj$x,
    xpost
  )

  expect_equal(
    x_post_obj$density,
    jointVect_weib / sum(jointVect_weib) / dx
  )

  expect_equal(
    x_post_obj$x_samp,
    c()
  )

  expect_error(
    kl_div <- calc_kl_div(x_post_obj,th_x_weib),
    NA
  )

  expect_equal(
    length(kl_div),
    1
  )

  expect_equal(
    is.na(kl_div),
    FALSE
  )

  # Uniform prior
  log_joint_vect_unif <- log_lik_vect + log_prior_vect_unif
  jointVect_unif <- exp(log_joint_vect_unif)
  jointVect_unif[!is.finite(jointVect_unif)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_unif,th_y,mod_spec),
    jointVect_unif
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_unif,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    names(x_post_obj),
    c("x","density","x_samp")
  )

  expect_equal(
    x_post_obj$x,
    xpost
  )

  expect_equal(
    x_post_obj$density,
    jointVect_unif / sum(jointVect_unif) / dx
  )

  expect_equal(
    x_post_obj$x_samp,
    c()
  )

  expect_error(
    kl_div <- calc_kl_div(x_post_obj,th_x_unif),
    NA
  )

  expect_equal(
    length(kl_div),
    1
  )

  expect_equal(
    is.na(kl_div),
    FALSE
  )
}

# Group 4: Model 2
# A univariate continuous variable
mod_spec <- list(mean_spec='pow_law')
mod_spec$noise_spec  <- 'const'
mod_spec$J <- 0
mod_spec$K <- 1
mod_spec$cdep_spec <- 'indep' # necessary for prep_for_neg_log_lik_multivariate

th_y <- c(r1,a1,b1,s3)

expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

expect_error(
  tf_cat_vect <- get_multivariate_transform_categories(mapping$mod_spec),
  NA
)

expect_error(
  th_y_bar <- param_constr_to_unconstr(th_y,tf_cat_vect),
  NA
)

w1 <- 1.2
w2 <- 1.4
eta_vect_direct <- c(
                    -dnorm(w1,a1*x1^r1+b1,s3,log=T),
                    -dnorm(w2,a1*x2^r1+b1,s3,log=T)
                   )

xcalc <- c(x1,x2)
wcalc <- c(w1,w2)
for(n in 1:length(xcalc)) {
  expect_error(
    cgiInputs <- calc_cond_gauss_int_inputs(th_y,xcalc[n],wcalc[n],mapping),
    NA
  )

  expect_equal(
    names(cgiInputs),
    c("mean_vect","cov_mat","lo","hi","y_giv")
  )

  expect_equal(
    cgiInputs$mean_vect,
    a1*xcalc[n]^r1 + b1
  )

  expect_equal(
    cgiInputs$cov_mat,
    as.matrix(s3^2)
  )

  expect_equal(
    cgiInputs$y_giv,
    wcalc[n]
  )

  expect_error(
    calc_data <- prep_for_neg_log_lik_multivariate(xcalc[n],
                                                  matrix(wcalc[n]),
                                                  mod_spec),
    NA
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  th_y_bar_scale <- rep(0.5, length(th_y_bar))
  param <- (th_y_bar - th_y_bar) / th_y_bar_scale
  expect_equal(
    hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
    eta_vect_direct[n]
  )
}

expect_error(
  calc_data <- prep_for_neg_log_lik_multivariate(xcalc,t(matrix(wcalc)),mod_spec),
  NA
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y,calc_data),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y,calc_data,num_chunks=2),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y_bar,
                                                 calc_data,
                                                 tf_cat_vect,
                                                 num_chunks=2),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y,calc_data),
  sum(eta_vect_direct)
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
  sum(eta_vect_direct)
)

th_y_bar_scale <- rep(0.5, length(th_y_bar))
param <- (th_y_bar - th_y_bar) / th_y_bar_scale
expect_equal(
  hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
  sum(eta_vect_direct)
)

# Test calc_joint and calc_x_posterior
for(w in c(0,2.5)) {
  y <- w
  Y <- matrix(y,nrow=length(y),ncol=length(xpost))
  calc_data_w <- prep_for_neg_log_lik_multivariate(xpost,Y,mod_spec)
  log_lik_vect <- -calc_neg_log_lik_vect_multivariate(th_y,calc_data_w)
  
  # Exponential prior
  log_joint_vect_exp <- log_lik_vect + log_prior_vect_exp
  jointVect_exp <- exp(log_joint_vect_exp)
  jointVect_exp[!is.finite(jointVect_exp)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_exp,th_y,mod_spec),
    jointVect_exp
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_exp,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_exp / sum(jointVect_exp) / dx
  )

  # Weibull mixture prior
  log_joint_vect_weib <- log_lik_vect + log_prior_vect_weib
  jointVect_weib <- exp(log_joint_vect_weib)
  jointVect_weib[!is.finite(jointVect_weib)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_weib,th_y,mod_spec),
    jointVect_weib
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_weib,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_weib / sum(jointVect_weib) / dx
  )

  # Uniform prior
  log_joint_vect_unif <- log_lik_vect + log_prior_vect_unif
  jointVect_unif <- exp(log_joint_vect_unif)
  jointVect_unif[!is.finite(jointVect_unif)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_unif,th_y,mod_spec),
    jointVect_unif
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_unif,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_unif / sum(jointVect_unif) / dx
  )
}

# Group 4: Model 3
# One ordinal and one continuous variable
mod_spec <- list(mean_spec=c('pow_law_ord','pow_law'))
mod_spec$noise_spec  <- c('const','const')
mod_spec$J <- 1
mod_spec$K <- 1
mod_spec$M <- 2
mod_spec$cdep_spec <- 'dep'
mod_spec$cdep_groups <- c(1,2)

th_y <- c(rho1,r1,a1,b1,tau1,s1,s3,z[2])

w1 <- 1.2
w2 <- 1.4

mu_bar1 <- x1^rho1 + z[2]*s1/s3*(w1-a1*x1^r1-b1)
s1_bar <- s1*sqrt(1-z[2]^2)
mu_bar2 <- x2^rho1 + z[2]*s1/s3*(w2-a1*x2^r1-b1)

expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

expect_error(
  tf_cat_vect <- get_multivariate_transform_categories(mapping$mod_spec),
  NA
)

expect_error(
  th_y_bar <- param_constr_to_unconstr(th_y,tf_cat_vect),
  NA
)

eta_vect_direct <- c(
                    -dnorm(w1,a1*x1^r1+b1,s3,log=T) -
                      log(pnorm((tau1[1]-mu_bar1)/s1_bar)),
                    -dnorm(w1,a1*x1^r1+b1,s3,log=T) -
                      log(pnorm((tau1[2]-mu_bar1)/s1_bar) -
                             pnorm((tau1[1]-mu_bar1)/s1_bar)),
                    -dnorm(w1,a1*x1^r1+b1,s3,log=T) -
                      log(1 - pnorm((tau1[2]-mu_bar1)/s1_bar)),
                    -dnorm(w2,a1*x2^r1+b1,s3,log=T) -
                      log(pnorm((tau1[1]-mu_bar2)/s1_bar)),
                    -dnorm(w2,a1*x2^r1+b1,s3,log=T) -
                      log(pnorm((tau1[2]-mu_bar2)/s1_bar) -
                             pnorm((tau1[1]-mu_bar2)/s1_bar)),
                    -dnorm(w2,a1*x2^r1+b1,s3,log=T) -
                      log(1 - pnorm((tau1[2]-mu_bar2)/s1_bar))
                   )

xcalc <- c(x1,x1,x1,x2,x2,x2)
Ycalc <- t(matrix(c(0,1,2,0,1,2,w1,w1,w1,w2,w2,w2),nrow=6))

for(n in 1:length(xcalc)) {
  expect_error(
    cgiInputs <- calc_cond_gauss_int_inputs(th_y,xcalc[n],Ycalc[,n],mapping),
    NA
  )

  expect_equal(
    names(cgiInputs),
    c("mean_vect","cov_mat","lo","hi","y_giv")
  )

  expect_equal(
    cgiInputs$mean_vect,
    c(xcalc[n]^rho1,a1*xcalc[n]^r1 + b1)
  )

  expect_equal(
    cgiInputs$cov_mat,
    matrix(c(s1^2,s1*s3*z[2],s1*s3*z[2],s3^2),nrow=2)
  )

  expect_equal(
    cgiInputs$y_giv,
    Ycalc[2,n]
  )

  expect_error(
    calc_data <- prep_for_neg_log_lik_multivariate(xcalc[n],
                                                  matrix(Ycalc[,n]),
                                                  mod_spec),
    NA
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  th_y_bar_scale <- rep(0.5, length(th_y_bar))
  param <- (th_y_bar - th_y_bar) / th_y_bar_scale
  expect_equal(
    hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
    eta_vect_direct[n]
  )
}

expect_error(
  calc_data <- prep_for_neg_log_lik_multivariate(xcalc,Ycalc,mod_spec),
  NA
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y,calc_data),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y,calc_data),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y_bar,calc_data,tf_cat_vect),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y,calc_data),
  sum(eta_vect_direct)
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
  sum(eta_vect_direct)
)

th_y_bar_scale <- rep(0.5, length(th_y_bar))
param <- (th_y_bar - th_y_bar) / th_y_bar_scale
expect_equal(
  hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
  sum(eta_vect_direct)
)
# Test calc_joint and calc_x_posterior
for(n in 1:ncol(Ycalc)) {
  y <- as.matrix(Ycalc[,n])
  Y <- t(apply(y,1,rep,length(xpost)))
  calc_data_y <- prep_for_neg_log_lik_multivariate(xpost,Y,mod_spec)
  log_lik_vect <- -calc_neg_log_lik_vect_multivariate(th_y,calc_data_y)
  
  # Exponential prior
  log_joint_vect_exp <- log_lik_vect + log_prior_vect_exp
  jointVect_exp <- exp(log_joint_vect_exp)
  jointVect_exp[!is.finite(jointVect_exp)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_exp,th_y,mod_spec),
    jointVect_exp
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_exp,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_exp / sum(jointVect_exp) / dx
  )

  # Weibull mixture prior
  log_joint_vect_weib <- log_lik_vect + log_prior_vect_weib
  jointVect_weib <- exp(log_joint_vect_weib)
  jointVect_weib[!is.finite(jointVect_weib)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_weib,th_y,mod_spec),
    jointVect_weib
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_weib,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_weib / sum(jointVect_weib) / dx
  )

  # Uniform prior
  log_joint_vect_unif <- log_lik_vect + log_prior_vect_unif
  jointVect_unif <- exp(log_joint_vect_unif)
  jointVect_unif[!is.finite(jointVect_unif)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_unif,th_y,mod_spec),
    jointVect_unif
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_unif,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_unif / sum(jointVect_unif) / dx
  )
}

# Group 4: Model 4
# Two ordinal and two continuous variable
mod_spec <- list(mean_spec=c('pow_law_ord','pow_law_ord','pow_law','pow_law'))
mod_spec$noise_spec  <- rep('const',4)
mod_spec$J <- 2
mod_spec$K <- 2
mod_spec$M <- c(2,2)
mod_spec$cdep_spec <- 'dep'
mod_spec$cdep_groups <- c(1,2,3,4)

th_y <- c(rho1,rho2,r1,a1,b1,r2,a2,b2,tau1,tau2,s1,s2,s3,s4,z)

Ycalc <- matrix(c(0,1,100,50,1,2,160,92),nrow=4)
# Directly Calculate likelihood for both observations
g1 <- x1^c(rho1,rho2)
g2 <- x2^c(rho1,rho2)
h1 <- c(a1,a2)*x1^c(r1,r2) + c(b1,b2)
h2 <- c(a1,a2)*x2^c(r1,r2) + c(b1,b2)

expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

expect_error(
  tf_cat_vect <- get_multivariate_transform_categories(mapping$mod_spec),
  NA
)

expect_error(
  th_y_bar <- param_constr_to_unconstr(th_y,tf_cat_vect),
  NA
)
noise_vect <- c(s1,s2,s3,s4)
z_mat <- get_z_full_fast(th_y,mapping,T)
cov_mat <- as.matrix(noise_vect) %*% base::t(as.matrix(noise_vect))
cov_mat <- cov_mat * z_mat

S1 <- cov_mat
S2 <- cov_mat

condMean1 <- g1 + S1[1:2,3:4] %*% solve(S1[3:4,3:4]) %*% as.matrix(c(100,50) - h1)
condCov1 <- S1[1:2,1:2] - S1[1:2,3:4] %*% solve(S1[3:4,3:4]) %*% S1[3:4,1:2]
logLik1 <- mvtnorm::dmvnorm(c(100,50),h1,S1[3:4,3:4],log=T)
condIntegral1 <- mvtnorm::pmvnorm(lower=c(-Inf,tau2[1]),
                                  upper=c(tau1[1],tau2[2]),
                                  mean=as.vector(condMean1),
                                  sigma=condCov1)
logLik1 <- logLik1 + log(as.numeric(condIntegral1))

condMean2 <- g2 + S2[1:2,3:4] %*% solve(S2[3:4,3:4]) %*% as.matrix(c(160,92) - h2)
condCov2 <- S2[1:2,1:2] - S2[1:2,3:4] %*% solve(S2[3:4,3:4]) %*% S2[3:4,1:2]
logLik2 <- mvtnorm::dmvnorm(c(160,92),h2,S2[3:4,3:4],log=T)
condIntegral2 <- mvtnorm::pmvnorm(lower=c(tau1[1],tau2[2]),
                                  upper=c(tau1[2],Inf),
                                  mean=as.vector(condMean2),
                                  sigma=condCov2)
logLik2 <- logLik2 + log(as.numeric(condIntegral2))

eta_vect_direct <- c(-logLik1,-logLik2)

xcalc <- c(x1,x2)
for(n in 1:length(xcalc)) {
  expect_error(
    cgiInputs <- calc_cond_gauss_int_inputs(th_y,xcalc[n],Ycalc[,n],mapping),
    NA
  )

  expect_equal(
    names(cgiInputs),
    c("mean_vect","cov_mat","lo","hi","y_giv")
  )

  expect_equal(
    cgiInputs$mean_vect,
    c(xcalc[n]^rho1,xcalc[n]^rho2,a1*xcalc[n]^r1 + b1,a2*xcalc[n]^r2+b2)
  )

  expect_equal(
    cgiInputs$cov_mat,
    cov_mat
  )

  expect_equal(
    cgiInputs$y_giv,
    Ycalc[3:4,n]
  )

  expect_error(
    calc_data <- prep_for_neg_log_lik_multivariate(xcalc[n],
                                                  matrix(Ycalc[,n]),
                                                  mod_spec),
    NA
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  th_y_bar_scale <- rep(0.5, length(th_y_bar))
  param <- (th_y_bar - th_y_bar) / th_y_bar_scale
  expect_equal(
    hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
    eta_vect_direct[n]
  )
}

expect_error(
  calc_data <- prep_for_neg_log_lik_multivariate(xcalc,Ycalc,mod_spec),
  NA
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y,calc_data),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y,calc_data,num_chunks=2),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y_bar,
                                                 calc_data,
                                                 tf_cat_vect,
                                                 num_chunks=2),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y,calc_data),
  sum(eta_vect_direct)
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
  sum(eta_vect_direct)
)

th_y_bar_scale <- rep(0.5, length(th_y_bar))
param <- (th_y_bar - th_y_bar) / th_y_bar_scale
expect_equal(
  hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
  sum(eta_vect_direct)
)

# Test calc_joint and calc_x_posterior
for(n in 1:ncol(Ycalc)) {
  y <- as.matrix(Ycalc[,n])
  Y <- t(apply(y,1,rep,length(xpost)))
  calc_data_y <- prep_for_neg_log_lik_multivariate(xpost,Y,mod_spec)
  log_lik_vect <- -calc_neg_log_lik_vect_multivariate(th_y,calc_data_y)
  
  # Exponential prior
  log_joint_vect_exp <- log_lik_vect + log_prior_vect_exp
  jointVect_exp <- exp(log_joint_vect_exp)
  jointVect_exp[!is.finite(jointVect_exp)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_exp,th_y,mod_spec),
    jointVect_exp
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_exp,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_exp / sum(jointVect_exp) / dx
  )

  # Weibull mixture prior
  log_joint_vect_weib <- log_lik_vect + log_prior_vect_weib
  jointVect_weib <- exp(log_joint_vect_weib)
  jointVect_weib[!is.finite(jointVect_weib)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_weib,th_y,mod_spec),
    jointVect_weib
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_weib,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_weib / sum(jointVect_weib) / dx
  )

  # Uniform prior
  log_joint_vect_unif <- log_lik_vect + log_prior_vect_unif
  jointVect_unif <- exp(log_joint_vect_unif)
  jointVect_unif[!is.finite(jointVect_unif)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_unif,th_y,mod_spec),
    jointVect_unif
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_unif,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_unif / sum(jointVect_unif) / dx
  )
}

# Test a model with two ordinal and two continuous variable (with log_ord,
# lin_pos_int, and some complexity in cdep_groups)
mod_spec <- list(mean_spec=c('pow_law_ord','log_ord','pow_law','pow_law'))
mod_spec$noise_spec  <- c('const','lin_pos_int','lin_pos_int','const')
mod_spec$J <- 2
mod_spec$K <- 2
mod_spec$M <- c(2,2)
mod_spec$cdep_spec <- 'dep'
mod_spec$cdep_groups <- c(1,1,NA,2)

z <- c(.25,-.2)

th_y <- c(rho1,r1,a1,b1,r2,a2,b2,tau1,tau2,s1,s2,kappa2,s3,kappa3,s4,z)

Ycalc <- matrix(c(0,1,100,50,1,2,160,92),nrow=4)
# Directly Calculate likelihood for both observations
g1 <- c(x1^rho1,log(x1))
g2 <- c(x2^rho1,log(x2))
h1 <- c(a1,a2)*x1^c(r1,r2) + c(b1,b2)
h2 <- c(a1,a2)*x2^c(r1,r2) + c(b1,b2)

expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

# Test get_z_full_fast here, too
z_full_direct <- c(.25,0,-.2,0,-.2,0)
expect_equal(
  get_z_full_fast(th_y,mapping),
  z_full_direct
)

z_mat_direct <- diag(4)
z_mat_direct[lower.tri(z_mat_direct)] <- z_full_direct
z_mat_direct <- t(z_mat_direct)
z_mat_direct[lower.tri(z_mat_direct)] <- z_full_direct
expect_error(
  z_mat <- get_z_full_fast(th_y,mapping,asMatrix=T),
  NA
)

expect_equal(
  z_mat,
  z_mat_direct
)

expect_error(
  tf_cat_vect <- get_multivariate_transform_categories(mapping$mod_spec),
  NA
)

expect_error(
  th_y_bar <- param_constr_to_unconstr(th_y,tf_cat_vect),
  NA
)

noise_vect1 <- c(s1,s2*(1+kappa2*x1),s3*(1+kappa3*x1),s4)
cov_mat1 <- as.matrix(noise_vect1) %*% base::t(as.matrix(noise_vect1))
cov_mat1 <- cov_mat1 * z_mat

noise_vect2 <- c(s1,s2*(1+kappa2*x2),s3*(1+kappa3*x2),s4)
cov_mat2 <- as.matrix(noise_vect2) %*% base::t(as.matrix(noise_vect2))
cov_mat2 <- cov_mat2 * z_mat

S1 <- cov_mat1
S2 <- cov_mat2

condMean1 <-
  g1 + S1[1:2,3:4] %*% solve(S1[3:4,3:4]) %*% as.matrix(c(100,50) - h1)
condCov1 <- S1[1:2,1:2] - S1[1:2,3:4] %*% solve(S1[3:4,3:4]) %*% S1[3:4,1:2]
logLik1 <- mvtnorm::dmvnorm(c(100,50),h1,S1[3:4,3:4],log=T)
condIntegral1 <- mvtnorm::pmvnorm(lower=c(-Inf,tau2[1]),
                                  upper=c(tau1[1],tau2[2]),
                                  mean=as.vector(condMean1),
                                  sigma=condCov1)
logLik1 <- logLik1 + log(as.numeric(condIntegral1))

condMean2 <-
  g2 + S2[1:2,3:4] %*% solve(S2[3:4,3:4]) %*% as.matrix(c(160,92) - h2)
condCov2 <- S2[1:2,1:2] - S2[1:2,3:4] %*% solve(S2[3:4,3:4]) %*% S2[3:4,1:2]
logLik2 <- mvtnorm::dmvnorm(c(160,92),h2,S2[3:4,3:4],log=T)
condIntegral2 <- mvtnorm::pmvnorm(lower=c(tau1[1],tau2[2]),
                                  upper=c(tau1[2],Inf),
                                  mean=as.vector(condMean2),
                                  sigma=condCov2)
logLik2 <- logLik2 + log(as.numeric(condIntegral2))

eta_vect_direct <- c(-logLik1,-logLik2)

xcalc <- c(x1,x2)
for(n in 1:length(xcalc)) {
  expect_error(
    cgiInputs <- calc_cond_gauss_int_inputs(th_y,xcalc[n],Ycalc[,n],mapping),
    NA
  )

  expect_equal(
    names(cgiInputs),
    c("mean_vect","cov_mat","lo","hi","y_giv")
  )

  if(n == 1) {
    expect_equal(
      cgiInputs$mean_vect,
      c(g1,h1)
    )

    expect_equal(
      cgiInputs$cov_mat,
      S1
    )
  } else {
    expect_equal(
      cgiInputs$mean_vect,
      c(g2,h2)
    )

    expect_equal(
      cgiInputs$cov_mat,
      S2
    )
  }

  expect_equal(
    cgiInputs$y_giv,
    Ycalc[3:4,n]
  )

  expect_error(
    calc_data <- prep_for_neg_log_lik_multivariate(xcalc[n],
                                                  matrix(Ycalc[,n]),
                                                  mod_spec),
    NA
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  th_y_bar_scale <- rep(0.5, length(th_y_bar))
  param <- (th_y_bar - th_y_bar) / th_y_bar_scale
  expect_equal(
    hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
    eta_vect_direct[n]
  )
}

expect_error(
  calc_data <- prep_for_neg_log_lik_multivariate(xcalc,Ycalc,mod_spec),
  NA
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y,calc_data),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y,calc_data,num_chunks=2),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y_bar,
                                                 calc_data,
                                                 tf_cat_vect,
                                                 num_chunks=2),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y,calc_data),
  sum(eta_vect_direct)
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
  sum(eta_vect_direct)
)

th_y_bar_scale <- rep(0.5, length(th_y_bar))
param <- (th_y_bar - th_y_bar) / th_y_bar_scale
expect_equal(
  hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
  sum(eta_vect_direct)
)

# Test calc_joint and calc_x_posterior
for(n in 1:ncol(Ycalc)) {
  # If the log_ord special case applies (y[2] > 0), subset to ensure that xpost
  # is not zero
  if(y[2] > 0) {
    ind <- which(xpost != 0)
  } else {
    ind <- 1:length(xpost)
  }

  y <- as.matrix(Ycalc[,n])
  Y <- t(apply(y,1,rep,length(xpost[ind])))


  calc_data_y <- prep_for_neg_log_lik_multivariate(xpost[ind],Y,mod_spec)
  log_lik_vect <- -calc_neg_log_lik_vect_multivariate(th_y,calc_data_y)
  
  # Exponential prior
  log_joint_vect_exp <- log_lik_vect + log_prior_vect_exp[ind]
  jointVect_exp <- exp(log_joint_vect_exp)
  jointVect_exp[!is.finite(jointVect_exp)] <- 0
  expect_equal(
    calc_joint(xpost[ind],y,th_x_exp,th_y,mod_spec),
    jointVect_exp
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_exp,th_y,mod_spec,xpost[ind]),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_exp / sum(jointVect_exp) / dx
  )

  # Weibull mixture prior
  log_joint_vect_weib <- log_lik_vect + log_prior_vect_weib[ind]
  jointVect_weib <- exp(log_joint_vect_weib)
  jointVect_weib[!is.finite(jointVect_weib)] <- 0
  expect_equal(
    calc_joint(xpost[ind],y,th_x_weib,th_y,mod_spec),
    jointVect_weib
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_weib,th_y,mod_spec,xpost[ind]),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_weib / sum(jointVect_weib) / dx
  )

  # Uniform prior
  log_joint_vect_unif <- log_lik_vect + log_prior_vect_unif[ind]
  jointVect_unif <- exp(log_joint_vect_unif)
  jointVect_unif[!is.finite(jointVect_unif)] <- 0
  expect_equal(
    calc_joint(xpost[ind],y,th_x_unif,th_y,mod_spec),
    jointVect_unif
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_unif,th_y,mod_spec,xpost[ind]),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_unif / sum(jointVect_unif) / dx
  )
}

# Group 4: Model 5
# Two ordinal and two continuous variable (with lin_ord, lin_pos_int, and some
# complexity in cdep_groups)
mod_spec <- list(mean_spec=c('lin_ord','pow_law_ord','pow_law','pow_law'))
mod_spec$noise_spec  <- c('lin_pos_int','const','const','lin_pos_int')
mod_spec$J <- 2
mod_spec$K <- 2
mod_spec$M <- c(2,2)
mod_spec$cdep_spec <- 'dep'
mod_spec$cdep_groups <- c(1,NA,NA,2)

z <- c(.25)

th_y <- c(rho2,r1,a1,b1,r2,a2,b2,tau1,tau2,s1,kappa1,s2,s3,s4,kappa4,z)

Ycalc <- matrix(c(0,1,100,50,1,2,160,92),nrow=4)
# Directly Calculate likelihood for both observations
g1 <- c(x1,x1^rho2)
g2 <- c(x2,x2^rho2)
h1 <- c(a1,a2)*x1^c(r1,r2) + c(b1,b2)
h2 <- c(a1,a2)*x2^c(r1,r2) + c(b1,b2)

expect_error(
  mapping <- get_var_index_multivariate_mapping(mod_spec),
  NA
)

# Test get_z_full_fast here, too
z_full_direct <- c(0,0,.25,0,0,0)
expect_equal(
  get_z_full_fast(th_y,mapping),
  z_full_direct
)

z_mat_direct <- diag(4)
z_mat_direct[lower.tri(z_mat_direct)] <- z_full_direct
z_mat_direct <- t(z_mat_direct)
z_mat_direct[lower.tri(z_mat_direct)] <- z_full_direct
expect_error(
  z_mat <- get_z_full_fast(th_y,mapping,asMatrix=T),
  NA
)

expect_equal(
  z_mat,
  z_mat_direct
)

expect_error(
  tf_cat_vect <- get_multivariate_transform_categories(mapping$mod_spec),
  NA
)

expect_error(
  th_y_bar <- param_constr_to_unconstr(th_y,tf_cat_vect),
  NA
)

noise_vect1 <- c(s1*(1+kappa1*x1),s2,s3,s4*(1+kappa4*x1))
cov_mat1 <- as.matrix(noise_vect1) %*% base::t(as.matrix(noise_vect1))
cov_mat1 <- cov_mat1 * z_mat

noise_vect2 <- c(s1*(1+kappa1*x2),s2,s3,s4*(1+kappa4*x2))
cov_mat2 <- as.matrix(noise_vect2) %*% base::t(as.matrix(noise_vect2))
cov_mat2 <- cov_mat2 * z_mat

S1 <- cov_mat1
S2 <- cov_mat2

condMean1 <-
  g1 + S1[1:2,3:4] %*% solve(S1[3:4,3:4]) %*% as.matrix(c(100,50) - h1)
condCov1 <- S1[1:2,1:2] - S1[1:2,3:4] %*% solve(S1[3:4,3:4]) %*% S1[3:4,1:2]
logLik1 <- mvtnorm::dmvnorm(c(100,50),h1,S1[3:4,3:4],log=T)
condIntegral1 <- mvtnorm::pmvnorm(lower=c(-Inf,tau2[1]),
                                  upper=c(tau1[1],tau2[2]),
                                  mean=as.vector(condMean1),
                                  sigma=condCov1)
logLik1 <- logLik1 + log(as.numeric(condIntegral1))

condMean2 <-
  g2 + S2[1:2,3:4] %*% solve(S2[3:4,3:4]) %*% as.matrix(c(160,92) - h2)
condCov2 <- S2[1:2,1:2] - S2[1:2,3:4] %*% solve(S2[3:4,3:4]) %*% S2[3:4,1:2]
logLik2 <- mvtnorm::dmvnorm(c(160,92),h2,S2[3:4,3:4],log=T)
condIntegral2 <- mvtnorm::pmvnorm(lower=c(tau1[1],tau2[2]),
                                  upper=c(tau1[2],Inf),
                                  mean=as.vector(condMean2),
                                  sigma=condCov2)
logLik2 <- logLik2 + log(as.numeric(condIntegral2))

eta_vect_direct <- c(-logLik1,-logLik2)

xcalc <- c(x1,x2)
for(n in 1:length(xcalc)) {
  expect_error(
    cgiInputs <- calc_cond_gauss_int_inputs(th_y,xcalc[n],Ycalc[,n],mapping),
    NA
  )

  expect_equal(
    names(cgiInputs),
    c("mean_vect","cov_mat","lo","hi","y_giv")
  )

  if(n == 1) {
    expect_equal(
      cgiInputs$mean_vect,
      c(g1,h1)
    )

    expect_equal(
      cgiInputs$cov_mat,
      S1
    )
  } else {
    expect_equal(
      cgiInputs$mean_vect,
      c(g2,h2)
    )

    expect_equal(
      cgiInputs$cov_mat,
      S2
    )
  }

  expect_equal(
    cgiInputs$y_giv,
    Ycalc[3:4,n]
  )

  expect_error(
    calc_data <- prep_for_neg_log_lik_multivariate(xcalc[n],
                                                  matrix(Ycalc[,n]),
                                                  mod_spec),
    NA
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y,calc_data),
    eta_vect_direct[n]
  )

  expect_equal(
    calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
    eta_vect_direct[n]
  )

  th_y_bar_scale <- rep(0.5, length(th_y_bar))
  param <- (th_y_bar - th_y_bar) / th_y_bar_scale
  expect_equal(
    hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
    eta_vect_direct[n]
  )
}

expect_error(
  calc_data <- prep_for_neg_log_lik_multivariate(xcalc,Ycalc,mod_spec),
  NA
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y,calc_data),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate(th_y_bar,calc_data,tf_cat_vect),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y,calc_data,num_chunks=2),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_vect_multivariate_chunk_outer(th_y_bar,
                                                 calc_data,
                                                 tf_cat_vect,
                                                 num_chunks=2),
  eta_vect_direct
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y,calc_data),
  sum(eta_vect_direct)
)

expect_equal(
  calc_neg_log_lik_multivariate(th_y_bar,calc_data,tf_cat_vect),
  sum(eta_vect_direct)
)

th_y_bar_scale <- rep(0.5, length(th_y_bar))
param <- (th_y_bar - th_y_bar) / th_y_bar_scale
expect_equal(
  hjk_nll_wrapper(param, th_y_bar, th_y_bar_scale, calc_data, tf_cat_vect),
  sum(eta_vect_direct)
)

# Test calc_joint and calc_x_posterior
for(n in 1:ncol(Ycalc)) {
  y <- as.matrix(Ycalc[,n])
  Y <- t(apply(y,1,rep,length(xpost)))
  calc_data_y <- prep_for_neg_log_lik_multivariate(xpost,Y,mod_spec)
  log_lik_vect <- -calc_neg_log_lik_vect_multivariate(th_y,calc_data_y)
  
  # Exponential prior
  log_joint_vect_exp <- log_lik_vect + log_prior_vect_exp
  jointVect_exp <- exp(log_joint_vect_exp)
  jointVect_exp[!is.finite(jointVect_exp)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_exp,th_y,mod_spec),
    jointVect_exp
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_exp,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_exp / sum(jointVect_exp) / dx
  )

  # Weibull mixture prior
  log_joint_vect_weib <- log_lik_vect + log_prior_vect_weib
  jointVect_weib <- exp(log_joint_vect_weib)
  jointVect_weib[!is.finite(jointVect_weib)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_weib,th_y,mod_spec),
    jointVect_weib
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_weib,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_weib / sum(jointVect_weib) / dx
  )

  # Uniform prior
  log_joint_vect_unif <- log_lik_vect + log_prior_vect_unif
  jointVect_unif <- exp(log_joint_vect_unif)
  jointVect_unif[!is.finite(jointVect_unif)] <- 0
  expect_equal(
    calc_joint(xpost,y,th_x_unif,th_y,mod_spec),
    jointVect_unif
  )

  expect_error(
    x_post_obj <- calc_x_posterior(y,th_x_unif,th_y,mod_spec,xpost),
    NA
  )

  expect_equal(
    x_post_obj$density,
    jointVect_unif / sum(jointVect_unif) / dx
  )
}

# Group 5
# sim_multivariate
# fit_multivariate
# sample_x_posterior
# calc_x_posterior [additional tests]
# analyze_x_posterior

# Group 5: Model 1 [the only model tested for this group]
# Four variable model
mod_spec <- list(mean_spec=c('pow_law_ord','log_ord','pow_law','pow_law'))
mod_spec$noise_spec  <- c('const','lin_pos_int','lin_pos_int','const')
mod_spec$J <- 2
mod_spec$K <- 2
mod_spec$M <- c(2,2)
mod_spec$cdep_spec <- 'dep'
mod_spec$cdep_groups <- c(1,1,NA,2)

th_y_sim <- c(
              .65,            # mean parameters for j = 1
              c(),            # mean parameters for j = 2 [no parameters for
                              #                            log_ord]
              c(.45,110,-45), # mean parameters for k = 1
              c(.55, 40, 15), # mean parameters for k = 2
              c( 1,1.5),      # tau for j = 1
              c(-1,1  ),      # tau for j = 2
              .25,            # noise parameters for j = 1
              c(.5,.02),      # noise parameters for j = 2
              c(5,.04),       # noise parameters for k = 1
              10,             # noise parameters for k = 2
              c(.6,.25)       # correlation parameters (z)
             )

N <- 100 # number of simulated observations

th_x <- list(fit_type='uniform',fit=c(0,5))

# Check simulation for when N and th_x are input
expect_error(
  sim <- sim_multivariate(th_y_sim,mod_spec,N,th_x),
  NA
)

expect_equal(
  names(sim),
  c('x','Y','Ystar')
)

expect_equal(
  length(sim$x),
  N
)

expect_equal(
  dim(sim$Ystar),
  c(mod_spec$J+mod_spec$K,N)
)

expect_equal(
  dim(sim$Y),
  c(mod_spec$J+mod_spec$K,N)
)

# Check that univariate_fits is extracted properly
set.seed(2000)
Nz <- get_z_length(mod_spec)
th_y0 <- th_y_sim[1:(length(th_y_sim) - Nz)]
mod_spec_cindep <- mod_spec
mod_spec_cindep$cdep_spec <- "indep"
mod_spec_cindep$cdep_groups <- NULL

th_y0 <- th_y0 + rnorm(length(th_y0)) * .01
tf_cat_vect_cindep <- get_multivariate_transform_categories(mod_spec_cindep)
th_y_bar0 <- param_constr_to_unconstr(th_y0, tf_cat_vect_cindep)
cindep_model <- list(th_y = th_y0,
                     mod_spec = mod_spec_cindep,
                     th_y_bar = th_y_bar0,
                     th_y_bar_se = rep(0.1, length(th_y_bar0)))

expect_error(
  fit <- fit_multivariate(sim$x,
                          sim$Y,
                          mod_spec,
                          cindep_model,
                          hjk_control=list(maxfeval=200)),
  NA
)

expect_equal(
  any(is.na(fit$th_y)),
  FALSE
)

expect_equal(
  length(fit$th_y),
  length(th_y_sim)
)
# Check functioning of the save file
save_file <- file.path(tempdir(),"hjk_temp.rds")
if (file.exists(save_file)) {
  success <- file.remove(save_file)
}

expect_error(
  fit <- fit_multivariate(sim$x,
                          sim$Y,
                          mod_spec,
                          cindep_model,
                          save_file=save_file,
                          hjk_control=list(maxfeval=200)),
  NA
)

expect_equal(
  any(is.na(fit$th_y)),
  FALSE
)

expect_equal(
  length(fit$th_y),
  length(th_y_sim)
)

expect_equal(
  file.exists(save_file),
  TRUE
)

hjk_prog <- readRDS(save_file)

expect_equal(
  names(hjk_prog),
  c("n",
    "eta_vect",
    "eta_best",
    "param_best",
    "eta0",
    "th_y_bar0",
    "th_y_bar_scale")
)

expect_error(
  fit <- fit_multivariate(sim$x,
                          sim$Y,
                          mod_spec,
                          cindep_model,
                          save_file=save_file,
                          hjk_control=list(maxfeval=200)),
  "save_file already exists"
)

success <- file.remove(save_file)

# Check simulation when x is input. Use an x-vector with 0 to check the
# functioning for j = 2 for which the mean_spec is 'log_ord'
x <- c(rep(0,10),sim$x)
N <- length(x)
expect_error(
  sim <- sim_multivariate(th_y_sim,mod_spec,x=x),
  NA
)

expect_equal(
  names(sim),
  c('x','Y','Ystar')
)

expect_equal(
  length(sim$x),
  N
)

expect_equal(
  dim(sim$Ystar),
  c(mod_spec$J+mod_spec$K,N)
)

expect_equal(
  dim(sim$Y),
  c(mod_spec$J+mod_spec$K,N)
)

expect_equal(
  sim$Ystar[2,1:10],
  rep(-Inf,10)
)

expect_equal(
  sim$Y[2,1:10],
  rep(0,10)
)

# Check sample_x_posterior
y <- c(1,0,65,55)

expect_error(
  x_samp <- sample_x_posterior(y,th_x,th_y_sim,mod_spec,200),
  NA
)

expect_equal(
  length(x_samp),
  200
)

expect_equal(
  any(is.na(x_samp)),
  FALSE
)

expect_error(
  x_samp <- sample_x_posterior(y,th_x,th_y_sim,mod_spec,200,
                               thinning=4,prop_rescale=0.15),
  NA
)

expect_equal(
  length(x_samp),
  200/4
)

expect_equal(
  any(is.na(x_samp)),
  FALSE
)

# Check calc_x_posterior [additional tests to check different ways of calling
#                         the function]
expect_error(
  x_post_obj <- calc_x_posterior(y,th_x,th_y_sim,mod_spec),
  NA
)

expect_equal(
  names(x_post_obj),
  c("x","density","x_samp")
)

expect_equal(
  length(x_post_obj$x),
  1000
)

expect_equal(
  length(x_post_obj$density),
  1000
)

expect_equal(
  length(x_post_obj$x_samp),
  1000
)

expect_error(
  x_post_obj <- calc_x_posterior(y,th_x,th_y_sim,mod_spec,normalize=F),
  NA
)

expect_equal(
  names(x_post_obj),
  c("x","density","x_samp")
)

expect_equal(
  length(x_post_obj$x),
  1000
)

expect_equal(
  length(x_post_obj$density),
  1000
)

expect_equal(
  length(x_post_obj$x_samp),
  1000
)


# Check analyze_x_posterior
dx <- 0.005
xv <- seq(0,max(x_post_obj$x),by=dx)
x_post_obj <- calc_x_posterior(y,th_x,th_y_sim,mod_spec,xcalc=xv,normalize=T)
fv <- x_post_obj$density
expect_equal(
  sum(fv) * dx,
  1,
  tolerance=1e-8
)

expect_error(
  post_analysis <- analyze_x_posterior(xv,fv),
  NA
)

expect_equal(
  names(post_analysis),
  c("x","density","dx",
    "xlolo","xlo","xmed","xhi","xhihi","xmean",
    "flolo","flo","fmed","fhi","fhihi","fmean")
)

expect_equal(
  post_analysis$x,
  xv
)

expect_equal(
  post_analysis$density,
  fv
)

expect_equal(
  post_analysis$dx,
  dx
)

expect_equal(
  post_analysis$dx,
  dx
)

expect_error(
  post_analysis <- analyze_x_posterior(xv,fv,xknown=0.9),
  NA
)

expect_equal(
  names(post_analysis),
  c("x","density","dx",
    "xlolo","xlo","xmed","xhi","xhihi","xmean",
    "flolo","flo","fmed","fhi","fhihi","fmean",
    "expected_sqr_err","xknown","fknown","Fknown")
)

expect_equal(
  post_analysis$x,
  xv
)

expect_equal(
  post_analysis$density,
  fv
)

expect_equal(
  post_analysis$dx,
  dx
)

expect_equal(
  post_analysis$dx,
  dx
)

expect_equal(
  post_analysis$xknown,
  0.9
)
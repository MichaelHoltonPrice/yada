# Test yada::get_J
# Test yada::get_K
# Test yada::get_z_length
# Test yada::get_num_var_multivariate

# A one variable, ordinal model -- powLawOrd / const
modSpec <- list()
modSpec$meanSpec  <- 'powLawOrd'
modSpec$noiseSpec <- 'const'
modSpec$J <- 1
modSpec$M <- 2

expect_equal(
  get_J(modSpec),
  1
)

expect_equal(
  get_K(modSpec),
  0
)

expect_equal(
  get_z_length(modSpec),
  0
)

expect_equal(
  get_num_var_multivariate('a',modSpec),
  1
)

expect_equal(
  get_num_var_multivariate('a',modSpec,preceding=T),
  0
)

expect_equal(
  get_num_var_multivariate('a',modSpec,j=1),
  1
)

expect_equal(
  get_num_var_multivariate('a',modSpec,j=1,preceding=T),
  0
)

expect_equal(
  get_num_var_multivariate('a',modSpec,i=1),
  1
)

expect_equal(
  get_num_var_multivariate('a',modSpec,i=1,preceding=T),
  0
)

expect_error(
  get_num_var_multivariate('a',modSpec,j=1,k=1),
  'If varName is a or alpha and j is specified, k should not be specified'
)

expect_error(
  get_num_var_multivariate('a',modSpec,j=1,k=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('a',modSpec,j=1,i=1),
  'If varName is a or alpha and j is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('a',modSpec,j=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('a',modSpec,j=2),
  'j = 2 is greater than the number of ordinal variables J = 1'
)

expect_error(
  get_num_var_multivariate('a',modSpec,k=1),
  'k = 1 is greater than the number of continuous variables K = 0'
)

# For some reason, the following does not work
#expect_error(
#  get_num_var_multivariate('a',modSpec,i=2),
#  'i = 2 is greater than the number of variables J+K = 1'
#)

#expect_equal(
#  get_num_var_multivariate('a',modSpec,k=1),
#  1
#)

expect_equal(
  get_num_var_multivariate('tau',modSpec),
  2
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,preceding=T),
  1
)

expect_equal(
  get_num_var_multivariate('tau',modSpec),
  2
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,preceding=T),
  1
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,j=1),
  2
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,j=1,preceding=T),
  1
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,i=1),
  2
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,i=1,preceding=T),
  1
)

expect_error(
  get_num_var_multivariate('tau',modSpec,j=1,k=1),
  'If varName is tau, k should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',modSpec,j=1,i=1),
  'If varName is tau and j is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',modSpec,j=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('tau',modSpec,k=1),
  'If varName is tau, k should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',modSpec,j=2),
  'j = 2 is greater than the number of ordinal variables J = 1'
)
expect_equal(
  get_num_var_multivariate('alpha',modSpec),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,preceding=T),
  3
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,j=1),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,j=1,preceding=T),
  3
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,i=1),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,i=1,preceding=T),
  3
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,j=1,k=1),
  'If varName is a or alpha and j is specified, k should not be specified'
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,j=1,k=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,j=1,i=1),
  'If varName is a or alpha and j is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,j=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,j=2),
  'j = 2 is greater than the number of ordinal variables J = 1'
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,k=1),
  'k = 1 is greater than the number of continuous variables K = 0'
)

expect_equal(
  get_num_var_multivariate('z',modSpec),
  0
)

expect_equal(
  get_num_var_multivariate('z',modSpec,preceding=T),
  4
)

expect_error(
  get_num_var_multivariate('notAVar',modSpec),
  paste('Unrecognized variable notAVar')
)

# A one variable, continuous model -- powLaw / const
modSpec <- list()
modSpec$meanSpec  <- 'powLaw'
modSpec$noiseSpec <- 'const'
modSpec$K <- 1

expect_equal(
  get_J(modSpec),
  0
)

expect_equal(
  get_K(modSpec),
  1
)

expect_equal(
  get_z_length(modSpec),
  0
)

expect_equal(
  get_num_var_multivariate('a',modSpec),
  3
)

expect_equal(
  get_num_var_multivariate('a',modSpec,preceding=T),
  0
)

expect_equal(
  get_num_var_multivariate('a',modSpec,k=1),
  3
)

expect_equal(
  get_num_var_multivariate('a',modSpec,k=1,preceding=T),
  0
)

expect_equal(
  get_num_var_multivariate('a',modSpec,i=1),
  3
)

expect_equal(
  get_num_var_multivariate('a',modSpec,i=1,preceding=T),
  0
)

expect_error(
  get_num_var_multivariate('a',modSpec,j=1,k=1),
  'If varName is a or alpha and j is specified, k should not be specified'
)

expect_error(
  get_num_var_multivariate('a',modSpec,j=1,k=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('a',modSpec,j=1,i=1),
  'If varName is a or alpha and j is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('a',modSpec,j=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('a',modSpec,k=2),
  'k = 2 is greater than the number of continuous variables K = 1'
)

expect_error(
  get_num_var_multivariate('a',modSpec,j=1),
  'j = 1 is greater than the number of ordinal variables J = 0'
)

expect_equal(
  get_num_var_multivariate('tau',modSpec),
  0
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,preceding=T),
  3
)

expect_error(
  get_num_var_multivariate('tau',modSpec,j=1,k=1),
  'If varName is tau, k should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',modSpec,j=1,i=1),
  'If varName is tau and j is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',modSpec,j=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('tau',modSpec,k=1),
  'If varName is tau, k should not be specified'
)

expect_error(
  get_num_var_multivariate('tau',modSpec,j=2),
  'j = 2 is greater than the number of ordinal variables J = 0'
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,preceding=T),
  3
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,k=1),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,k=1,preceding=T),
  3
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,i=1),
  1
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,i=1,preceding=T),
  3
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,j=1,k=1),
  'If varName is a or alpha and j is specified, k should not be specified'
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,j=1,k=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,k=1,i=1),
  'If varName is a or alpha and k is specified, i should not be specified'
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,k=1,i=1,preceding=T),
  'Only one of j, k, or i should be specified'
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,j=1),
  'j = 1 is greater than the number of ordinal variables J = 0'
)

expect_error(
  get_num_var_multivariate('alpha',modSpec,k=2),
  'k = 2 is greater than the number of continuous variables K = 1'
)

expect_equal(
  get_num_var_multivariate('z',modSpec),
  0
)

expect_equal(
  get_num_var_multivariate('z',modSpec,preceding=T),
  4
)

expect_error(
  get_num_var_multivariate('notAVar',modSpec),
  paste('Unrecognized variable notAVar')
)

# A six variable, conditionally dependent model with logOrd
modSpec <- list()
modSpec$meanSpec=c('powLawOrd','logOrd','powLawOrd','powLaw','powLaw','powLaw')
modSpec$noiseSpec <- c('const','lin_pos_int','lin_pos_int','lin_pos_int','const','const')
modSpec$J <- 3
modSpec$K <- 3
modSpec$M <- c(2,3,2)
modSpec$cdepSpec <- 'dep'
modSpec$cdepGroups <- c(1,2,1,3,NA,2)

expect_equal(
  get_J(modSpec),
  3
)

expect_equal(
  get_K(modSpec),
  3
)

expect_equal(
  get_z_length(modSpec),
  5
)

expect_equal(
  get_num_var_multivariate('a',modSpec),
  11
)

expect_equal(
  get_num_var_multivariate('a',modSpec,preceding=T),
  0
)

#expect_equal(
#  get_num_var_multivariate('a',modSpec,j=1),
#  1
#)

#expect_equal(
#  get_num_var_multivariate('a',modSpec,j=1,preceding=T),
#  0
#)

#expect_equal(
#  get_num_var_multivariate('a',modSpec,i=1),
#  1
#)

#expect_equal(
#  get_num_var_multivariate('a',modSpec,i=1,preceding=T),
#  0
#)

#expect_equal(
#  get_num_var_multivariate('a',modSpec,j=2),
#  0
#)

#expect_equal(
#  get_num_var_multivariate('a',modSpec,j=2,preceding=T),
#  1
#)

#expect_equal(
#  get_num_var_multivariate('a',modSpec,i=2),
#  0
#)

#expect_equal(
#  get_num_var_multivariate('a',modSpec,i=2,preceding=T),
#  1
#)

expect_equal(
  get_num_var_multivariate('a',modSpec,j=3),
  1
)

expect_equal(
  get_num_var_multivariate('a',modSpec,j=3,preceding=T),
  1
)

expect_equal(
  get_num_var_multivariate('a',modSpec,i=3),
  1
)

expect_equal(
  get_num_var_multivariate('a',modSpec,i=3,preceding=T),
  1
)

expect_equal(
  get_num_var_multivariate('a',modSpec,k=3),
  3
)

expect_equal(
  get_num_var_multivariate('a',modSpec,k=3,preceding=T),
  8
)

expect_equal(
  get_num_var_multivariate('a',modSpec,i=6),
  3
)

expect_equal(
  get_num_var_multivariate('a',modSpec,i=6,preceding=T),
  8
)

expect_equal(
  get_num_var_multivariate('tau',modSpec),
  7
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,preceding=T),
  11
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,j=3),
  2
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,j=3,preceding=T),
  16
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,i=3),
  2
)

expect_equal(
  get_num_var_multivariate('tau',modSpec,i=3,preceding=T),
  16
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec),
  9
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,preceding=T),
  18
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,j=3),
  2
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,j=3,preceding=T),
  21
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,i=3),
  2
)

expect_equal(
  get_num_var_multivariate('alpha',modSpec,i=3,preceding=T),
  21
)

expect_equal(
  get_num_var_multivariate('z',modSpec),
  5
)

expect_equal(
  get_num_var_multivariate('z',modSpec,preceding=T),
  27
)


# Test get_var_index_multivariate with a six variable model
modSpec <- list(meanSpec=c('logOrd','powLawOrd','linOrd','powLaw','powLaw','powLaw'))
modSpec$noiseSpec <- c('const','lin_pos_int','lin_pos_int','const','const','lin_pos_int')
modSpec$J <- 3
modSpec$K <- 3
modSpec$M <- c(2,3,2)
modSpec$cdepSpec <- 'dep' # conditionally dependent
modSpec$cdepGroups <- c(1,2,1,3,NA,2)


expect_equal(
  get_var_index_multivariate('z',modSpec),
  27:31
)

expect_equal(
  get_var_index_multivariate('z',modSpec,i1=1,i2=3),
  27
)

expect_equal(
  get_var_index_multivariate('z',modSpec,i1=2,i2=6),
  28
)

expect_equal(
  get_var_index_multivariate('z',modSpec,i1=1,i2=2),
  29
)

expect_equal(
  get_var_index_multivariate('z',modSpec,i1=1,i2=4),
  30
)

expect_equal(
  get_var_index_multivariate('z',modSpec,i1=6,i2=4),
  31
)

expect_error(
  get_var_index_multivariate('z',modSpec,i1=2,i2=2),
  'i1 should not equal i2'
)

expect_error(
  get_var_index_multivariate('z',modSpec,i1=2,i2=5),
  'Correlation requested for a variable with no correlations'
)

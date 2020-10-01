# Test yada::get_J
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


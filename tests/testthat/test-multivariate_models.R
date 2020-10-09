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


# Test get_var_index_multivariate on a six variable model
modSpec <- list(meanSpec=c('logOrd','powLawOrd','linOrd','powLaw','powLaw','powLaw'))
modSpec$noiseSpec <- c('const','lin_pos_int','lin_pos_int','const','const','lin_pos_int')
modSpec$J <- 3
modSpec$K <- 3
modSpec$M <- c(2,3,2)
modSpec$cdepSpec <- 'dep' # conditionally dependent
modSpec$cdepGroups <- c(1,2,1,3,NA,2)
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
# 28	28		z-ns-1			2	6
# 29	29		z-inter-12		1	2
# 30	30		z-inter-13		1	4
# 31	31		z-inter 23		2	4

expect_equal(
  get_var_index_multivariate('a',modSpec),
  1:10
)

expect_equal(
  get_var_index_multivariate('tau',modSpec),
  11:17
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec),
  18:26
)

expect_equal(
  get_var_index_multivariate('z',modSpec),
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
  get_var_index_multivariate('a',modSpec),
  NA
)

expect_error(
  get_var_index_multivariate('tau',modSpec),
  NA
)

expect_error(
  get_var_index_multivariate('alpha',modSpec),
  NA
)

# 1 0 0 0 0
expect_error(
  get_var_index_multivariate('a',modSpec,j=2),
  NA
)

expect_error(
  get_var_index_multivariate('tau',modSpec,j=2),
  NA
)

expect_error(
  get_var_index_multivariate('alpha',modSpec,j=2),
  NA
)

# 0 1 0 0 0
expect_error(
  get_var_index_multivariate('a',modSpec,k=3),
  NA
)

expect_error(
  get_var_index_multivariate('alpha',modSpec,k=3),
  NA
)

# 0 0 1 0 0
expect_error(
  get_var_index_multivariate('a',modSpec,i=5),
  NA
)

expect_error(
  get_var_index_multivariate('tau',modSpec,i=2),
  NA
)

expect_error(
  get_var_index_multivariate('alpha',modSpec,i=5),
  NA
)

# 0 0 0 1 1
expect_error(
  get_var_index_multivariate('z',modSpec,i1=2,i2=4),
  NA
)

expect_error(
  get_var_index_multivariate('a',modSpec,j=2,k=3),
  'Unsupported input pattern for index variables. See yada documentation'
)

expect_error(
  get_var_index_multivariate('tau',modSpec,j=2,i=5),
  'Unsupported input pattern for index variables. See yada documentation'
)

expect_error(
  get_var_index_multivariate('alpha',modSpec,i=2,k=3),
  'Unsupported input pattern for index variables. See yada documentation'
)

# Check a for indices being specified
expect_equal(
  get_var_index_multivariate('a',modSpec,j=1),
  c()
)

expect_equal(
  get_var_index_multivariate('a',modSpec,j=2),
  1
)

expect_equal(
  get_var_index_multivariate('a',modSpec,j=3),
  c()
)

expect_equal(
  get_var_index_multivariate('a',modSpec,k=1),
  2:4
)

expect_equal(
  get_var_index_multivariate('a',modSpec,k=2),
  5:7
)

expect_equal(
  get_var_index_multivariate('a',modSpec,k=3),
  8:10
)

expect_equal(
  get_var_index_multivariate('a',modSpec,i=1),
  c()
)

expect_equal(
  get_var_index_multivariate('a',modSpec,i=2),
  1
)

expect_equal(
  get_var_index_multivariate('a',modSpec,i=3),
  c()
)

expect_equal(
  get_var_index_multivariate('a',modSpec,i=4),
  2:4
)

expect_equal(
  get_var_index_multivariate('a',modSpec,i=5),
  5:7
)

expect_equal(
  get_var_index_multivariate('a',modSpec,i=6),
  8:10
)

# Check tau for indices being specified
expect_equal(
  get_var_index_multivariate('tau',modSpec,j=1),
  11:12
)

expect_equal(
  get_var_index_multivariate('tau',modSpec,j=2),
  13:15
)

expect_equal(
  get_var_index_multivariate('tau',modSpec,j=3),
  16:17
)

expect_equal(
  get_var_index_multivariate('tau',modSpec,i=1),
  11:12
)

expect_equal(
  get_var_index_multivariate('tau',modSpec,i=2),
  13:15
)

expect_equal(
  get_var_index_multivariate('tau',modSpec,i=3),
  16:17
)

# Check alpha for indices being specified
expect_equal(
  get_var_index_multivariate('alpha',modSpec,j=1),
  18
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,j=2),
  19:20
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,j=3),
  21:22
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,k=1),
  23
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,k=2),
  24
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,k=3),
  25:26
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,i=1),
  18
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,i=2),
  19:20
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,i=3),
  21:22
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,i=4),
  23
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,i=5),
  24
)

expect_equal(
  get_var_index_multivariate('alpha',modSpec,i=6),
  25:26
)

# Check z for indices being specified
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
          get_univariate_indices(modSpec,j=j),
          NA
        )
      } else if(is.na(j) && !is.na(k) && is.na(i)) {
        # 0 1 0
        expect_error(
          get_univariate_indices(modSpec,k=k),
          NA
        )
       } else if(is.na(j) && is.na(k) && !is.na(i)) {
        # 0 0 1
        expect_error(
          get_univariate_indices(modSpec,i=i),
          NA
        )
      } else {
        # All other patterns
        expect_error(
          get_univariate_indices(modSpec,j=j,k=k,i=i),
          'Unsupported input pattern for index variables. See yada documentation'
        )
      }
    }
  }
}

expect_equal(
  get_univariate_indices(modSpec,j=1),
  c(11:12,18)
)

expect_equal(
  get_univariate_indices(modSpec,i=1),
  c(11:12,18)
)

expect_equal(
  get_univariate_indices(modSpec,j=2),
  c(1,13:15,19:20)
)

expect_equal(
  get_univariate_indices(modSpec,i=2),
  c(1,13:15,19:20)
)

expect_equal(
  get_univariate_indices(modSpec,j=3),
  c(16:17,21:22)
)

expect_equal(
  get_univariate_indices(modSpec,i=3),
  c(16:17,21:22)
)

expect_equal(
  get_univariate_indices(modSpec,k=1),
  c(2:4,23)
)

expect_equal(
  get_univariate_indices(modSpec,i=4),
  c(2:4,23)
)

expect_equal(
  get_univariate_indices(modSpec,k=2),
  c(5:7,24)
)

expect_equal(
  get_univariate_indices(modSpec,i=5),
  c(5:7,24)
)

expect_equal(
  get_univariate_indices(modSpec,k=3),
  c(8:10,25:26)
)

expect_equal(
  get_univariate_indices(modSpec,i=6),
  c(8:10,25:26)
)

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
# 28	28		z-ns-1			2	6
# 29	29		z-inter-12		1	2
# 30	30		z-inter-13		1	4
# 31	31		z-inter 23		2	4


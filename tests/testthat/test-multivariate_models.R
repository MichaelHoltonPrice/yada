library(doParallel)
registerDoParallel(detectCores()-2)
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

# Test get_multivariate_transform_categories on the preceding model
expect_equal(
  get_multivariate_transform_categories(modSpec),
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

# TODO: add tests for is_cdep
# TODO: add tests for get_multivariate_transform_categories

# TODO: Check get_var_index_multivariate_fast and
#       get_var_index_multivariate_mapping for all cases used for
#       get_var_index_multivariate.

# Test get_var_index_multivariate_mapping and get_var_index_multivariate_fast
# on the preceding model

# Build the mapping, checking that no error is thrown
expect_error(
  mapping <- get_var_index_multivariate_mapping(modSpec),
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
  get_var_index_multivariate     ('a',modSpec)
)

expect_equal(
  get_var_index_multivariate_fast('tau',mapping),
  get_var_index_multivariate     ('tau',modSpec)
)

expect_equal(
  get_var_index_multivariate_fast('alpha',mapping),
  get_var_index_multivariate     ('alpha',modSpec)
)

expect_equal(
  get_var_index_multivariate_fast('z',mapping),
  get_var_index_multivariate     ('z',modSpec)
)

# Check for input pattern 1 0 0 0 0, j specified
J <- get_J(modSpec)
for(j in 1:J) {
  expect_equal(
    get_var_index_multivariate_fast('a',mapping,j=j),
    get_var_index_multivariate     ('a',modSpec,j=j)
  )

  expect_equal(
    get_var_index_multivariate_fast('tau',mapping,j=j),
    get_var_index_multivariate     ('tau',modSpec,j=j)
  )

  expect_equal(
    get_var_index_multivariate_fast('alpha',mapping,j=j),
    get_var_index_multivariate     ('alpha',modSpec,j=j)
  )
}

# Check for input pattern 0 1 0 0 0, k specified
K <- get_J(modSpec)
for(k in 1:K) {
  expect_equal(
    get_var_index_multivariate_fast('a',mapping,k=k),
    get_var_index_multivariate     ('a',modSpec,k=k)
  )

  expect_equal(
    get_var_index_multivariate_fast('alpha',mapping,k=k),
    get_var_index_multivariate     ('alpha',modSpec,k=k)
  )
}

# Check for input pattern 0 0 1 0 0, i specified
for(i in 1:(J+K)) {
  expect_equal(
    get_var_index_multivariate_fast('a',mapping,i=i),
    get_var_index_multivariate     ('a',modSpec,i=i)
  )

  if(i <= J) {
    expect_equal(
      get_var_index_multivariate_fast('tau',mapping,i=i),
      get_var_index_multivariate     ('tau',modSpec,i=i)
    )
  }

  expect_equal(
    get_var_index_multivariate_fast('alpha',mapping,i=i),
    get_var_index_multivariate     ('alpha',modSpec,i=i)
  )
}

# Check for input pattern 0 0 0 1 1, i1 and i2 specified

counter <- 0 # using a counter is probably clearer than using combinadic indexing
for(i1 in 1:(J+K-1)) {
  for(i2 in (i1+1):(J+K)) {
    counter <- counter + 1
    if(!is.na(modSpec$cdepGroups[i1]) && !is.na(modSpec$cdepGroups[i2])) {
      expect_equal(
        get_var_index_multivariate_fast('z',mapping,i1=i1,i2=i2),
        get_var_index_multivariate     ('z',modSpec,i1=i1,i2=i2)
      )
    } else {
      expect_error(
        get_var_index_multivariate_fast('z',mapping,i1=i1,i2=i2),
        'Correlation requested for a variable with no correlations'
      )
    }
  }
}


# Check calculations related to z
# A four variable (does not exercise all functionality; TODO: check also linOrd and hyperb)
modSpec <- list(meanSpec=c('logOrd','powLawOrd','powLaw','powLaw'))
modSpec$noiseSpec <- c('const','lin_pos_int','lin_pos_int','const')
modSpec$J <- 2
modSpec$M <- c(2,3)
modSpec$K <- 2
modSpec$cdepSpec <- 'dep' # conditionally dependent
modSpec$cdepGroups <- c(1,2,1,2)

expect_error(
  mapping <- get_var_index_multivariate_mapping(modSpec),
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
z_full_direct <- c(zcr,zns[1],zcr,zcr,zns[2],zcr)

expect_equal(
  get_z_full_fast(th_y,mapping),
  z_full_direct
)


zMat_direct <- diag(modSpec$J+modSpec$K)
zMat_direct[1,2] <- zcr
zMat_direct[1,3] <- zns[1]
zMat_direct[1,4] <- zcr
zMat_direct[2,3] <- zcr
zMat_direct[2,4] <- zns[2]
zMat_direct[3,4] <- zcr
zMat_direct[2,1] <- zMat_direct[1,2]
zMat_direct[3,1] <- zMat_direct[1,3]
zMat_direct[4,1] <- zMat_direct[1,4]
zMat_direct[3,2] <- zMat_direct[2,3]
zMat_direct[4,2] <- zMat_direct[2,4]
zMat_direct[4,3] <- zMat_direct[3,4]

expect_equal(
  get_z_full_fast(th_y,mapping,asMatrix=T),
  zMat_direct
)


if(F) {
# Test calc_noise_sd
x <- c(0,2,4)
N <- length(x)
sdMat_direct <- matrix(NA,4,N)
#zz <- try(calc_noise_vect_fast(th_y,x[1],mapping))
sdMat_direct[,1] <- calc_noise_vect_fast(th_y,x[1],mapping)
sdMat_direct[,2] <- calc_noise_vect_fast(th_y,x[2],mapping)
sdMat_direct[,3] <- calc_noise_vect_fast(th_y,x[3],mapping)


x <- c(0,2,4)
sigArray_direct <- array(NA,c(4,4,length(x)))
sigArray_direct[,,1] <- sdMat_direct[,1]%*%  t(sdMat_direct[,1]) * zMat_direct
sigArray_direct[,,2] <- sdMat_direct[,2]%*%  t(sdMat_direct[,2]) * zMat_direct
sigArray_direct[,,3] <- sdMat_direct[,3]%*%  t(sdMat_direct[,3]) * zMat_direct

z <- try(calc_Sigma_fast(th_y,x[1],mapping))
print('')
print(z)
print(sigArray_direct[,,1])
expect_equal(
  calc_Sigma_fast(th_y,x[1],mapping),
  sigArray_direct[,,1]
)

if(F) {
expect_equal(
  calc_Sigma_fast(th_y,x[2],mapping),
  sigArray_direct[,,2]
)

expect_equal(
  calc_Sigma_fast(th_y,x[3],mapping),
  sigArray_direct[,,3]
)
}

# Test calc_neg_log_lik_multivariate_core

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

x1 <- 1
x2 <- 2

# Test a model with one ordinal variable
modSpec <- list(meanSpec='powLawOrd')
modSpec$J <- 1
modSpec$K <- 0
modSpec$M <- 2
modSpec$noiseSpec  <- 'const'

th_y <- c(rho1,tau1,s1)

expect_error(
  mapping <- get_var_index_multivariate_mapping(modSpec),
  NA
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x1,0,mapping),
  -log(pnorm((tau1[1]-x1^rho1)/s1))
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x2,0,mapping),
  -log(pnorm((tau1[1]-x2^rho1)/s1))
)


expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x1,1,mapping),
  -log(pnorm((tau1[2]-x1^rho1)/s1) - pnorm((tau1[1]-x1^rho1)/s1))
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x2,1,mapping),
  -log(pnorm((tau1[2]-x2^rho1)/s1) - pnorm((tau1[1]-x2^rho1)/s1))
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x1,2,mapping),
  -log(1-pnorm((tau1[2]-x1^rho1)/s1))
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x2,2,mapping),
  -log(1-pnorm((tau1[2]-x2^rho1)/s1))
)


# Test a model with one continuous variable
modSpec <- list(meanSpec='powLaw')
modSpec$noiseSpec  <- 'const'
modSpec$J <- 0
modSpec$K <- 1
modSpec$cdepSpec <- 'indep'

th_y <- c(r1,a1,b1,s3)

expect_error(
  mapping <- get_var_index_multivariate_mapping(modSpec),
  NA
)

w1 <- 1.2
w2 <- 1.4
expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x1,w1,mapping),
  -dnorm(w1,a1*x1^r1+b1,s3,log=T)
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x2,w2,mapping),
  -dnorm(w2,a1*x2^r1+b1,s3,log=T)
)

# Test a model with one ordinal and one continuous variable
modSpec <- list(meanSpec=c('powLawOrd','powLaw'))
modSpec$noiseSpec  <- c('const','const')
modSpec$J <- 1
modSpec$K <- 1
modSpec$M <- 2
modSpec$cdepSpec <- 'dep'
modSpec$cdepGroups <- c(1,2)

th_y <- c(rho1,r1,a1,b1,tau1,s1,s3,z[2])

expect_error(
  mapping <- get_var_index_multivariate_mapping(modSpec),
  NA
)

w1 <- 1.2
w2 <- 1.4

mu_bar1 <- x1^rho1 + z[2]*s1/s3*(w1-a1*x1^r1-b1)
s1_bar <- s1*sqrt(1-z[2]^2)
mu_bar2 <- x2^rho1 + z[2]*s1/s3*(w2-a1*x2^r1-b1)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x1,matrix(c(0,w1)),mapping),
  -dnorm(w1,a1*x1^r1+b1,s3,log=T) - log(pnorm((tau1[1]-mu_bar1)/s1_bar))
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x1,matrix(c(1,w1)),mapping),
  -dnorm(w1,a1*x1^r1+b1,s3,log=T) - log(pnorm((tau1[2]-mu_bar1)/s1_bar) - pnorm((tau1[1]-mu_bar1)/s1_bar))
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x1,matrix(c(2,w1)),mapping),
  -dnorm(w1,a1*x1^r1+b1,s3,log=T) - log(1 - pnorm((tau1[2]-mu_bar1)/s1_bar))
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x2,matrix(c(0,w2)),mapping),
  -dnorm(w2,a1*x2^r1+b1,s3,log=T) - log(pnorm((tau1[1]-mu_bar2)/s1_bar))
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x2,matrix(c(1,w2)),mapping),
  -dnorm(w2,a1*x2^r1+b1,s3,log=T) - log(pnorm((tau1[2]-mu_bar2)/s1_bar) - pnorm((tau1[1]-mu_bar2)/s1_bar))
)

expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x2,matrix(c(2,w2)),mapping),
  -dnorm(w2,a1*x2^r1+b1,s3,log=T) - log(1 - pnorm((tau1[2]-mu_bar2)/s1_bar))
)

# Test a model with two ordinal and two continuous variable
modSpec <- list(meanSpec=c('powLawOrd','powLawOrd','powLaw','powLaw'))
modSpec$noiseSpec  <- rep('const',4)
modSpec$J <- 2
modSpec$K <- 2
modSpec$M <- c(2,2)
modSpec$cdepSpec <- 'dep'
modSpec$cdepGroups <- c(1,2,3,4)

th_y <- c(rho1,rho2,r1,a1,b1,r2,a2,b2,tau1,tau2,s1,s2,s3,s4,z)

expect_error(
  mapping <- get_var_index_multivariate_mapping(modSpec),
  NA
)

Y <- matrix(c(0,1,100,50,1,2,160,92),nrow=4)
# Directly Calculate likelihood for both observations
g1 <- x1^c(rho1,rho2)
g2 <- x2^c(rho1,rho2)
h1 <- c(a1,a2)*x1^c(r1,r2) + c(b1,b2)
h2 <- c(a1,a2)*x2^c(r1,r2) + c(b1,b2)

S1 <- calc_Sigma_fast(th_y,x1,mapping)
S2 <- calc_Sigma_fast(th_y,x2,mapping)

condMean1 <- g1 + S1[1:2,3:4] %*% solve(S1[3:4,3:4]) %*% as.matrix(c(100,50) - h1)
condCov1 <- S1[1:2,1:2] - S1[1:2,3:4] %*% solve(S1[3:4,3:4]) %*% S1[3:4,1:2]
logLik1 <- mvtnorm::dmvnorm(c(100,50),h1,S1[3:4,3:4],log=T)
condIntegral1 <- mvtnorm::pmvnorm(lower=c(-Inf,tau2[1]),upper=c(tau1[1],tau2[2]),mean=as.vector(condMean1),sigma=condCov1)
logLik1 <- logLik1 + log(as.numeric(condIntegral1))
expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x1,Y[,1],mapping),
  -logLik1
)

condMean2 <- g2 + S2[1:2,3:4] %*% solve(S2[3:4,3:4]) %*% as.matrix(c(160,92) - h2)
condCov2 <- S2[1:2,1:2] - S2[1:2,3:4] %*% solve(S2[3:4,3:4]) %*% S2[3:4,1:2]
logLik2 <- mvtnorm::dmvnorm(c(160,92),h2,S2[3:4,3:4],log=T)
condIntegral2 <- mvtnorm::pmvnorm(lower=c(tau1[1],tau2[2]),upper=c(tau1[2],Inf),mean=as.vector(condMean2),sigma=condCov2)
logLik2 <- logLik2 + log(as.numeric(condIntegral2))
expect_equal(
  calc_neg_log_lik_multivariate_core(th_y,x2,Y[,2],mapping),
  -logLik2
)

modSpecList <- list()
modSpecList[[1]] <- modSpec
modSpecList[[2]] <- modSpec
mappingList <- list()
mappingList[[1]] <- mapping
mappingList[[2]] <- mapping

#expect_equal(
#  calc_neg_log_lik_vect_multivariate_core(th_y,c(x1,x2),Y,modSpecList,mappingList),
#  -c(lik1,lik2)
#)
}

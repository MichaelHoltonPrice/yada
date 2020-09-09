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


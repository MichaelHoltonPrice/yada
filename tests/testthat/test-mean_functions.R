# Test yada::meanSpec_int2str
expect_equal(
  yada::meanSpec_int2str(0),
  'powLawOrd'
)

expect_equal(
  yada::meanSpec_int2str(1),
  'logOrd'
)

expect_equal(
  yada::meanSpec_int2str(2),
  'linOrd'
)

expect_equal(
  yada::meanSpec_int2str(3),
  'powLaw'
)

expect_error(
  yada::meanSpec_int2str(c(0,1)),
  'The input, meanSpec_int, does not have length 1'
)

expect_error(
  yada::meanSpec_int2str('powLawOrd'),
  'The input, meanSpec_int, is not numeric'
)

expect_error(
  yada::meanSpec_int2str(4),
  'Unrecognized case, meanSpec_int = 4'
)

# Test yada::calc_mean
expect_equal(
  yada::calc_mean(c(1,1.5),'powLawOrd',0.5),
  c(1,1.5)^0.5
)

expect_equal(
  yada::calc_mean(c(1,1.5),0,0.5),
  c(1,1.5)^0.5
)

expect_equal(
  yada::calc_mean(c(1,1.5),'logOrd'),
  log(c(1,1.5))
)

expect_equal(
  yada::calc_mean(c(1,1.5),1),
  log(c(1,1.5))
)

expect_equal(
  yada::calc_mean(c(1,1.5),'linOrd'),
  c(1,1.5)
)

expect_equal(
  yada::calc_mean(c(1,1.5),2),
  c(1,1.5)
)

expect_equal(
  yada::calc_mean(c(1,1.5),'powLaw',c(0.5,0.2,-.4)),
  0.2*c(1,1.5)^0.5 - 0.4
)

expect_equal(
  yada::calc_mean(c(1,1.5),3,c(0.5,0.2,-.4)),
  0.2*c(1,1.5)^0.5 - 0.4
)

expect_error(
  yada::calc_mean(c(1,1.5),'notAModel',c(0.5,0.2,-.4)),
  'Unrecognized case, meanSpec = notAModel'
)


# Test yada::get_get_num_var_mean
expect_equal(
  yada::get_num_var_mean('powLawOrd'),
  1
)

expect_equal(
  yada::get_num_var_mean(0),
  1
)

expect_equal(
  yada::get_num_var_mean('logOrd'),
  0
)

expect_equal(
  yada::get_num_var_mean(1),
  0
)

expect_equal(
  yada::get_num_var_mean('linOrd'),
  0
)

expect_equal(
  yada::get_num_var_mean(2),
  0
)

expect_equal(
  yada::get_num_var_mean('powLaw'),
  3
)

expect_equal(
  yada::get_num_var_mean(3),
  3
)

expect_error(
  yada::get_num_var_mean('notAModel'),
  'Unrecognized case, meanSpec = notAModel'
)

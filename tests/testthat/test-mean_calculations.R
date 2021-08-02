# Test yada::mean_spec_int_to_str
expect_equal(
  yada::mean_spec_int_to_str(0),
  'pow_law_ord'
)

expect_equal(
  yada::mean_spec_int_to_str(1),
  'log_ord'
)

expect_equal(
  yada::mean_spec_int_to_str(2),
  'lin_ord'
)

expect_equal(
  yada::mean_spec_int_to_str(3),
  'pow_law'
)

expect_error(
  yada::mean_spec_int_to_str(c(0,1)),
  'The input, mean_spec_int, does not have length 1'
)

expect_error(
  yada::mean_spec_int_to_str('pow_law_ord'),
  'The input, mean_spec_int, is not numeric'
)

expect_error(
  yada::mean_spec_int_to_str(4),
  'Unrecognized case, mean_spec_int = 4'
)

# Test yada::calc_mean
expect_equal(
  yada::calc_mean(c(1,1.5),'pow_law_ord',0.5),
  c(1,1.5)^0.5
)

expect_equal(
  yada::calc_mean(c(1,1.5),0,0.5),
  c(1,1.5)^0.5
)

expect_equal(
  yada::calc_mean(c(1,1.5),'log_ord'),
  log(c(1,1.5))
)

expect_equal(
  yada::calc_mean(c(1,1.5),1),
  log(c(1,1.5))
)

expect_equal(
  yada::calc_mean(c(1,1.5),'lin_ord'),
  c(1,1.5)
)

expect_equal(
  yada::calc_mean(c(1,1.5),2),
  c(1,1.5)
)

expect_equal(
  yada::calc_mean(c(1,1.5),'pow_law',c(0.5,0.2,-.4)),
  0.2*c(1,1.5)^0.5 - 0.4
)

expect_equal(
  yada::calc_mean(c(1,1.5),3,c(0.5,0.2,-.4)),
  0.2*c(1,1.5)^0.5 - 0.4
)

expect_error(
  yada::calc_mean(c(1,1.5),'not_a_model',c(0.5,0.2,-.4)),
  'Unrecognized case, mean_spec = not_a_model'
)


# Test yada::get_get_num_var_mean
expect_equal(
  yada::get_num_var_mean('pow_law_ord'),
  1
)

expect_equal(
  yada::get_num_var_mean(0),
  1
)

expect_equal(
  yada::get_num_var_mean('log_ord'),
  0
)

expect_equal(
  yada::get_num_var_mean(1),
  0
)

expect_equal(
  yada::get_num_var_mean('lin_ord'),
  0
)

expect_equal(
  yada::get_num_var_mean(2),
  0
)

expect_equal(
  yada::get_num_var_mean('pow_law'),
  3
)

expect_equal(
  yada::get_num_var_mean(3),
  3
)

expect_error(
  yada::get_num_var_mean('not_a_model'),
  'Unrecognized case, mean_spec = not_a_model'
)

# Test yada::get_mean_transform_categories
expect_equal(
  yada::get_mean_transform_categories('pow_law_ord'),
  1
)

expect_equal(
  yada::get_mean_transform_categories(0),
  1
)

expect_equal(
  yada::get_mean_transform_categories('log_ord'),
  c()
)

expect_equal(
  yada::get_mean_transform_categories(1),
  c()
)

expect_equal(
  yada::get_mean_transform_categories('lin_ord'),
  c()
)

expect_equal(
  yada::get_mean_transform_categories(2),
  c()
)

expect_equal(
  yada::get_mean_transform_categories('pow_law'),
  c(1,1,0)
)

expect_equal(
  yada::get_mean_transform_categories(3),
  c(1,1,0)
)

expect_error(
  yada::get_mean_transform_categories('not_a_model'),
  'Unrecognized case, mean_spec = not_a_model'
)

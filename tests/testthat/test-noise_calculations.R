# Test yada::noise_spec_int_to_str
expect_equal(
  yada::noise_spec_int_to_str(0),
  'const'
)

expect_equal(
  yada::noise_spec_int_to_str(1),
  'lin_pos_int'
)

expect_error(
  yada::noise_spec_int_to_str(c(0,1)),
  'The input, noise_spec_int, does not have length 1'
)

expect_error(
  yada::noise_spec_int_to_str('const'),
  'The input, noise_spec_int, is not numeric'
)

expect_error(
  yada::noise_spec_int_to_str(3),
  'Unrecognized case, noise_spec_int = 3'
)

# Test yada::calc_noise
expect_equal(
  yada::calc_noise(c(1,1.5),'const',0.5),
  c(0.5,0.5)
)

expect_equal(
  yada::calc_noise(c(1,1.5),0,0.5),
  c(0.5,0.5)
)

expect_equal(
  yada::calc_noise(c(1,1.5),'lin_pos_int',c(0.25,0.10)),
  0.25*(1 + c(1,1.5)*0.10)
)

expect_equal(
  yada::calc_noise(c(1,1.5),1,c(0.25,0.10)),
  0.25*(1 + c(1,1.5)*0.10)
)

expect_error(
  yada::calc_noise(c(1,1.5),'not_a_model',c(0.5,0.2,-.4)),
  'Unrecognized case, noise_spec = not_a_model'
)

# Test yada::get_get_num_var_noise
expect_equal(
  yada::get_num_var_noise('const'),
  1
)

expect_equal(
  yada::get_num_var_noise(0),
  1
)

expect_equal(
  yada::get_num_var_noise('lin_pos_int'),
  2
)

expect_equal(
  yada::get_num_var_noise(1),
  2
)

expect_error(
  yada::get_num_var_noise('not_a_model'),
  'Unrecognized case, noise_spec = not_a_model'
)

# Test yada::get_noise_transform_categories
expect_equal(
  yada::get_noise_transform_categories('const'),
  1
)

expect_equal(
  yada::get_noise_transform_categories(0),
  1
)

expect_equal(
  yada::get_noise_transform_categories('lin_pos_int'),
  c(1,1)
)

expect_equal(
  yada::get_noise_transform_categories(1),
  c(1,1)
)

expect_error(
  yada::get_noise_transform_categories('not_a_model'),
  'Unrecognized case, noise_spec = not_a_model'
)

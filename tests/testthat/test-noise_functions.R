# Test yada::noiseSpec_int2str
expect_equal(
  yada::noiseSpec_int2str(0),
  'const'
)

expect_equal(
  yada::noiseSpec_int2str(1),
  'lin_pos_int'
)

expect_error(
  yada::noiseSpec_int2str(c(0,1)),
  'The input, noiseSpec_int, does not have length 1'
)

expect_error(
  yada::noiseSpec_int2str('const'),
  'The input, noiseSpec_int, is not numeric'
)

expect_error(
  yada::noiseSpec_int2str(3),
  'Unrecognized case, noiseSpec_int = 3'
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
  yada::calc_noise(c(1,1.5),'notAModel',c(0.5,0.2,-.4)),
  'Unrecognized case, noiseSpec = notAModel'
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
  yada::get_num_var_noise('notAModel'),
  'Unrecognized case, noiseSpec = notAModel'
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
  yada::get_noise_transform_categories('notAModel'),
  'Unrecognized case, noiseSpec = notAModel'
)

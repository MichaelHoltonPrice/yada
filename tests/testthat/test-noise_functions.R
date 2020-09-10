# Test yada::noiseSpec_int2str
expect_equal(
  yada::noiseSpec_int2str(0),
  'const'
)

expect_equal(
  yada::noiseSpec_int2str(1),
  'lin_pos_int'
)

expect_equal(
  yada::noiseSpec_int2str(2),
  'hyperb'
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

expect_equal(
  yada::calc_noise(c(1,1.5),'hyperb',c(0.3,0.20,-.4)),
  (0.3/2)*(sqrt((c(1,1.5)-.4/.3)^2 + 4*.20*(.20+.4)/(.3^2)) + c(1,1.5)-.4/.3)
)

expect_equal(
  yada::calc_noise(c(1,1.5),2,c(0.3,0.20,-.4)),
  (0.3/2)*(sqrt((c(1,1.5)-.4/.3)^2 + 4*.20*(.20+.4)/(.3^2)) + c(1,1.5)-.4/.3)
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

expect_equal(
  yada::get_num_var_noise('hyperb'),
  3
)

expect_equal(
  yada::get_num_var_noise(2),
  3
)

expect_error(
  yada::get_num_var_noise('notAModel'),
  'Unrecognized case, noiseSpec = notAModel'
)

# Test yada::get_noise_transform_matrix
expect_equal(
  yada::get_noise_transform_matrix('const'),
  matrix(c(1,1,NA),nrow=1)
)

expect_equal(
  yada::get_noise_transform_matrix(0),
  matrix(c(1,1,NA),nrow=1)
)

expect_equal(
  yada::get_noise_transform_matrix('lin_pos_int'),
  matrix(c(1,1,1,2,NA,NA),nrow=2)
)

expect_equal(
  yada::get_noise_transform_matrix(1),
  matrix(c(1,1,1,2,NA,NA),nrow=2)
)

expect_equal(
  yada::get_noise_transform_matrix('hyperb'),
  matrix(c(1,1,0,1,2,3,NA,NA,NA),nrow=3)
)

expect_equal(
  yada::get_noise_transform_matrix(2),
  matrix(c(1,1,0,1,2,3,NA,NA,NA),nrow=3)
)

expect_error(
  yada::get_noise_transform_matrix('notAModel'),
  'Unrecognized case, noiseSpec = notAModel'
)

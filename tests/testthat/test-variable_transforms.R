# TODO:
# extract_univariate_param_vect

# Test yada::param_constr_to_unconstr
expect_equal(
  yada::param_constr_to_unconstr(c(1,1,.5,-2,.75,-.6,4,6),c(0,1,2,0,1,2,0,3)),
  c(1,0,gtools::logit((.5+1)/2),-2,log(.75),gtools::logit((-.6+1)/2),4,log(2))
)

# Test yada::param_unconstr_to_constr
expect_equal(
  yada::param_unconstr_to_constr(c(1,0,4,-2,log(.75),-2,4,log(2)),c(0,1,2,0,1,2,0,3)),
  c(1,1, -1 + 2/(1+exp(-4)),-2,.75,-1 + 2/(1+exp(2)),4,6)
)

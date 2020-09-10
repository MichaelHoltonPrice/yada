# Test yada::param_constr2unconstr
expect_equal(
  yada::param_constr2unconstr(c(1,1,.5,-2,.75,-.6),matrix(c(0,1,2,0,1,2,1:6,rep(NA,6)),nrow=6)),
  c(1,0,gtools::logit((.5+1)/2),-2,log(.75),gtools::logit((-.6+1)/2))
)

# Test yada::param_unconstr2constr
expect_equal(
  yada::param_unconstr2constr(c(1,0,4,-2,log(.75),-2),matrix(c(0,1,2,0,1,2,1:6,rep(NA,6)),nrow=6)),
  c(1,1, -1 + 2/(1+exp(-4)),-2,.75,-1 + 2/(1+exp(2)))
)

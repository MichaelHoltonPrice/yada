# Create a quadratic objective function with an optimum at (x0,y0), where
# x0 and y0 are inputs (this tests the functionality of the ... options) in
# the call to doGpBasedOptim.
quadObj <- function(param,x0,y0) {
  return( (param[1]-x0)^2 + (param[2]-y0)^2 )
}

# The following code both tests that the optimum (x0,y0) is found and that the
# variables in optimResult are correct.
expect_error(
  optimResult <- doGpBasedOptim(c(0.1,0.1),objFun=quadObj,control=list(scaleVect=c(1e-2,1e-2),seed=1000,maxEval=400,objTol=NA),x0=.5,y0=-1.5),
  NA
)

expect_equal(
  names(optimResult),
  c("paramMat","objVect","objVectFull","param_best","f_best","legData" )
)

expect_equal(
  dim(optimResult$paramMat),
  c(100,2)
)

expect_equal(
  dim(optimResult$objVect),
  c(100,1)
)

expect_equal(
  dim(optimResult$objVectFull),
  c(401,1)
)

expect_equal(
  optimResult$param_best,
  c(0.5,-1.5),
  tol=1e-4
)

expect_equal(
  optimResult$f_best,
  0,
  tol = 1e-6
)

expect_equal(
  length(optimResult$legData),
  1
)

expect_equal(
  names(optimResult$legData[[1]]),
  c("param0","f0","objFun","control0","control","saveFile")
)

expect_equal(
  optimResult$legData[[1]]$param0,
  c(0.1,0.1),
)

expect_equal(
  optimResult$legData[[1]]$f0,
  .4^2 + 1.6^2
)

expect_equal(
  all.equal(optimResult$legData[[1]]$objFun,quadObj),
  T
)

expect_equal(
  optimResult$legData[[1]]$control0,
  list(scaleVect=c(1e-2,1e-2),seed=1000,maxEval=400,objTol=NA),
)

expect_equal(
  optimResult$legData[[1]]$control,
  list(scaleVect=c(1e-2,1e-2),Nmodel=100,Ncand=100,maxEval=400,objTol=NA,catchErrors=F,seed=1000,verbose=F,showPlot=F),
)

expect_equal(
  optimResult$legData[[1]]$saveFile,
  NA
)

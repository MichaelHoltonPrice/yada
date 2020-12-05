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

# The following code does not test that the optimum (x0,y0) is found, but does
# test that doGpBasedOptim can be initialized with a save file. Use the
# tempdir() function to get the temp directory to use for the check.
saveFile1 <- paste0(tempdir(),'/leg1.rds')
saveFile2 <- paste0(tempdir(),'/leg2.rds')
if(file.exists(saveFile1)) {
  file.remove(saveFile1)
}

if(file.exists(saveFile2)) {
  file.remove(saveFile2)
}

expect_equal(
  file.exists(saveFile1),
  F
)

expect_error(
  optimResult1 <- doGpBasedOptim(c(0.1,0.1),objFun=quadObj,control=list(seed=2000,maxEval=60,Nmodel=20,Ncand=30),saveFile=saveFile1,x0=.5,y0=-1.5),
  NA
)

expect_equal(
  file.exists(saveFile1),
  T
)

expect_equal(
  file.exists(saveFile2),
  F
)

optimResult1_from_file <- readRDS(saveFile1)

expect_equal(
  optimResult1,
  optimResult1_from_file
)

expect_equal(
  names(optimResult1),
  c("paramMat","objVect","objVectFull","param_best","f_best","legData" )
)

expect_equal(
  dim(optimResult1$paramMat),
  c(20,2)
)

expect_equal(
  dim(optimResult1$objVect),
  c(20,1)
)

expect_equal(
  dim(optimResult1$objVectFull),
  c(61,1)
)

expect_equal(
  length(optimResult1$param_best),
  2
)

expect_equal(
  length(optimResult1$f_best),
  1
)

expect_equal(
  length(optimResult1$legData),
  1
)

expect_equal(
  names(optimResult1$legData[[1]]),
  c("param0","f0","objFun","control0","control","saveFile")
)

expect_equal(
  optimResult1$legData[[1]]$param0,
  c(0.1,0.1),
)

expect_equal(
  optimResult1$legData[[1]]$f0,
  .4^2 + 1.6^2
)

expect_equal(
  all.equal(optimResult1$legData[[1]]$objFun,quadObj),
  T
)

expect_equal(
  optimResult1$legData[[1]]$control0,
  list(seed=2000,maxEval=60,Nmodel=20,Ncand=30),
)

expect_equal(
  optimResult1$legData[[1]]$control,
  list(scaleVect=c(1e-4,1e-4),Nmodel=20,Ncand=30,maxEval=60,objTol=1e-4,catchErrors=F,seed=2000,verbose=F,showPlot=F),
)

expect_equal(
  optimResult1$legData[[1]]$saveFile,
  saveFile1
)

# Run a new leg using the save file. Reset some control variables.
expect_equal(
  file.exists(saveFile2),
  F
)

expect_error(
  optimResult2 <- doGpBasedOptim(saveFile1,control=list(Nmodel=30,Ncand=20,maxEval=70,seed=3000),saveFile=saveFile2,x0=.5,y0=-1.5),
  NA
)

expect_equal(
  file.exists(saveFile2),
  T
)

optimResult2_from_file <- readRDS(saveFile2)

expect_equal(
  optimResult2,
  optimResult2_from_file
)

expect_equal(
  names(optimResult2),
  c("paramMat","objVect","objVectFull","param_best","f_best","legData" )
)

expect_equal(
  dim(optimResult2$paramMat),
  c(30,2)
)

expect_equal(
  dim(optimResult2$objVect),
  c(30,1)
)

expect_equal(
  dim(optimResult2$objVectFull),
  c(131,1)
)

expect_equal(
  length(optimResult2$param_best),
  2
)

expect_equal(
  length(optimResult2$f_best),
  1
)

expect_equal(
  length(optimResult2$legData),
  2
)

expect_equal(
  names(optimResult2$legData[[1]]),
  c("param0","f0","objFun","control0","control","saveFile")
)

expect_equal(
  optimResult2$legData[[1]]$param0,
  c(0.1,0.1),
)

expect_equal(
  optimResult2$legData[[1]]$f0,
  .4^2 + 1.6^2
)

expect_equal(
  all.equal(optimResult2$legData[[1]]$objFun,quadObj),
  T
)

expect_equal(
  optimResult2$legData[[1]]$control0,
  list(seed=2000,maxEval=60,Nmodel=20,Ncand=30),
)

expect_equal(
  optimResult2$legData[[1]]$control,
  list(scaleVect=c(1e-4,1e-4),Nmodel=20,Ncand=30,maxEval=60,objTol=1e-4,catchErrors=F,seed=2000,verbose=F,showPlot=F),
)

expect_equal(
  optimResult2$legData[[1]]$saveFile,
  saveFile1
)

expect_equal(
  optimResult2$legData[[2]]$control0,
  list(Nmodel=30,Ncand=20,maxEval=70,seed=3000),
)

expect_equal(
  optimResult2$legData[[2]]$control,
  list(scaleVect=c(1e-4,1e-4),Nmodel=30,Ncand=20,maxEval=70,objTol=1e-4,catchErrors=F,seed=3000,verbose=F,showPlot=F),
)

expect_equal(
  optimResult2$legData[[2]]$saveFile,
  saveFile2
)

# Make sure an error is thrown if an invalid control variable is input
expect_error(
  doGpBasedOptim(c(0.1,0.1),objFun=quadObj,control=list(seed=2000,maxEval=60,Nmodel=20,Ncand=30,bad=T),saveFile=saveFile1,x0=.5,y0=-1.5),
  'Unrecognized control parameter, bad'
)

file.remove(saveFile1)
file.remove(saveFile2)

#' Solve an unconstrained minimization problem that involves random selection of
#' candidates followed by assessment of candidates using a Gaussian Process (GP)
#' model based on previous calculations of the objective function (to increase
#' estimation and evaluation speed of the GP, the fast local approximation of
#' laGP package is used). Although  it is assumed that the optimization problem
#' is unconstrained, the objective function can return Inf or NA (such points
#' are not used in the GP model). The behavior of the function is controlled
#' by the variable control, which is a list with named variables. conrol$Nmodel
#' specifies the number of preceding observations to use for the GP model. 
#' control$Ncand specifies the number of candidates to check, where candidates
#' are randomly chosen with a normal draw for which the mean equals the current
#' best parameter vector and the standard deviations are specified by
#' control$scaleVect. For rerpoducibility, the control$seed can be used to set
#' the random number seed. The candidate with the lowest estimated value of the
#' objective function is chosen for the next evaluation of the objective
#' function. The problem can be initialized in one of two ways: by specifying
#' an initial parameter vector, param0, or from a save file storing the results
#' of a previous "leg" of the optimization. If an initial parameter vector is
#' used for initialization, the objective function must be specified using the
#' optional input objFun. If a save file is used, the objective function does
#' not need to be input (if it is, it is ignored and a warning is given). The
#' optimization terminates either after the maximum number of evaulations,
#' control$maxEval, have been made, or the objective function has failed to
#' improve more than the objective function tolerance, control$objTol, over the
#' last 4*control$Nmodel evaluations. The boolean variable control$catchErrors
#' allows catching errors in the call to objFun using try, and setting the
#' ojective function value to Inf (infinity) if an error is encountered.
#' control$verbose specifies whether or not to print out summary information as
#' the optimization proceeds. control$showPlot specifies whether to plot the
#' progress of the optimization algorithm as it proceeds. The optional input
#' saveFile specifies whether to save the state of the optimization after each
#' function evaluation (default NA, not used). The control varibles and their
#' default values are:
#' ****************************************************************************
#' scaleVect   Scaling vector of standard deviations for drawing new candidate
#'             parameter vectors
#' Default:    rep(1e-4,length(param0)
#' ****************************************************************************
#' Nmodel      Number of observations to use to build the Gaussian Process model
#'   Default:    100
#' ****************************************************************************
#' Ncand       Number of randomly selected candidates to check with the Gaussian
#'             Process model 
#'   Default:    100
#' *****************************************************************************
#' maxEval     Maximum number of evaluations of the objective function
#'   Default:    10000
#' *****************************************************************************
#' objTol      Tolerance to use in the stopping condition. If NA is input for
#'             objTol, no check is made.
#'   Default:    1e-4
#' *****************************************************************************
#' catchErrors Whether or not to catch errors in the objective function
#'             evaluation, and set the value to Inf (infinity) if an error
#'             is encountered
#'   Default:    FALSE
#' *****************************************************************************
#' seed        The random number seed to use. If it is not input, a random
#'             integer on the interval 1 to 1000000 is used (seeds from previous
#'             are never reused)
#'   Default:    sample.int(1000000,1)
#' *****************************************************************************
#' verbose     Whether or not to print out progress of the optimization
#'   Default:    FALSE
#' *****************************************************************************
#' showPlot    Whether or not to show a plot of the objective function value as
#'             the optimization proceeds
#'   Default:    FALSE
#' *****************************************************************************
#' The named variables in the output list are:
#' *****************************************************************************
#' paramMat    A matrix of parameter values for the last control$Nmodel
#'             evaluations that did not evaluate to infinity (or as many are
#'             available if fewer than control$Nmodel)
#' *****************************************************************************
#' objVect     A vector of objective function values for the last control$Nmodel
#'             evaluations that did not evaluate to infinity (or as many are
#'             available if fewer than control$Nmodel)
#' *****************************************************************************
#' objVectFull A vector of all the objective function values. If an evaluation
#'             is infinite, NA is stored.
#' *****************************************************************************
#' param_best  The parameter vector of the best solution found so far
#' *****************************************************************************
#' f_best      The objective function value of the best solution found so far
#' *****************************************************************************
#' legData     Information specific to each leg (call) to the doGpBasedOptim.
#'             legData is a list with a length equal to the number of legs. For
#'             all legs its elements  include control0, the input control
#'             variable, control, the full list of used control variables, and
#'             saveFile, the save file to use (NA if none is used). In addition,
#'             for the first leg the variables param0, the initial parameter
#'             vector, f0, the value of the objective function at param0, and
#'             objFun, the bjective function, are also stored.
#' *****************************************************************************
#' @param init The initialization object (either an initial parameter vector or a save file from a previous leg of the optimization)
#' @param objFun The objective function (ignored if init is a save file)
#' @param control A named list of control variables (see function description for details)
#' @param saveFile An optional save file to store intermediate results of the optimization (same variables as output and the default is NA, no save file used)
#' @param ... Additional inputs to the objective function
#' @return A list of named variables containing key information about the optimization (see details)
#' @export
doGpBasedOptim <- function(init,objFun=NA,control=list(),saveFile=NA,...) {
  # Should results be saved to file?
  useSaveFile <- !is.na(saveFile)

  # A local helper function to set control values (or use default)
  set_or_use_default <- function(setting,control,default) {
    if(setting %in% names(control)) {
      # The setting is specified
      return(control[[setting]])
    }
    # The setting is not specified
    return(default)
  }

  # Which leg is this?
  if(is.numeric(init)) {
    # This is the first leg. init is the initial parameter vector, param0. The
    # control values are set either to the default or the input value
    legNum <- 1

    # objFun must be specified for the first leg
    if(class(objFun) != 'function') {
      stop('A function must be specified for objFun if param0 is specified')
    }

    # Ensure that all named parameters in the control list are valid
    for(setting in names(control)) {
      if( !(setting %in% c('scaleVect','Nmodel','Ncand','maxEval','objTol','catchErrors','seed','verbose','showPlot')) ) {
        stop(paste0('Unrecognized control parameter, ',setting))
      }
    }

    # Number of parameters
    Np <- length(init)

    # Set control values (first saving the input control list as control0)
    control0 <- control
    control <- list()
    control$scaleVect   <- set_or_use_default(  'scaleVect',control0,         rep(1e-4,Np))
    control$Nmodel      <- set_or_use_default(     'Nmodel',control0,                  100)
    control$Ncand       <- set_or_use_default(      'Ncand',control0,                  100)
    control$maxEval     <- set_or_use_default(    'maxEval',control0,                10000)
    control$objTol      <- set_or_use_default(     'objTol',control0,                 1e-4)
    control$catchErrors <- set_or_use_default('catchErrors',control0,                    F)
    control$seed        <- set_or_use_default(       'seed',control0,sample.int(1000000,1))
    control$verbose     <- set_or_use_default(    'verbose',control0,                    F)
    control$showPlot    <- set_or_use_default(   'showPlot',control0,                    F)

  } else {
    # This is not the first leg. init is a save file.
    saveData <- readRDS(init)
    legNum <- length(saveData$legData) + 1

    # objFun must be specified for the first leg
    if(!is.na(objFun)) {
      warning('objFun was specified, but the saved value will be used')
    }
    objFun <- saveData$legData[[1]]$objFun

    # Ensure that all named parameters in the control list are valid
    for(setting in names(control)) {
      if( !(setting %in% c('scaleVect','Nmodel','Ncand','maxEval','objTol','catchErrors','seed','verbose','showPlot')) ) {
        stop(paste0('Unrecognized control parameter, ',setting))
      }
    }

    # Number of parameters
    Np <- length(saveData$legData[[1]]$param0)

    # Set control values (first saving the input control list as control0)
    control0 <- control
    # last_control, the last set of full control values used, is treated as the
    # default for continuing runs
    last_control <- saveData$legData[[legNum-1]]$control
    control <- list()
    control$scaleVect   <- set_or_use_default(  'scaleVect',control0,last_control$scaleVect)
    control$Nmodel      <- set_or_use_default(     'Nmodel',control0,last_control$Nmodel)
    control$Ncand       <- set_or_use_default(      'Ncand',control0,last_control$Ncand)
    control$maxEval     <- set_or_use_default(    'maxEval',control0,last_control$maxEval)
    control$objTol      <- set_or_use_default(     'objTol',control0,last_control$objTol)
    control$catchErrors <- set_or_use_default('catchErrors',control0,last_control$catchErrors)
    control$seed        <- set_or_use_default(       'seed',control0,sample.int(1000000,1)) # redraw on continuing runs unless the seed is input
    control$verbose     <- set_or_use_default(    'verbose',control0,last_control$verbose)
    control$showPlot    <- set_or_use_default(   'showPlot',control0,last_control$showPlot)

  }

  # Make sure scaleVect has length Np
  if(length(control$scaleVect) != Np) {
    stop(paste0('scaleVect should have length equal to the parameter vector, ',Np,', but has length ',length(control$scaleVect)))
  }

  # Make sure scaleVect is all positive
  if(any(control$scaleVect <= 0)) {
    stop('scaleVect is not all positive')
  }

  # Set the random number seed
  set.seed(control$seed)

  # If this is the first leg, evaluate the objective function at the initial
  # parameter vector. This evaluation does not count towards maxEval. If this
  # is not the first leg, initialize variables from file.
  if(legNum == 1) {
    t0 <- Sys.time()
    if(!control$catchErrors) {
      f0 <- objFun(init,...)
    } else {
      f0 <- try(objFun(init,...))
      if(class(f0) == 'try-error') {
        f0 <- Inf
      }
    }
    t1 <- Sys.time()

    if(!is.finite(f0)) {
      stop('Objective function is not finite at initial parameter vector (param0)')
    }

    if(control$verbose) {
      print('--')
      print(paste0('Evaluation at initial parameter vector took ', as.numeric(t1-t0,units='secs'),' seconds'))
      print('Value of objective function at initial parameter vector:')
      print(f0)
    }

    # Initialize variables
    param_best <- init
    f_best <- f0
    paramMat <- matrix(init,nrow=1)
    objVect <- matrix(f0,nrow=1)
    objVectFull <- matrix(f0,nrow=1)
    s0 <- 1 # For the first leg, the first "sample" is 1

    # Store inputs and other information in legData
    legData <- list()
    legData[[1]] <- list(param0=init,f0=f0,objFun=objFun,control0=control0,control=control,saveFile=saveFile)
   
  } else {
    f0 <- saveData$legData[[1]]$f0
    if(control$verbose) {
      print('--')
      print('This is not the first leg. Loading parameters from file')
      print('Value of objective function at initial parameter vector:')
      print(f0)
    }

    # Initialize variables
    param_best <- saveData$param_best
    f_best <- saveData$f_best
    paramMat <- saveData$paramMat
    objVect <- saveData$objVect
    objVectFull <- saveData$objVectFull
    s0 <- length(objVectFull) + 1

    # Store inputs and other information in legData
    legData <- saveData$legData
    legData[[legNum]] <- list(control0=control0,control=control,saveFile=saveFile)
  }

  # Begin main loop
  for(s in s0:(s0+control$maxEval-1)) {
    if(s <= 2) {
      # For the first two samples, make a single random draw for the parameter
      # vector to add
      param_to_add <- param_best + rnorm(Np)*control$scaleVect
    } else {
      # After the first two draws, use a Gaussian Process to choose the
      # parameter vector to add

      # Initialize a matrix to store candidate vectors to add, then populate it
      # with random samples
      paramMatToCheck <- matrix(NA,nrow=control$Ncand,ncol=Np)
      for(sc in 1:control$Ncand) {
        paramMatToCheck[sc,] <- param_best + rnorm(Np)*control$scaleVect
      }

      # Create a Gaussian Process model to approximate the objective function,
      # subtracting the initial value of the objective function (f0). This is
      # based on paramMat and objVect, which are limited to Nmodel preceding 
      # observations (or all observations if fewer than Nmodel)
      gp <- laGP::newGP(paramMat, objVect-f0, 2, 1e-6, dK = TRUE)

      
      laGP::mleGP(gp, tmax=20)
      p <- laGP::predGP(gp, paramMatToCheck)
      laGP::deleteGP(gp)
      param_to_add <- paramMatToCheck[which.min(p$mean),]
    }

    t0 <- Sys.time()
    if(!control$catchErrors) {
      f_prop <- objFun(param_to_add,...)
    } else {
      f_prop <- try(objFun(param_to_add,...))
      if(class(f_prop) == 'try-error') {
        f_prop <- Inf
      }
    }
    t1 <- Sys.time()
    if(control$verbose) {
      print('--')
      print(paste0('Evaluation #',s,' took ', as.numeric(t1-t0,units='secs'),' seconds'))
      print('Value of objective function at proposed parameter vector:')
      print(f_prop)
    }

    if(is.finite(f_prop)) {
      # Only store Nmodel previous parameter vectors (except for objVectFull)
      if(nrow(paramMat) == control$Nmodel) {
        paramMat <- rbind(paramMat[2:control$Nmodel,],param_to_add)
        objVect  <- rbind( matrix(objVect[2:control$Nmodel]) ,f_prop) # in R, subsetting a column matrix does not yield a column matrix
        objVectFull <- rbind(objVectFull,f_prop)
      } else {
        paramMat <- rbind(paramMat,param_to_add)
        objVect  <- rbind( objVect,f_prop)
        objVectFull <- rbind(objVectFull,f_prop)
      }

      if(f_prop < f_best) {
        param_best <- param_to_add
        f_best <- f_prop
      }
    } else {
      objVectFull <- rbind(objVectFull,NA)
    }


    if(control$verbose) {
      print('Improvement in objective function per evaluation:')
      print((f0-f_best)/length(objVectFull))
    }

    if(control$showPlot) {
      plot(objVectFull + f0)
    }

    if(useSaveFile) {
      saveRDS(list(paramMat=paramMat,objVect=objVect,objVectFull=objVectFull,param_best=param_best,f_best=f_best,legData=legData),saveFile)
    }

    if(control$showPlot) {
      plot(1:length(objVectFull),objVectFull,xlab='Evaluation Number',ylab='Objective Function Value')
    }

    # If necessary, check the stopping condition
    if(!is.na(control$objTol)) {
      if(nrow(paramMat) >= 4*control$Nmodel + 1) {
        ind <- (nrow(paramMat) - 4*control$Nmodel + 1):nrow(paramMat) # indices of the preceding 4*Nmodel evaluations
        f_stopping <- min(objVect[-ind]) # The best objective function value before the indices to be checked
        improvement <- f_stopping - objVect[ind] # The improvement of the indices to be checked; positive if things got better
        if(!any(improvement > control$objTol)) {
          break
        }
      }
    }
  }

  return(list(paramMat=paramMat,objVect=objVect,objVectFull=objVectFull,param_best=param_best,f_best=f_best,legData=legData))
}

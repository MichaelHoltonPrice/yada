#' @title Visualize a continuous fit
#'
#' @description
#' Visualize a continuous fit by plotting the observed points (x,w) and the fit.
#'
#' @param x The vector of x values for the variable
#' @param w The vector of response values for the variable
#' @param th_w The fit parameter vector
#' @param mod_spec The model specification
#' @param xplot (optional) A vector at which to calculate the fit for the plot.
#'   If not provided, a sequence of equally spaced values of length hundred
#'   from min(x) to max(x) is used.
#' @param th_w0 (optional) A baseline fit to add to the plot (likely the known
#'   value of the parameter vector from a simulation)
#' @param show_unc (default: FALSE) Whether to plot the uncertainty for the fit
#' @param ... Additional arguments to pass to the plot call for the x-w scatter
#'   plot (e.g., to set the x- and y-labels).
#'
#' @export
vis_cont_fit <- function(x,w,th_w,mod_spec,xplot=c(),th_w0=c(),show_unc=F,...) {

  if(length(xplot) == 0) {
    xplot <- seq(min(x),max(x),len=100)
  }

  h <- calc_mean_univariate_cont(xplot,th_w,mod_spec)

  if(length(th_w0) > 0) {
    h0 <- calc_mean_univariate_cont(xplot,th_w0,mod_spec)
    w_range <- range(c(w,h,h0), finite=TRUE)
  } else {
    w_range <- range(c(w,h), finite=TRUE)
  }

  if(show_unc) {
    sig <- calc_noise_univariate_cont(xplot,th_w,mod_spec)
    lo <- h - sig
    hi <- h + sig
    w_range <- range(c(w_range,lo,hi))
  }

  xrange <- range(c(x,xplot))
  plot(x,w,xlim=xrange,ylim=w_range,...)
  lines(xplot,h,col='red',lwd=2)
  if(show_unc) {
    polygon(c(xplot, rev(xplot)),
            c(lo, rev(hi)),
            border = NA,
            xlab = NULL,
            col = adjustcolor("red", alpha.f = 0.5))
  }
  if(!all(is.na(th_w0))) {
    lines(xplot,h0,col='grey',lty=2,lwd=2)
  }
}

#' @title Visualize an ordinal fit
#'
#' @description
#' Visualize an ordinal fit by binning based on the x-values, calculating the
#' probability of each m-value for each bin using the v-values, and plotting
#' the fits.
#'
#' @param x The vector of x values for the variable
#' @param v The vector of response values for the variable
#' @param th_v The fit parameter vector
#' @param mod_spec The model specification
#' @param xplot (optional) A vector at which to calculate the fit for the plot.
#'   If not provided, a sequence of equally spaced values of length hundred
#'   from min(x) to max(x) is used.
#' @param th_v0 (optional) A baseline fit to add to the plot (likely the known
#'   value of the parameter vector from a simulation)
#' @param ... Additional arguments to pass to the plot call for the x-w scatter
#'   plot (e.g., to set the x- and y-labels).
#'
#' @export
vis_ord_fit <- function(x,
                        v,
                        th_v,
                        mod_spec,
                        bin_bounds=c(),
                        xplot=c(),
                        th_v0=c(),
                        ...) {

  if (length(bin_bounds) == 0) {
    bin_bounds <- seq(min(x),max(x),len=20)
  }
  num_bins <- length(bin_bounds) - 1

  num_cat <- length(unique(na.omit(v))) # number of categories
  bin_counts <- matrix(NA,num_bins,num_cat)
  bin_centers <- (bin_bounds[1:num_bins] + bin_bounds[2:(num_bins+1)])/2
  if ( length(xplot) == 0) {
    xplot <- bin_bounds
  }

  for(b in 1:num_bins) {
    if(b < num_bins) {
      ind_b <- bin_bounds[b] <= x & x <  bin_bounds[b+1]
    } else {
      ind_b <- bin_bounds[b] <= x & x <= bin_bounds[b+1]
    }
    #xb <- x[ind_b]
    vb <- v[ind_b]
    for(m in 0:(num_cat-1)) {
      bin_counts[b,m+1] <- sum(vb == m)
    }
  }

  bin_prop <- matrix(NA,num_bins,num_cat) # bin proportions
  for(b in 1:num_bins) {
    bin_prop[b,] <- bin_counts[b,] / sum(bin_counts[b,])
  }

  # TODO: consider how to handle plot margins for different values of M
  # mar is bottom, left, top, right
  # TODO: Improve the graph (e.g., only have labels and tick-marks on one axis)
  par(mfrow=c(num_cat,1),mar=c(2, 1, 1, 1))
  for(m in 0:(num_cat-1)) {
    qm <- calc_q(xplot,th_v,m,mod_spec)
    if (length(th_v0 != 0)) {
      qm0 <- calc_q(xplot,th_v0,m,hetero)
      #yrange <- range(c(qm,qm0,bin_prop[,m+1]))
    } else {
      #yrange <- range(c(qm,bin_prop[,m+1]))
    }
    #plot(bin_centers,bin_prop[,m+1],lim=yrange,xlab='Age [years]', ylab='Probability')
    plot(bin_centers,bin_prop[,m+1],ylim=c(0,1),...)
    lines(xplot,qm,col='red')
    if (length(th_v0) != 0) {
      lines(xplot,qm0,col='grey',lty=2)
    }
  }

  return(list(bin_centers=bin_centers,bin_counts=bin_counts,bin_prop=bin_prop))
}

#' @title
#' Plot the posterior density using the result of a call to analyze_x_posterior
#'
#' @description
#' x_post_obj is the result of a call to analyze_x_posterior. Plot the density,
#' including marking the mean and 95% confidence intervals. If the known age was
#' provided in the call to analyze_x_posterior, add it to the plot.
#'
#' @param analysis The result of a call to analyze_x_posterior
#' @param ... Additional parameters for the line plot (e.g., the x- and
#'   y-labels)
#'
#' @export
plot_x_posterior <- function(analysis,...) {
  yrange <- range(analysis$density,analysis$flo,analysis$fhi)
  if ("xknown" %in% names(analysis)) {
    yrange <- range(yrange,analysis$fknown)
  }

  # TODO: consider only supporting specific plot options
  plot(analysis$x,analysis$density,type="l",...)
  lines(c(1,1)*analysis$xmean,c(0,analysis$fmean),col="black",lwd=2)
  # TODO: consider marking the bounds with a grey region
  lines(c(1,1)*analysis$xlo,c(0,analysis$flo),col="grey",lwd=2)
  lines(c(1,1)*analysis$xhi,c(0,analysis$fhi),col="grey",lwd=2)

  if ("xknown" %in% names(analysis)) {
    lines(c(1,1)*analysis$xknown,c(0,analysis$fknown),col="green",lwd=2)
  }
}

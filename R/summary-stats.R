###=========================================================================#
### SUMMARY STATS ==========================================================#


###=========================================================================#
###== FUNCTIONS ============================================================#
###-- summary_stats ............. calculate summary statistics
###---| mean_ci ................. calculate mean and 95% confidence interval
###-- hpd ....................... highest posterior density interval
###-- sem ....................... calculate standard error of the mean
###-- cv ........................ calculate coefficient of variation

##--------------------------------------------------------------------------#
## Calculate summary statistics --------------------------------------------#
summary_stats <-
function(x, hpd = FALSE, na.rm = FALSE) {
  ## throw warning if NA's present
  if (!na.rm && any(is.na(x))) {
    warning(paste("'x' contains", sum(is.na(x)), "NA's"))
  }

  ## calculate quantiles
  if (hpd) {
    q <- c(hpd(x, 0.05), hpd(x, 0.10))[c(1, 3, 4, 2)]

  } else {
    q <- quantile(x, c(.025, .05, .95, .975), na.rm = na.rm)
  }

  ## combine stats
  stats <-
    c(mean = mean(x, na.rm = na.rm),
      median = median(x, na.rm = na.rm),
      q)
  names(stats) <-
    c("mean", "median", "2.5%", "5%", "95%", "97.5%")
  return(stats)
}

##--------------------------------------------------------------------------#
## Calculate mean and 95% confidence interval ------------------------------#
mean_ci <-
function(x, ...) {
  c(mean = mean(x, ...), quantile(x, probs = c(0.025, 0.975), ...))
}


##--------------------------------------------------------------------------#
## Highest posterior density interval --------------------------------------#

## note: idem 'boa.hpd()' in 'boa' package
hpd <-
function(x, alpha = 0.05) {
  n <- length(x)
  m <- max(1, ceiling(alpha * n))
  y <- sort(x)
  a <- y[seq(m)]
  b <- y[(n - m + 1):n]
  i <- order(b - a)[1]
  return(c(a[i], b[i]))
}


##--------------------------------------------------------------------------#
## Calculate standard error of the mean ------------------------------------#
sem <-
function(x, ...) {
  sem <- sd(x, ...) / sqrt(length(x))
  return(sem)
}


##--------------------------------------------------------------------------#
## Calculate coefficient of variation --------------------------------------#
cv <-
function(x, ...) {
  cv <- sd(x, ...) / mean(x, ...)
  return(cv)
}
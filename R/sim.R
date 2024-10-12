###=========================================================================#
### SIMULATION FUNCTIONS ===================================================#
###=========================================================================#
###-- rgamma2 ..................... random Gamma deviates ~ mean and SD
###-- rlnorm2 ..................... random Log-normal deviates ~ mean and SD


##--------------------------------------------------------------------------#
## Random Gamma deviates with given mean and SD ----------------------------#
rgamma2 <-
function(n, mean, sd) {
  shape <- mean^2 / sd^2
  rate <- mean / sd^2
  rgamma(n, shape, rate)
}


##--------------------------------------------------------------------------#
## Random Log-normal deviates with given mean and SD -----------------------#
rlnorm2 <-
function(n, mean, sd) {
  var <- sd^2
  varlog <- log((var + mean^2) / mean^2)
  sdlog <- sqrt(varlog)
  meanlog <- log(mean) - 0.5 * varlog
  rlnorm(n, meanlog, sdlog)
}

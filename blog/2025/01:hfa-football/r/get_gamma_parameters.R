#get_gamma_parameters.R
gamma_parameters <- function(mean, variance) {
  # Calculate the shape parameter
  shape <- mean^2 / variance
  # Calculate the rate parameter
  rate <- mean / variance
  # Return the shape and rate parameters
  return(tibble(shape, rate))
}
# Example
# mean <- 2
# variance <- 1

# Extract the parameters
# shape, rate <- gamma_parameters(mean, variance)
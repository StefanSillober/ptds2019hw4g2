#'@title Estimate Pi with randomly generated numbers

#'@description compute Pi with randomly generated points

#'@param B A \code{numeric} (integer) used to denote the number of simulations
#'@param seed \code{numeric} used to denote the seed used in the simulaton
#'@return A \code{list} containing the following attributes:
#' \describe{
#'    \item{estimated_pi}{Estimated value for Pi}
#'    \item{points}{Points from the simulation and if the lie inside the unit circle}
#'}



#'@examples
#'estimate_pi(B = 5000, seed = 10)
#'#'estimate_pi(B = 5000*2, seed = 220)
estimate_pi <- function(B = 5000, seed = 10) {
  stopifnot(is.numeric(B),is.numeric(seed))
  if (!is.integer(B)){
    B <- as.integer(B)
  }
  # set a seed
  set.seed(seed)
  
  # simulate B points
  points <- data.frame(
    x = runif(n = B, min = -1, max = 1),
    y = runif(n = B, min = -1, max = 1),
    inside = rep(NA, B)
  )
  
  # your loop goes here
  circ = (points$x)^2 + (points$y)^2
  points$inside <- ifelse(circ <= 1, T, F)
  estimated_pi <- 4 * (sum(points$inside)/B)
  
  # create a new list
  rval <- list(
    estimated_pi = estimated_pi,
    points = points
  )
  
  # assign pi class to rval
  class(rval) <- "pi"
  
  # return rval
  return(rval)
  
}


#'@param x A \code{dataframe} containing x and y coordinates and a Boolian column if the points are inside the unit circle
#'@return A \code{plot} Plotting the simulated points

#'@examples
#'example <- estimate_pi(B = 5000, seed = 10)
#'plot.pi(example)
#'plot.pi(estimate_pi()[2])
plot.pi <- function(x) {
  points <- x[["points"]]
  
  # plot points
  plot(points$x[which(points$inside == T)],points$y[which(points$inside == T)],xlab="X",ylab="Y",main="Monte Carlo Pi simulation", col = 'cadetblue3')
  points(points$x[which(points$inside == F)],points$y[which(points$inside == F)],col='coral2')
}


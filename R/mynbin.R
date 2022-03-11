#' Creates a negative binomial probability distribution for variable y
#'
#' @param y number of trials until the rth success is observe
#' @param r number of successes
#' @param p probability of success on a single Bernoulli trial
#'
#' @return probability of y in the negative binomial distribution
#'
#' @examples
#' mynbin(10,3,0.4)
#' mynbin(21, 2, .1)
#'
#'@export
mynbin=function(y,r,p){
  choose(y-1,r-1)*p^r*(1-p)^(y-r)
}

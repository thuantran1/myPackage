#' Creates a piecewise function that is centered around xk
#'
#' @param x vector of x-values
#' @param coef array of 3 coefficients for the function {B0, B1, z}
#' @param xk x value where the the line segments meet
#'
#' @return y value for the function y = B0 + B1x + z(x-xk)(x>xk)
#'
#' @examples
#' piecewise(2, c(1.23, .234, -.24), 18)
#' piecewise(3, c(1, 2, 3), 10)
#'
#'@export
piecewise = function(x,coef,xk){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x>xk)
}

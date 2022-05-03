#' Creates a distribution of the sum of samples of a uniform distribution
#'
#' @param n size of each uniform distribution
#' @param iter amount of iterations done for each sampling
#' @param a lower bound for the uniform distribution
#' @param b upper bound for the uniform distribution
#'
#' @return distribution of the sum of uniforms
#'
#' @examples
#' myclt(n=10,iter=10000)
#' myclt(n=2,iter=8000)
#'
#'@export
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b) #A
  data=matrix(y,nr=n,nc=iter,byrow=TRUE) #B
  sm=apply(data,2,sum) #C
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}

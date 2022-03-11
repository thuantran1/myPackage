#' Creates a normal distribution curve with x <= alpha
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param alpha point where probability is calculated, x <= alpha
#'
#' @return shaded normal distribution curve with the probability
#'
#' @examples
#' myncurve(0,1,2)
#' myncurve(5,.5,4)
#'
#'@export
myncurve = function(mu, sigma, alpha){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(-1000,alpha,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)

  polygon(c(-1000,xcurve,alpha),c(0,ycurve,0),col="Red")

  prob=pnorm(alpha,mean=mu,sd=sigma)
  prob=round(prob,4)

  text(x=mu,y=.5*dnorm(mu,mu,sigma),paste0("Area = ", prob))
}

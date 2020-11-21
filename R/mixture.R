#' A Mixture Norm Function
#'
#' Generate n observations from a mixture of two normal distributions, i.e., generate U from a Bernoulli(p) distribution and if U=1, then draw Y from a normal distribution with mean μ1 and standard deviation σ1, or if U=0, draw Y from the other normal distribution with mean μ2 and standard deviation σ2.
#' In this context, there is a probability (p) that Y is drawn from a normal distribution N(μ1,σ1), and there is a probability 1−p that Y is drawn from the other normal distribution N(μ2,σ2). Note that the density of Y is the sum of weighted density of two normal distributions.
#' @param n Number of Observations
#' @param p Probability (p) that Y is drawn from a normal distribution N(μ1,σ1)
#' @param mu1 Mean of first Normal Distribution
#' @param mu2 Mean of Second Normal Distribution
#' @param sigma1 Standard Deviation of first Normal Distribution
#' @param sigma2 Standard Deviation of Second Normal Distribution
#' @export
#' @examples
#' mixture_norm()

mixture_norm=function(n,p,mu1,mu2,sigma1,sigma2){
  u=rbernoulli(n,p)
  y=matrix(0,nrow = n,ncol = 1)
  y=as.vector(y)
  for(i in 1:n){
    if(u[i]==T){
      y[i]=rnorm(1,mu1,sigma1)
    }else{
      y[i]=rnorm(1,mu2,sigma2)
    }
  }
  return(y)
}

#' Sign test for given median
#' 
#' @param med0 null median
#' @param x vector of data for test
#' @return list of two elements: table of values above and below null median, data frame of 1-sided and 2-sided P-values
#' 
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' @examples 
#' sign_test(3.5,1:10)
#' sign_test(3,1:10)
#' sign_test(25,mtcars$mpg)
#' 
#' @export
#' 
sign_test <-
function(med0,x) {
  # get rid of x's too close to med0
  tol=1e-6
  y=x-med0
  y=y[abs(y)>tol] # take only values of y more than tol away from 0
  y # +ve = above null median, -ve = below
  tab=c(sum(y<0),sum(y>0))
  names(tab)=c("below","above")
  n=sum(tab)
  stat=tab[1]
  p_upper=sum(dbinom(0:stat,n,0.5))
  p_lower=sum(dbinom(stat:n,n,0.5))
  p_two=2*min(p_lower,p_upper)
  nn=c("lower","upper","two-sided")
  d=data.frame(alternative=nn,p_value=c(p_lower,p_upper,p_two))
  list(above_below=tab,p_values=d)
}

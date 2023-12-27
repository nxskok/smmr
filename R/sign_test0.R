#' Sign test (simplified) for given median
#' 
#' @param x vector of data
#' @param med0 null median (defaults to zero)
#' @param tol (default 1e-6) how close a data value has to be to the null median to be considered equal to null median (and discarded)
#' @return list of two elements: table of values above and below null median, data frame of 1-sided and 2-sided P-values
#' 
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' @examples 
#' sign_test0(1:10,3.5)
#' sign_test0(1:10,3)
#' sign_test0(datasets::mtcars$mpg,25)
#' 
#' @export
#' 
sign_test0=function(x,med0=0,tol=1e-6) {
  d=data.frame(x=x)
  d %>% mutate(y=x-med0) %>% 
    filter(abs(y)>tol) %>% count(y>0) -> tab
  freqs_0=tab$n
  v = x
  if (length(freqs_0)==1) {
    if(v[1]>med0) {
      freqs=c(0,freqs_0)
    } else {
      freqs=c(freqs_0,0)
    }
  } else {
    freqs=freqs_0
  }
  names(freqs)=c("below","above")
  stat=freqs[1]
  n=sum(freqs)
  p_upper=sum(dbinom(0:stat,n,0.5))
  p_lower=sum(dbinom(stat:n,n,0.5))
  p_two=2*min(p_lower,p_upper)
  nn=c("lower","upper","two-sided")
  out=data.frame(alternative=nn,p_value=c(p_lower,p_upper,p_two))
  list(above_below=freqs,p_values=out)
}

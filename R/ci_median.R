#' Confidence bnterval for median by inverting sign test
#' 
#' @param x vector of data 
#' @param conf.level level for CI (as decimal), default 95%
#' @return lower and upper limits 
#'  
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' @examples 
#'
#' @importFrom purrr map_dbl
#' @export
#' 
ci_median=function(x,conf.level=0.95) {
  r=range(x)
  y=seq(from=r[1]-1,to=r[2]+1,length.out = 3*length(x)) # "denser" than x
  pv=map_dbl(y,pval_sign,x)
  d=data.frame(y,pv)
  lims=d %>% filter(pv>=1-conf.level) %>% slice(c(1,n()))
  thelims=c(lims[1,1],lims[2,1])
  thelims
}
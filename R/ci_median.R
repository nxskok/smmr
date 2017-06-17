#' Confidence interval for median by inverting sign test
#' 
#' @param x vector of data 
#' @param conf.level level for CI (as decimal), default 95 perent
#' @return lower and upper limits
#' 
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' 
#' @examples 
#' ci_median(mtcars$mpg,0.90)
#' ci_median(1:20)
#' ci_median(1:5)
#' 
#' @importFrom purrr map_dbl
#' 
#'
#' @export
#' 
ci_median=function(x,conf.level=0.95) {
  r=range(x)
  y=seq(from=r[1]-1,to=r[2]+1,length.out = 3*length(x)) # "denser" than x
  pv=purrr::map_dbl(y,pval_sign,x)
  inside=(pv>=1-conf.level)
  first=min(which(inside))
  last=max(which(inside))
  lims=c(y[first],y[last])
  d=data.frame(y,pv)
  list(d,lims)
}

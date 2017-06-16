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
#' @importFrom dplyr filter
#' @importFrom dplyr slice
#' @export
#' 
ci_median=function(x,conf.level=0.95) {
  r=range(x)
  y=seq(from=r[1]-1,to=r[2]+1,length.out = 3*length(x)) # "denser" than x
  pv=purrr::map_dbl(y,pval_sign,x)
  d=data.frame(y,pv)
  tmp1=dplyr::filter(d,pv>=1-conf.level) 
  lims=dplyr::slice(tmp1,c(1,dplyr::n()))
  thelims=c(lims[1,1],lims[2,1])
  thelims
}
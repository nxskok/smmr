#' Confidence interval for median by inverting sign test
#' 
#' @param x vector of data 
#' @param conf.level level for CI (as decimal), default 95 percent
#' @return lower and upper limits
#' 
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' 
#' @examples 
#' ci_median0(mtcars$mpg)
#' ci_median0(mtcars$mpg,0.80)
#' ci_median0(1:20)
#' ci_median0(1:5)
#' 
#' @importFrom purrr map_dbl
#' 
#'
#' @export
#' 
ci_median0=function(x,conf.level=0.95) {
  r=range(x)
  xx=sort(unique(x))
  y1=c(r[1]-1,xx)
  y2=c(xx,r[2]+1)
  betweens=(y1+y2)/2
  y=sort(c(xx,betweens))
  d=data.frame(xname=x)
  pv=purrr::map_dbl(y,pval_sign,d,xname)
  inside=(pv>=1-conf.level)
  first=min(which(inside))
  last=max(which(inside))
  c(y[first],y[last])
}

#' Confidence interval for median by inverting sign test
#' 
#' @param d a data frame
#' @param x unquoted name of column of data
#' @param conf.level level for CI (as decimal), default 95 percent
#' @return lower and upper limits
#' 
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' 
#' @examples 
#' ci_median(mtcars,mpg)
#' ci_median(mtcars,mpg,0.80)
#' d=data.frame(z=1:20)
#' d1=data.frame(z=1:5)
#' ci_median(d,z)
#' ci_median(d1,z)
#' 
#' @importFrom purrr map_dbl
#' 
#'
#' @export
#' 
ci_median=function(d,x,conf.level=0.95) {
  x1=enquo(x)
  exes= d %>% pull(!!x1)
  r=range(exes)
  xx=sort(unique(exes))
  y1=c(r[1]-1,xx)
  y2=c(xx,r[2]+1)
  betweens=(y1+y2)/2
  y=sort(c(xx,betweens))
  pv=purrr::map_dbl(y,pval_sign,d,!!x1)
  inside=(pv>=1-conf.level)
  first=min(which(inside))
  last=max(which(inside))
  c(y[first],y[last])
}


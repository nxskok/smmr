#' Two-sided P-value for sign test
#' 
#' @param med0 null median
#' @param d data frame
#' @param x vector of data for test
#' @return P-value of two-sided sign test for median
#'  
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' @examples 
#' d=data.frame(z=1:10)
#' pval_sign(3.5,d,z)
#' pval_sign(3,d,z)
#' pval_sign(25,mtcars,mpg)
#' 
#' @export
#' 
pval_sign=function(med0,d,x) {
  x=enquo(x)
  ans=sign_test(d,!!x,med0)
  pvals = ans$p_values %>% pull(p_value)
  pvals[3]
}
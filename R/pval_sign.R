#' Two-sided P-value for sign test
#' 
#' @param med0 null median
#' @param x vector of data for test
#' @return P-value of two-sided sign test for median
#'  
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' @examples 
#' pval_sign(3.5,1:10)
#' pval_sign(3,1:10)
#' pval_sign(25,mtcars$mpg)
#' 
#' @export
#' 
pval_sign=function(med0,x) {
  ans=sign_test(med0,x)
  ans$p_values[3,2]
}
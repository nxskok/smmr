#' Mood's median test for comparison of group medians
#' 
#' @param x vector of data
#' @param g vector of group memberships (same length as x)
#' @return list of 2 objects: table, counts of values above and below the grand median in each group; value, test statistic, df and P-value
#' 
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' 
#' @examples
#' median_test(mtcars$mpg,mtcars$cyl)
#' 
#' @export
#' 
median_test <-
function(x,g) {
  m=median(x)
  tol=1e-6
  d=data.frame(x,g)
  d=d[abs(x-m)>tol,]
  tab=with(d,table(group=g,below=(x<m)))
  s=summary(tab)
  d=data.frame(what=c("statistic","df","P-value"),
               value=c(s$statistic,s$parameter,s$p.value))
  list(table=tab,test=d)
}

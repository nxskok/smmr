#' Mood's median test for comparison of group medians
#' 
#' @param x vector of data
#' @param g vector of group memberships (same length as x)
#' @return list of 3 objects: grand median of all obs, table, counts of values above and below the grand median in each group; value, test statistic, df and P-value
#' 
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' 
#' @examples
#' median_test0(mtcars$mpg,mtcars$cyl)
#' 
#' @importFrom stats dbinom
#' @importFrom stats median
#' @importFrom datasets mtcars
#' @export
#' 
median_test0 <-
  function(x,g) {
    m=median(x)
    tol=1e-6
    d=data.frame(x,g)
    d=d[abs(x-m)>tol,]
    tab=with(d,table(group=g,below=(x<m)))
    s=summary(tab,correct=F)
    d=data.frame(what=c("statistic","df","P-value"),
                 value=c(s$statistic,s$parameter,s$p.value))
    list(table=tab,test=d)
  }

#' Mood's median test for comparison of group medians
#' 
#' @param d a data frame
#' @param x unquoted name of quantitative variable
#' @param g unquoted name of grouping variable
#' @param tol (default 1e-6) any data values closer to overall median than this are discarded
#' @return list of 2 objects: table, counts of values above and below the grand median in each group; value, test statistic, df and P-value
#' 
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' 
#' @examples
#' median_test(mtcars,mpg,cyl)
#' d=data.frame(z=1:9,gg=c(1,1,1,1,1,2,2,2,2))
#' median_test(d,z,gg)
#' 
#' @importFrom stats dbinom
#' @importFrom stats median
#' @importFrom datasets mtcars
#' @importFrom rlang enquo
#' @export
#' 
median_test <-
  function(d,x,g,tol=1e-6) {
    x=enquo(x)
    g=enquo(g)
    grand_median= d %>% summarize(med=median(!!x)) %>% pull(med)
    tbl = d %>% mutate(y=(!!x)-grand_median) %>% 
      filter(abs(y)>tol) %>% 
      mutate(group=(!!g),above=ifelse(y<0,"below","above"))
    tab=with(tbl,table(group=group,above=above))
    s=summary(tab,correct=F)
    d=data.frame(what=c("statistic","df","P-value"),
                 value=c(s$statistic,s$parameter,s$p.value))
    list(grand_median = grand_median, table=tab, test=d)
  }

#' Pairwise Mood's median tests for all comparison of pairs of group medians
#' 
#' @param d a data frame
#' @param x unquoted name of quantitative variable
#' @param g unquoted name of grouping variable (can be a factor, is treated as text)
#' @param tol (default 1e-6) any data values closer to overall median than this are discarded
#' @return data frame of groups being compared and unadjusted and Bonferroni-adjusted P-values 
#' 
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' 
#' @examples
#' pairwise_median_test(datasets::mtcars,mpg,cyl)
#' 
#' @importFrom stats median
#' @importFrom rlang enquo
#' @export
#' 
pairwise_median_test <-
  function(d,x,g,tol=1e-6) {
    x=enquo(x)
    g=enquo(g)
    d %>% distinct(!!g) %>% 
      arrange(!!g) %>% 
      pull(!!g) ->
    distinct_groups
    crossing(g1=distinct_groups,g2=distinct_groups) %>% 
      filter(as.character(g1)<as.character(g2)) ->
    combos
    n=length(distinct_groups)
    n_pair=n*(n-1)/2
    combos %>% 
      mutate(p_value=map2_dbl(g1,g2,~median_test_pair(d,!!x,as.character(!!g),.x,.y))) %>% 
      mutate(adj_p_value=pmin(p_value*n_pair, 1))
    # 
    # grand_median= d %>% summarize(med=median(!!x)) %>% pull(med)
    # tbl = d %>% mutate(y=(!!x)-grand_median) %>% 
    #   filter(abs(y)>tol) %>% 
    #   mutate(group=(!!g),above=ifelse(y<0,"below","above"))
    # tab=with(tbl,table(group=group,above=above))
    # s=summary(tab,correct=F)
    # d=data.frame(what=c("statistic","df","P-value"),
    #              value=c(s$statistic,s$parameter,s$p.value))
    # list(table=tab,test=d)
  }
#' Mood's median tests for one pair of groups
#' 
#' @param d a data frame
#' @param x unquoted name of quantitative variable
#' @param g unquoted name of grouping variable (as character, not a factor)
#' @param g1 first group to compare (as text)
#' @param g2 second group to compare (as text)
#' @param tol (default 1e-6) any data values closer to overall median than this are discarded
#' @return (two-sided) P-value
#' 
#' @author Ken Butler, \email{butler@utsc.utoronto.ca}
#' 
#' @examples
#' median_test_pair(datasets::mtcars,mpg,cyl,4,8)
#' 
#' @importFrom stats median
#' @importFrom rlang enquo
#' @export
#' 
median_test_pair = function(d,x,g,g1,g2,tol=1e-6) {
  x=enquo(x)
  g=enquo(g)
  d %>% filter(!!g==g1 | !!g==g2) %>% 
    median_test(!!x, !!g) ->
  test_results
  test_results$test %>% pluck("value",3)
}

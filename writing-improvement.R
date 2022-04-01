#' Calculate writing improvement
#'
#' @param a the raw score on the policy brief
#' @param d the raw score on their barnes paper
#'
#' @return the "writing improvement" score
#'
#' @examples
#' 
#' foo(17.5,9)
#' 
foo <- function(a,d){
  
  b <- a / 19 - d / 10
  
  cc <- max(0,a / 19 + b)
  
  min(1,cc)
 
  # a = fraction of points achieved on the rubric criteria above (X/ 19)
  # b = difference between a and the fraction of points earned in paper 1
  # Paper improvement grade is :
  #   min(1, c)
  # where c= max(0, a+b)
  # 
   
}
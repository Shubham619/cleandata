
#' @title Cleaning first column (name)
#'
#' @description Cleaning first (name column)
#'
#' @param    'symbol
#'
#' @return NULL
#'
#' @examples clean_first_col(df)
#'




clean_first_col <- function(x){
  x[,1]<-sapply(x[,1],function(x) stringr::str_to_lower(x))
  pattern2 <- c(".*\\.\\s","\\s+","\\s+$")
  x[,1]<-sapply(x[,1],function(x) qdap::multigsub(pattern2,"",x))
  
}

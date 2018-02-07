
#' @title Cleaning data
#'
#' @description This package is used for data cleaning purpose
#'
#' @param    'symbol
#'
#' @return NULL
#'
#' @examples dataclean(df)
#'
#' @export  'clean data'


remove_timestamp <- function(x) {
  x <- x[,!names(x) %in% c("Timestamp")]
}

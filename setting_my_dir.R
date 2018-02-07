
#' @title setting my dir
#'
#' @description Used for setting dir purpose
#'
#' @param    'symbol
#'
#' @return NULL
#'
#' @examples setting_my_dir("path to file")
#'


setting_my_dir <- function (dir) {
  .Internal(setwd(dir))
}



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

#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

cleandata::

cleandata <- function(symbol) {

}

options(warn=-1)
options("getSymbols.warning4.0"=FALSE)

setting_my_dir <- function (dir) {
  .Internal(setwd(dir))
}



remove_timestamp <- function(x) {
  x <- x[,!names(x) %in% c("Timestamp")]
}


clean_first_col <- function(x){
  x[,1]<-sapply(x[,1],function(x) stringr::str_to_lower(x))
  pattern2 <- c(".*\\.\\s","\\s+","\\s+$")
  x[,1]<-sapply(x[,1],function(x) qdap::multigsub(pattern2,"",x))

}




remove_bracs <- function(x) {
  colnames(x) <- gsub("\\([A-Z]+_[A-Z]+_[A-z0-9]+_[0-9]+\\)","",colnames(x))

}



match_teacher_nm<- function(x,y) {
  x[match(y,x[,1]),]
}


adding_col_bind <- function(x) {
  emp1 <- data.frame(matrix(NA,nrow=1,ncol=ncol(x)))
  emp2 <- data.frame(matrix(NA,nrow=1,ncol=ncol(x)))
  colnames(emp1) <- colnames(x)
  colnames(emp2) <- colnames(x)
  x <- rbind(emp1,emp2,x)

}

#Reading in the UID  reference file and saving the sheet names
load_workbook_sheetname <- function(x) {
  wb <- xlsx::loadWorkbook(x)
  library(xlsx)
  y <- names(getSheets(wb))

}


load_narrange_UIDs <- function(xz,y,df) {
  for(k in 1:length(xz)){
    refer <- read.xlsx(y,sheetName = xz[k],stringsAsFactors=FALSE)
    ind <- apply(refer, 1, function(x) all(is.na(x)))
    pak <- apply(refer, 2, function(x) all(is.na(x)))
    refer <- refer[ !ind,!pak ]
    for(i in 1:ncol(df))
      for(j in 1:nrow(refer))
        if(grepl(colnames(df)[i],refer$Actual.Question.[j],fixed=TRUE)==TRUE){
          df[2,i] = refer[j,2]
          df[1,i] = refer[j,5]
        }
  }


}


binding_block <- function(x) {

  colnames(x[,1]) <- "NAMES"
  number <- min(which(!is.na(x[1,])))
  number1 <- max(which(!is.na(x[1,])))
  df2 <-x[,-(number:(number1+1))]
  df2 <- df2[!is.na(teach4[,1]),]
  df3 <- x[,number:(number1+1)]
  df4 <- cbind(df3,df4)

}


devtools::install_github("Shubham619/cleandata")
library(cleandata)

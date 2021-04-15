#' A function
#'
#' @param file A string
#' @param expr A variable in the dataframe
#'
#' @return A dataframe
#' @import dplyr
#' @import stringr
#' @import utils
#' @export

readacsv=function(file)
  file%>%read.csv(fileEncoding="UTF-8-BOM")%>%lapply(.,as.character)%>%as.data.frame%>%`names<-`(gsub("[[:punct:][:blank:]]", "",names(.)))

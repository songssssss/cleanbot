#' A function
#'
#' @param file A string
#' @param expr A variable in the dataframe
#'
#' @return A dataframe
#' @import dplyr
#' @import stringr
#' @import haven
#' @import utils
#' @export

readasav=function(file)
  file%>%read_sav%>%lapply(.,as.character)%>%as.data.frame%>%`names<-`(gsub("[[:punct:][:blank:]]", "",names(.)))

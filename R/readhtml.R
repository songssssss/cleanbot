#' A function
#'
#' @param file A string
#' @param expr A variable in the dataframe
#'
#' @return A dataframe
#' @import dplyr
#' @importFrom xml2 xml_find_all
#' @importFrom rvest read_html
#' @importFrom rvest html_table
#' @export
readhtml=function(file){
  read_html(file)%>%xml_find_all(.,"//table")%>%.[3]%>%html_table()%>%as.data.frame()%>%`colnames<-`(.[1,])%>%.[-1,]%>%
  lapply(df,as.character)%>%as.data.frame%>%`names<-`(gsub("[[:punct:][:blank:]]", "",names(df)))
}

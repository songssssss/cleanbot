#' A function
#'
#' @param file A string
#' @param expr A variable in the dataframe
#'
#' @return A dataframe
#' @import dplyr
#' @import utils
#' @import haven
#' @import stringr
#' @import writexl
#' @export
#'
Shinyreadonemerge=function(xls=F,path,keyword,writexlsx=F,writesav=F){
  setwd(path)
  if (xls==F)
    dir()%>%tolower%>%grepl(".xlsx",.)%>%dir()[.]->file
  else
    dir()%>%tolower%>%grepl(".xls",.)%>%dir()[.]->file

  filelist=file%>%set_names()%>%tolower()%>%str_detect(tolower(keyword))%>%file[.]
  print(filelist)
  alldata=list()
  if (xls==F){
    alldata=filelist%>%set_names()%>%map_df(readaexcel)}
  else{
    alldata=filelist%>%set_names()%>%map_df(readhtml)}
  if(writexlsx==T)
    alldata%>%writexl::write_xlsx(path = paste(keyword,".xlsx",sep = ""))
  if(writesav==T)
    alldata%>%haven::write_sav(path = paste(keyword,".sav",sep = ""))
  return(alldata)
}

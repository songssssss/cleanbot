#' A function
#'
#' @param file A string
#' @param expr A variable in the dataframe
#'
#' @return A dataframe
#' @import dplyr
#' @import purrr
#' @import rvest
#' @import stringr
#' @import writexl
#' @import haven
#' @import utils
#' @export
#need readsheets, readhtml functions
readnmerge=function(html=F){
  path=choose.dir(default = "", caption = "Select the folder where contains all raw data files.")
  print(path)
  setwd(path)

  catno=readline("Number of categories: ")%>%as.integer()
  keyword=vector(mode = "character",length = catno)
  for (i in 1:catno)
    keyword[i]=readline(prompt=paste("Enter keyword No.",i,": ",sep = ""))
  print(keyword)
  cat("Generating file list ...\n")

  filelist=dir()%>%set_names()%>%tolower()%>%
    map_dfr(str_detect,tolower(keyword))%>%t()%>%`colnames<-`(keyword)%>%
    apply(.,2,function(x)(which(x==T)%>%dir()[.]))
  print(filelist)
  if (class(filelist)=="character")
    filelist=as.list(filelist)
  else if (class(filelist)!="list")
    filelist=lapply(seq_len(ncol(filelist)), function(i) filelist[,i])%>%`names<-`(keyword)
  print(filelist)
  cat("Processing...\n")

  alldata=list()
  for(i in seq_along(filelist)){
    if (html==F){
      alldata[[i]]=filelist[[i]]%>%set_names()%>%map_df(readaexcel)}
    else{
      alldata[[i]]=filelist[[i]]%>%set_names()%>%map_df(readhtml)}

    alldata[[i]]%>%write_xlsx(path = paste(names(filelist)[i],".xlsx",sep = ""))
    alldata[[i]]%>%write_sav(path = paste(names(filelist)[i],".sav",sep = ""))
    cat("Finished:",names(filelist)[i],"\n")
  }

  cat("Done\n")
  return(alldata)
}

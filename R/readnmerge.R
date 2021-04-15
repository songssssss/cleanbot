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
readnmerge=function(html=F,writexlsx=F,writesav=F){
  path=choose.dir(default = "", caption = "Select the folder where contains all raw data files.")
  print(path)
  setwd(path)

  catno=readline("Number of categories: ")%>%as.integer()
  keyword=vector(mode = "character",length = catno)
  for (i in 1:catno)
    keyword[i]=readline(prompt=paste("Enter keyword No.",i,": ",sep = ""))
  print(keyword)
  cat("Generating file list ...\n")

  if (html==F)
    dir()%>%tolower%>%grepl(".xlsx",.)%>%dir()[.]->file
  else
    dir()%>%tolower%>%grepl(".xls",.)%>%dir()[.]->file

  filelist=file%>%set_names()%>%tolower()%>%
    map_dfr(str_detect,tolower(keyword))%>%t()%>%`colnames<-`(keyword)%>%
    apply(.,2,function(x)(which(x==T)%>%file[.]))
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
    if(writexlsx==T){
      maxno=1048000
      lengthno=dim(alldata)[1]%/%maxno+1
      for (i in 1:lengthno)
        alldata[((i-1)*maxno+1):i*maxno,]%>%writexl::write_xlsx(path = paste(names(filelist),i,".xlsx",sep = ""))
    }
    if(writesav==T)
      alldata[[i]]%>%haven::write_sav(path = paste(names(filelist)[i],".sav",sep = ""))
    cat("Finished:",names(filelist)[i],"\n")
  }

  cat("Done\n")
  return(alldata)
}

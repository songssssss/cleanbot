#' A function
#'
#' @param file A string
#' @param expr A variable in the dataframe
#'
#' @return A dataframe
#' @import purrr
#' @import stringr
#' @import utils
#' @export
diffmerge=function(filelist,path){
  df=data.frame()
  for (i in 1:length(filelist)) {
    if (grepl(".sav",filelist[i])){
      data=readasav(paste(path[i],filelist[i],sep = "/"))
    } else if (grepl(".xlsx",filelist[i])){
      data=readaexcel(paste(path[i],filelist[i],sep = "/"))
    } else if (grepl(".xls",filelist[i])){
      data=readhtml(paste(path[i],filelist[i],sep = "/"))
    } else{
      return(paste("Cannot identify the file type: ", filelist[i]),sep="")
    }
    df=bind_rows(df,data)
  }
  return(df)
}

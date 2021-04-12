#' A function
#'
#' @param file A string
#' @param expr A variable in the dataframe
#'
#' @return A dataframe
#' @importFrom dplyr %>%
#' @import readxl
#' @import stringr
#' @import purrr
#' @import utils
#' @export

readacleansheet=function(filename,sheetname){
  read_excel(filename,sheetname)->df
  if (is_empty(df))
    return(data.frame())

  headerind=df[1]%>%head(n=200)%>%as.matrix%>%grepl("Reference Key",.,useBytes = TRUE)%>%which
  if(!is_empty(headerind))
    df%>%`colnames<-`(df[headerind,])%>%.[-(1:headerind),]->df

  footerind=df[1]%>%tail(n=200)%>%as.matrix%>%grepl("No. of Records",.,useBytes = TRUE)%>%which
  if(!is_empty(footerind))
    df%>%.[1:(dim(.[1])[1]+footerind-202),]->df

  lapply(df,as.character)%>%as.data.frame%>%`names<-`(gsub("[[:punct:][:blank:]]", "",names(df)))
}

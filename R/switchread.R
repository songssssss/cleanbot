library(tools)
#' @param expr A variable in the dataframe
#'
#' @return A dataframe
#' @import tools
#' @import utils
#' @export

switchread=function(x){
  switch(file_ext(x),
         sav=readasav(x),
         xlsx=readaexcel(x),
         xls=readhtml(x),
         csv=readacsv(x),
         print(paste("Cannot identify the file type:", x)))
}

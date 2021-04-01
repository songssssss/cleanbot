#' A function
#'
#' @param file A string
#' @param expr A variable in the dataframe
#'
#' @return A dataframe
#' @importFrom dplyr %>%
#' @importFrom readxl excel_sheets
#' @importFrom purrr set_names
#' @importFrom purrr map_df
#' @export

readaexcel=function(file)
  file%>%excel_sheets()%>%set_names()%>%map_df(readacleansheet,filename=file)

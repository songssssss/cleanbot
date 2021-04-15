#' A function
#'
#' @param file A string
#' @param expr A variable in the dataframe
#'
#' @return A dataframe
#' @import purrr
#' @import dplyr
#' @import utils
#' @export

findMED=function(cohort,data){
  data%>%mutate_at(c("ReferenceKey","Multipledosageindicator","DispensingDuration","QuantityNamedPatient"),as.integer)%>%
    mutate_at(c("DispensingDateyyyymmdd","PrescriptionStartDate","PrescriptionEndDate"),as.Date)->data

  data$PrescriptionStartDate%>%ifelse(is.na(.),data$DispensingDateyyyymmdd,.)%>%as.Date(origin="1970-01-01")->data$PrescriptionStartDate
  data$PrescriptionEndDate%>%ifelse(is.na(.)&!is.na(data$DispensingDuration),data$PrescriptionStartDate+data$DispensingDuration,.)%>%as.Date(origin="1970-01-01")->data$PrescriptionEndDate
  data$PrescriptionEndDate%>%ifelse(is.na(.)&is.na(data$DispensingDuration),data$PrescriptionStartDate,.)%>%as.Date(origin="1970-01-01")->data$PrescriptionEndDate

  data%>%group_by(ReferenceKey)%>%summarise(Start= min(PrescriptionStartDate),End=max(PrescriptionEndDate))%>%
    left_join(cohort,.,by ="ReferenceKey")-> cohort

  cohort%>%mutate(Baseline=0)%>%mutate(Baseline=replace(Baseline,BaselineDate>Start,1))%>%
    select(ReferenceKey,Baseline)%>%
    left_join(cohort,.,by ="ReferenceKey") -> cohort

  return(cohort)
}

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
  data$PrescriptionStartDate%>%ifelse(is.na(.),data$DispensingDateyyyymmdd,.)->data$PrescriptionStartDate
  data$PrescriptionEndDate%>%ifelse(is.na(.)&!is.na(data$DispensingDuration),as.Date(data$PrescriptionStartDate)+as.numeric(data$DispensingDuration),.)->data$PrescriptionEndDate
  data$PrescriptionEndDate%>%ifelse(is.na(.)&is.na(data$DispensingDuration),data$PrescriptionStartDate,.)->data$PrescriptionEndDate

  data%>%group_by(ReferenceKey)%>%summarise(Start= min(PrescriptionStartDate),End=max(PrescriptionEndDate))%>%
    left_join(cohort,.,by ="ReferenceKey")-> cohort

  cohort%>%mutate(Baseline=0)%>%mutate(Baseline=replace(Baseline,BaselineDate>Start,1))%>%
    select(ReferenceKey,Baseline)%>%
    #setNames(.,c("ReferenceKey",paste("Baseline",medname,sep = "_")))%>%
    left_join(cohort,.,by ="ReferenceKey") -> cohort

  #names(cohort)[names(cohort)=="Start"]=paste(medname,"start_date",sep = "_")
  #names(cohort)[names(cohort)=="End"]=paste(medname,"end_date",sep = "_")

  #print(paste("Done:",medname))
  return(cohort)
}

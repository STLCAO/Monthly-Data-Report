library(dplyr)

#After getting a new file from a legal inquiry report add month column and get rid of group by column.
setwd("C:/Users/Gunathilakel/Desktop/Monthly Data Report/Monthly-Data-Report/RawData")


#While reading these files add a column that has a name 
myMergedData <- 
  do.call(rbind,
          lapply(list.files(getwd()), read.csv,stringsAsFactors=FALSE))


datain<-myMergedData%>%select(FileNumber,Stage,CaseStatus,NAME,Severity,Month)
#Refused Cases
datainRefused<-datain%>%filter(CaseStatus=='REFUSED',Stage!='CLOSED')%>%distinct(FileNumber,.keep_all = TRUE)
refusedCases<-datainRefused%>%split(datainRefused$Month)
#Issued Cases
datainIssued<-datain%>%filter(CaseStatus=="OPEN")%>%distinct(FileNumber,.keep_all = TRUE)
IssuedCases<-datainIssued%>%split(datainIssued$Month)
#Review Cases
datainReview<-datain%>%filter(CaseStatus=="REVIEW")%>%distinct(FileNumber,.keep_all = TRUE)
ReviewedCases<-datainReview%>%split(datainReview$Month)

refused<-sapply(refusedCases,function(l)(nrow(l)))
issued<-sapply(IssuedCases,function(l)(nrow(l)))
review<-sapply(ReviewedCases,function(l)(nrow(l)))
accepted<-issued+review
total<-refused+accepted
data.frame(refused,accepted,total)


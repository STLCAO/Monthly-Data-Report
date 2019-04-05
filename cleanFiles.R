library(dplyr)

#After getting a new file from a legal inquiry report-
#add month column to each file and get rid of "groupby" column
#Save all files in RawData folder


setwd("C:/Users/Gunathilakel/Desktop/Monthly Data Report/Monthly-Data-Report/RawData")

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

#Creating Dataframe with all the Issued-Refused-Files
refused<-sapply(refusedCases,function(l)(nrow(l)))
issued<-sapply(IssuedCases,function(l)(nrow(l)))
review<-sapply(ReviewedCases,function(l)(nrow(l)))
accepted<-issued+review
total<-refused+accepted
RefusalRate<-(refused/total*100)
data.frame(refused,accepted,total,RefusalRate)




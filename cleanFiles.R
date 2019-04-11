library(dplyr)
library(ggplot2)

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
reviewCases<-datainReview%>%split(datainReview$Month)

#Misdemeanor Cases
misdemCases<-datain%>%filter(Severity=="M")%>%distinct(FileNumber,.keep_all = TRUE)#%>%nrow())
#misdemCases<-misdemCases%>%split(misdemCases$Month)
write.csv(misdemCases,file = "C:/Users/Gunathilakel/Desktop/Monthly Data Report/Monthly-Data-Report/OuputFiles/misdemCases.csv")



#Creating Dataframe with all the Issued-Refused-Files
refused<-sapply(refusedCases,function(l)(nrow(l)))
issued<-sapply(IssuedCases,function(l)(nrow(l)))
review<-sapply(reviewCases,function(l)(nrow(l)))
accepted<-issued+review
total<-refused+accepted
RefusalRate<-(refused/total*100)
dataSummary<-data.frame(refused,accepted,total,RefusalRate)
dataSummary$'Month'<-rownames(dataSummary)
dataSummary$Month <- factor(dataSummary$Month, levels = c("December","January","February","March"))
dataSummary


##----------------------------------Creating a Visualization-----------------------------------##

pl<-ggplot(data = dataSummary,aes(x=Month,y=RefusalRate))+geom_point(color='red',size=2)+geom_line(group = 1)
pl2<-pl+coord_cartesian(ylim=c(0,100))+ scale_y_continuous(labels = function(x) paste0(x, "%"))+ggtitle("Case Refusal Rate")+theme(plot.title = element_text(hjust = 0.5))
pl2



rm(datain,datainIssued,datainRefused,myMergedData,datainReview,IssuedCases,reviewCases,refusedCases)



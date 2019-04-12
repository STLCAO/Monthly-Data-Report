library(dplyr)
library(reshape2)
library(ggplot2)

setwd("C:/Users/Gunathilakel/Desktop/Monthly Data Report/Monthly-Data-Report/warrantAppGenerated")

datain <- 
  do.call(rbind,
          lapply(list.files(getwd()), read.csv,stringsAsFactors=FALSE)) #Getting all warrant app generated files

write.csv(datain,"C:/Users/Gunathilakel/Desktop/Monthly Data Report/Monthly-Data-Report/OuputFiles/warrantappGenerated.csv")

warrantappGenerated<-datain%>%select(FileNumber,Month,Status)%>%distinct(FileNumber,.keep_all = TRUE) #all cases where warrant app was generated

misdemCases<-read.csv("C:/Users/Gunathilakel/Desktop/Monthly Data Report/Monthly-Data-Report/OuputFiles/misdemCases.csv",
                      stringsAsFactors = FALSE) #Getting all misdemeanor cases



#Need to change these parameters everyMonth we run these
MarchwarrantappGenerated<-warrantappGenerated%>%filter(Month=="March"|Month=="April")
MarchmisdemCases<-misdemCases%>%filter(Month=="March",CaseStatus=="OPEN")


rm(datain,misdemCases,warrantappGenerated)


misdemwithSummons<-dplyr::anti_join(MarchmisdemCases,MarchwarrantappGenerated,"FileNumber")


#-----------------Creating graph for misdem with summons--------------
misd.with.sum<-c(41,45,27,36)
misd.with.nosums<-c(22,27,14,21)
total.misd.cases.issued<-c(63,72,41,57)
misd.summons.rate<-c(65,63,66,63)

df<-data.frame(misd.with.sum,misd.with.nosums,total.misd.cases.issued,misd.summons.rate)
df<-t(df)
colnames(df)<-c("Dec-18","Jan-19","Feb-19","March-19")
summonsDf<-melt(df)%>%rename(Month=Var2,misdemeanor_with_summons=value)%>%filter(Var1=="misd.summons.rate")


pl<-ggplot(summonsDf,aes(x=Month,y=misdemeanor_with_summons,group=1))+geom_point(color='red',size=2)+geom_line(size=1)
pl+coord_cartesian(ylim=c(0,100))+xlab("Time")+ylab("Rate of Misdemeanors with Summons")+ggtitle("Misdemeanors with Summons")+ scale_y_continuous(labels = function(x) paste0(x, "%"))

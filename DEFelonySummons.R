library(dplyr)
library(ggplot2)

setwd("C:/Users/Gunathilakel/Desktop/Monthly Data Report/Monthly-Data-Report/DEFelonies")

datain<-read.csv("marchDEFElonies.csv",stringsAsFactors = FALSE)

deFelonyCases<-datain%>%distinct(FileNumber,.keep_all = TRUE)%>%select(FileNumber,Defendant,Month)


warrantappGenerated<-read.csv("C:/Users/Gunathilakel/Desktop/Monthly Data Report/Monthly-Data-Report/OuputFiles/warrantappGenerated.csv")


marchDEFelonyCases<-deFelonyCases%>%filter(Month=="March")
marchWarrantAppGenerated<-warrantappGenerated%>%filter(Month=="March"|Month=="April") #also important to have April to capture if warrants were issued at a later stage



caseswithNoWarrant<-dplyr::anti_join(marchDEFelonyCases,marchWarrantAppGenerated,"FileNumber")
caseswithNoWarrant%>%nrow()
marchDEFelonyCases%>%nrow()

rate<-caseswithNoWarrant%>%nrow()/marchDEFelonyCases%>%nrow()



#---------------------------Visualization-----------------------------------------

Time<-c("Dec-18","Jan-19","Feb-19","March-19")
DEFelonySummonsRate<-c(9,6,15,19)

df<-data.frame(Time,DEFelonySummonsRate)
df$Time<-factor(df$Time,levels = c("Dec-18","Jan-19","Feb-19","March-19"))


plot<-ggplot(data=df,aes(x=Time,y=DEFelonySummonsRate,group=1))+geom_point(color="red",size=1.5)+geom_line(size=0.9)
plot+coord_cartesian(ylim=c(0,100))+ scale_y_continuous(labels = function(x) paste0(x, "%"))+ggtitle("Rate of Felony Summons Issued")




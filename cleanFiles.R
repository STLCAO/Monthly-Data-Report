setwd("C:/Users/Gunathilakel/Desktop/Monthly Data Report/Monthly-Data-Report/RawData")
fileList<-list.files(getwd())

fileList


#While reading these files add a column that has a name 
myMergedData <- 
  do.call(rbind,
          lapply(list.files(getwd()), read.csv))



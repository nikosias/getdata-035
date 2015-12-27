library(reshape2)

#define variable
path <- "UCI HAR Dataset"
outFileName<-"./tidyData.txt"
#Load names for cols
getActivityLabels <- read.table(paste("./",path,"/activity_labels.txt",sep=""))
getAllFeatures <- read.table(paste("./",path,"/features.txt",sep=""))
#prepair vars
onlyMeanAndStd <- grepl("mean|std", getAllFeatures$V2)
nameActivity <- c("activityID", "activityLabel")
nameId <- c("subject",nameActivity)
allData<-F
#create vectors for file load
listsDir <- c("test","train")
listsFiles <- c("subj"="subject_","fileX"="X_","fileY"="y_")

#functions for read file
getFile<-function(name,dir){
  data <- read.table(paste("./",path,"/",dir,"/",name,dir,".txt",sep=""))
  data
}
#main loop for load and bind data
for( i in listsDir){
  #get all files from current dir
  data<-sapply(listsFiles,getFile,i)
  
  #Prepare data
  names(data$fileX) = getAllFeatures$V2
  data$fileX = data$fileX[,onlyMeanAndStd]
  data$fileY[,2] = getActivityLabels[data$fileY[,1],2]
  names(data$fileY) = nameActivity
  names(data$subj) = nameId[1]
  #bind columns
  bindData <- cbind(data$subj, data$fileY, data$fileX)
  #test and union rows
  if( class(allData)=="logical"){
    allData<-bindData
  }else{
    allData<-rbind(allData,bindData)
  }
}
#group data by subject+activityLabel
allData<-melt(allData,id=nameId)
groupData <- dcast(allData, subject+activityLabel ~ variable, mean)
#write data to file
write.table(groupData, file = outFileName,row.name=FALSE)
##Function

run_analysis <-function(){

##Reads the source
  
  X_test<-read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
  Y_test<-read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt")
  subject_test<-read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
  X_train<-read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
  y_train<-read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
  subject_train<-read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
  features<-read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")
  activity<-read.table("getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
  
  
  ##as character
  features[,2] <- as.character(features[,2])
  
  ##Bind sources 
  X<-rbind(X_test,X_train)
  Y<-rbind(Y_test,y_train)
  subject<-rbind(subject_test,subject_train)
  
  # Extract only the data on mean and standard deviation
  featuresWanted <- grep(".*mean.*|.*std.*", features[,2])
  featuresWanted.names <- features[featuresWanted,2]
  featuresWanted.names = gsub('-mean', 'Mean', featuresWanted.names)
  featuresWanted.names = gsub('-std', 'Std', featuresWanted.names)
  featuresWanted.names <- gsub('[-()]', '', featuresWanted.names)
  
 ## Renames
 colnames(subject)<-"Subject_ID"
 
 #Tyding Activity
 library(plyr)
 Activity<-join(Y,activity)
 Activity_final<-Activity[,2]
 Activity_final<-data.frame(Activity_final)
 colnames(Activity_final)<-"Activity_Label"
 
 #Extrating wanted features from X
 X<-X[,featuresWanted]
 


 ##Binding all sets together
 COMPLETE_SET<-cbind(subject,X,Activity_final)
 
 #Naming
 colnames(COMPLETE_SET) <- c("subject", "activity", featuresWanted.names)
 
 write.table(COMPLETE_SET,"Final.txt")
 
 
 #Calculating mean and SD

 library(data.table)
 TidyData<-data.table(COMPLETE_SET)
 Mean<-TidyData[,lapply(.SD,mean), by=c("subject","activity")]
 write.table(Mean,"MeanSD_Final.txt", row.name=FALSE)

}

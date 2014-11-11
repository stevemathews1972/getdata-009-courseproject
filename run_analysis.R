## This script will prepare the data file as directed by the 'course project' instructions at https://class.coursera.org/getdata-009/human_grading/view/courses/972587/assessments/3/submissions

## requires reshape2 package
## requires plyr package      

run_analysis<-function(){
      (!file.exists("WCProj")) {dir.create(".//WCProj")}
## set working directory and download and unzip data files
setwd("c://R//WCProj")
      url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      download.file(url,"data.zip")
      unzip("data.zip")
## read relevent text files to data fames
      activity_labels<-read.table(".//UCI HAR Dataset//activity_labels.txt")
      features<-read.table(".//UCI HAR Dataset//features.txt")
      x_test<-read.table(".//UCI HAR Dataset//test//x_test.txt")
      x_train<-read.table(".//UCI HAR Dataset//train//x_train.txt")
      y_test<-read.table(".//UCI HAR Dataset//test//y_test.txt")
      y_train<-read.table(".//UCI HAR Dataset//train//y_train.txt")
      subject_train<-read.table(".//UCI HAR Dataset//train//subject_train.txt")
      subject_test<-read.table(".//UCI HAR Dataset//test//subject_test.txt")
## Name colums of data sets
      names(x_test)<-features[,2]
      names(x_train)<-features[,2]
      names(y_test)<-"activity"
      names(y_train)<-"activity"
      names(subject_test)<-"subject"
      names(subject_train)<-"subject"
      names(activity_labels)<-c("activity","activity_name")

## Step 1 start
## Combine x and y sets
      test<-cbind(subject_test,y_test,x_test)
      train<-cbind(subject_train,y_train,x_train)
## Combine test and train sets
      data<-rbind(test, train)
## Step 1 complete

##Step 2 Start
## Restrict Column names relating to just mean and sd (but still include activity colum) - had to explicitly exclude *Freq as couldn't get escape characters to work!
      msdata<-(data[ , (grepl(pattern = "subject", names( data ) )|grepl(pattern = "activity", names( data ) )|grepl(pattern = "*std*", names( data ) )|grepl(pattern = "*mean*", names( data ) ))&!grepl(pattern = "*Freq*", names( data ) ) ])
##Step 2 complete

##Step 3 Start
## Merge msdata and activity _labels to get descriptive labels in msdata 
      fulldata<-merge(msdata,activity_labels,by.x="activity",by.y="activity",all.x = TRUE)
##Step 3 Complete

## Step 4 - already completed as part of data reading 

## Step 5 involves 'melting' i.e. making into flat file.

      
      flat <- melt(fulldata, id.vars = c("activity","activity_name","subject"),variable.name = "statistic")

## then summarising by key variable
      
      sumflat <- ddply(flat, c("activity", "activity_name","subject","statistic"), summarise,mean=mean(value))

## then splitting out all key variables

      split_out<-cbind(colsplit(string = sumflat$statistic,pattern ="-" ,names = c("sensor","statistic","axis")),sumflat)

## and re-arranging into a sensible order of columns and set factor to class of factor

      final<-split_out[,c(6,5,1,3,2,8)]
      unfactorize<-c(1:5)
      final[,unfactorize]<-lapply(unfactorize, function(x) as.factor(final[,x]))

final_table<<-final

write.table(final,file = "final_table.txt",row.names = FALSE)
}
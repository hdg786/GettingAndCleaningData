##################################################################################################################################
### Course Project : Getting and Cleaning Data

#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data 
#that can be used for later analysis. 
#This project uses data  collected from the accelerometers from the Samsung Galaxy S smartphone 
##################################################################################################################################


##################################################################################################################################
#Load required packages
###################################################################################################################################

library(plyr)
library(reshape2)
library(data.table)

##################################################################################################################################
# Set working directory to where file have been stored
###################################################################################################################################

myDir<-'C:/Users/heena/Documents/CleaningData/UCI HAR Dataset/'
setwd(myDir)


##################################################################################################################################
# First extract all relevant data, activity labels, features, training and test data
# create "data.frame" variables 
###################################################################################################################################

actv_labels = read.table('activity_labels.txt')
features = read.table('features.txt')
FeaturesTest <- read.table(paste0(myDir,'/test/X_test.txt'))
ActivityTest <- read.table(paste0(myDir,'/test/Y_test.txt'))
subjectTest <- read.table(paste0(myDir,'/test/subject_test.txt'))

FeaturesTrain <- read.table(paste0(myDir,'/train/X_train.txt'))
ActivityTrain <- read.table(paste0(myDir,'/train/Y_train.txt'))
subjectTrain <- read.table(paste0(myDir,'/train/subject_train.txt'))
                
##################################################################################################################################
# Combine test and training  data sets for Features, Activity and Subject
###################################################################################################################################

Features = rbind(FeaturesTest,FeaturesTrain)
Activity = rbind(ActivityTest,ActivityTrain)
Subject = rbind(subjectTest, subjectTrain)

names(Features)<-features$V2
names(Activity)<-"Activity"
names(Subject)<-"Subject"

##################################################################################################################################
# Merge everything to include into one data set
###################################################################################################################################


myData<-cbind(Features,Activity, Subject)
fnames<-names(Features)


##################################################################################################################################
# Extracts only the measurements on the mean and standard deviation for each measurement. 
###################################################################################################################################


mean_std_fnames<- fnames[grep(paste(c("std()","mean()"),collapse="|"), fnames)]
mean_std_fnames<-mean_std_fnames[-grep("meanFreq", mean_std_fnames)]


##################################################################################################################################
# Use merge to replace activity names (more descriptive) with activity numbers in combined data set
###################################################################################################################################


myData<-myData[,c(mean_std_fnames, "Activity", "Subject")]
myData<- merge(myData, actv_labels, by.x = "Activity", by.y = "V1", all.x = TRUE)
names(myData)
myData$Activity<-myData$V2
myData$V2<-NULL

##################################################################################################################################
# Use a clean up function or make.names to give tidier names 
#Appropriately labels the data set with descriptive variable names. 
###################################################################################################################################


cleanup<-function(v)
{
  v=gsub('BodyBody','Body',v)
  v=gsub('-','.',v)         
  v=gsub('\\(','',v)
  v=gsub(')','',v)
  v            
}
          
names(myData)<-make.names(cleanup(names(myData)))
myDataSummary<-aggregate(. ~Subject + Activity, myData, mean)

##################################################################################################################################
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each 
# activity and each subject. 
###################################################################################################################################


write.table(myDataSummary, 'myDataSummaryTidy.txt', row.names=FALSE)

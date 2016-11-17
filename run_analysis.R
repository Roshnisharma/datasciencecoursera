#loading file from URL
if(!file.exists("/datafile.csv")){
  print("no")
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile= "./datafile.csv")
}

# the Url has zipped file, thus unzip the file
unzip("datafile.csv")

#There are multiple files in the folder thus to know the list of files
filesinfolder <- list.files("UCI HAR Dataset", recursive = TRUE)
filesinfolder

# After reading README file as instructed in the assignment
# we understand that there is test and train data having variables whose names are listed in features file
# performing certain activity
# performed by certain subjects

# now read data into variables from file 
## Read Activities data, which has what activities were done
dataActivityTest <- read.table("UCI HAR Dataset/test/Y_test.txt", header= FALSE)
dataActivityTrain <- read.table("UCI HAR Dataset/train/Y_train.txt", header= FALSE)
str(dataActivityTest)
Str(dataActivityTrain)
### A quick look at activity files 

## read subject file
dataSubjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header= FALSE)
dataSubjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header= FALSE)
str(dataSubjectTest)
str(dataSubjectTrain)
### A quick look at subject files

## read features file
dataFeaturesTest <- read.table("UCI HAR Dataset/test/X_test.txt", header= FALSE)
dataFeaturesTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header= FALSE)
str(dataFeaturesTest)
str(dataFeaturesTrain)
### A quick look at features files

#1. Merge training and test data set to create one data set
## adding rows at end to get merged data set
dataSubject <- rbind(dataSubjectTest, dataSubjectTrain)
dataActivity <- rbind(dataActivityTest, dataActivityTrain)
dataFeatures <- rbind(dataFeaturesTest, dataFeaturesTrain)
## adding meaningful names to variables
names(dataSubject) <- c("subject")
names(dataActivity) <- c("activity")
dataFeaturesNames <- read.table("UCI HAR Dataset/features.txt",header=FALSE)
names(dataFeatures) <- dataFeaturesNames$V2
## merging whole data sets
dataMerged<- cbind(dataSubject,dataActivity)
dataMerged<- cbind(dataFeatures,dataMerged)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
MeanStdNames<-dataFeaturesNames$V2[grep("std\\(\\)|mean\\(\\)",dataFeaturesNames$V2)]
# After few trials and reading i understood the meanfrequency has to be excluded thus the "()"
FieldNames <- c(as.character(MeanStdNames),"activity","subject")
dataMergedlater<- dataMerged[,FieldNames]
# checking if i get correct values
str(dataMergedlater)

#3.Uses descriptive activity names to name the activities in the data set
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header= FALSE)
##Factor the column with labels from activity labels table
dataMergedlater$activity <- factor(dataMergedlater$activity, labels = as.character(activityLabels$V2))

#4.Appropriately labels the data set with descriptive variable names
#In 1. we have already names activity and subject properly, we have name restall variables
names(dataMergedlater)
# looking at data and the names in datanames page it is found that:
# Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
###Thus Acc is reading from accelerometer
# Triaxial Angular velocity from the gyroscope. 
###Thus Gyro is reading from the gyroscope
# A 561-feature vector with time and frequency domain variables. 
###Thus t stands for time and f stands for frequency
###BodyBody stands for body, Mag for Magnitude
# Its activity label. 
# An identifier of the subject who carried out the experiment.
### Above two points are being already taken care of
names(dataMergedlater)<- gsub("^t","time",names(dataMergedlater))
names(dataMergedlater)<- gsub("^f","frequency",names(dataMergedlater))
names(dataMergedlater)<- gsub("Acc","Accelerometer",names(dataMergedlater))
names(dataMergedlater)<- gsub("Gyro","Gyroscope",names(dataMergedlater))
names(dataMergedlater)<- gsub("Mag","Magnitude",names(dataMergedlater))
names(dataMergedlater)<- gsub("BodyBody","Body",names(dataMergedlater))
names(dataMergedlater)

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
dataMergedlater1<- aggregate(.~subject+activity, dataMergedlater, mean)
dataMergedlater1<- dataMergedlater1[order(dataMergedlater1$subject, dataMergedlater1$activity),]
write.table(dataMergedlater1, file = "tidydata.txt",row.name= FALSE)
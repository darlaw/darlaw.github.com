
#See README.md for details and instructions about this script

#------------------SET UP ENVIRONMENT-------------------------#
#**STEP 1: Set up the environment
#**USER TODO: change the working directory to the location of the R project files

#Import libraries
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("ggplot2",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(ggplot2) #to plot data

if("reshape2" %in% rownames(installed.packages()) == FALSE) {
  install.packages("reshape2",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(reshape2) #reshape using melt and cast

if("dplyr" %in% rownames(installed.packages()) == FALSE) {
  install.packages("dplyr",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(dplyr) #to summarise data

if("gridExtra" %in% rownames(installed.packages()) == FALSE) {
  install.packages("gridExtra",
                   repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))}
library(gridExtra) #arrange plots into a grid

setwd("C:/_portfolio/courses/_coursera.cleaningdata")
source("cleanfeaturenames.R")
source("getvalues.R")

#**STEP 2: Download and unzip the source data files
dest <- paste(getwd(),"/UCIHARDataset.zip",sep="")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles/UCI%20HAR%20Dataset.zip", 
              dest, mode="wb",method="internal")
unzip(dest,exdir="C:/_stuff",junkpaths=TRUE)

#-----------------SET UP TRAINING DATA------------------------#
#**STEP 3: Get the training data from x_train.txt
#file contains one or more white spaces between each value
traindata <- read.table("UCI HAR Dataset/train/X_train.txt",
                        sep="",colClasses=rep("character",561))

#**STEP 4: get the list of features from features.txt and trim whitespace
featurenames <- read.table("UCI HAR Dataset/features.txt",
                           sep="", stringsAsFactors=FALSE)
if (!"stringr" %in% rownames(installed.packages())) {install.packages("stringr")}
library(stringr)
featurenames$V3 <- str_trim(featurenames$V2) # make sure there are no leading, trailing spaces

#**STEP 5: convert featurename values to valid column names
featurenamescleaned <- cleanfeaturenames(featurenames)

#**STEP 6: use the list of features to create column names for the training data
colnames(traindata) <- featurenamescleaned$V2

#**STEP 7: get activity id labels for traindata from y_train.txt
activitytrain <- read.table("UCI HAR Dataset/train/y_train.txt",sep="")

#**STEP 8: Get the subject Ids from subject_train.txt
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt",sep="")

#**STEP 9: use the activitytrain data to add a new column for activityId
traindata$ActivityId <- activitytrain[,1] #add a new column

#**STEP 10: use the subjecttrain to add a new column for subjectId
traindata$SubjectId <- subjecttrain[,1]  #add a new column

#-----------------------SETUP TEST DATA -------------------------#
#**STEP 11: Get the test data 
testdata <- read.table("UCI HAR Dataset/test/X_test.txt",
                       sep="",colClasses=rep("character",561))

#**STEP 12: use the list of features to create column names for the test data
colnames(testdata) <- featurenamescleaned$V2

#**STEP 13: get activity id labels for test data from y_test.txt
activitytest <- read.table("UCI HAR Dataset/test/y_test.txt",sep="")

#**STEP 14: Get the subject Ids from subject_test.txt
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt",sep="")

#**STEP 15: use the activitytest data to add a new column for activityId
testdata$ActivityId <- activitytest[,1] #add a new column

#**STEP 16: use the subjecttest to add a new column for subjectId
testdata$SubjectId <- subjecttest[,1] #add a new column

#--------------MERGE TRAINING AND TEST DATA TO ONE DATAFRAME--------------#
#**STEP 17: MERGE training and test data
alldata <- rbind(testdata,traindata) #keep

#----------AGGREGATE DATA BASED ON Activity Id AND SubjectId------------#
#**STEP 18: get the column names that contain the value - 'mean'
matches <- getvalues(alldata,"\\.std\\.|.std$|\\.mean\\.|.mean$")

#**STEP 19: Use descriptive activity names to name the activities in the data set
key = c(1,2,3,4,5,6)
value = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
i=1
for(i in 1:length(key)){
  alldata$ActivityId <- replace(alldata$ActivityId, alldata$ActivityId == key[i],value[i])
  i=i+1
}

#**STEP 20: create a subset of the data with the variables for mean 
avgReadings <- subset(alldata, select = colnames(alldata) %in% matches)

#**STEP 21: convert datatype to double
nAvgReadings <- as.data.frame(apply(avgReadings,c(1, 2),as.double))

#**STEP 22: Add activityId and subjectId columns#
nAvgReadings <- cbind(nAvgReadings,alldata$ActivityId)
colnames(nAvgReadings)[ncol(nAvgReadings)] <- "ActivityId"
nAvgReadings <-cbind(nAvgReadings,as.factor(alldata$SubjectId))
colnames(nAvgReadings)[ncol(nAvgReadings)] <- "SubjectId"

#**STEP 23: Calculate mean of each ActivityId and Subject Id combination
attach(nAvgReadings)
aggNAvgReadings <- aggregate(nAvgReadings[1:(ncol(nAvgReadings)-2)], by=list(ActivityId,SubjectId), mean)
colnames(aggNAvgReadings)[1] <- "ActivityId"
colnames(aggNAvgReadings)[2] <- "SubjectId"

#-------------------CREATE A MOLTEN DATA SET AND CORRECT MESSY DATA SET ----------------------#
#column headers are variable names
#one variable is stored per  column
#variables are not stored in both rows and columns

#**STEP 24: melt the data frame
if (!"reshape" %in% rownames(installed.packages())) {install.packages("reshape")}
library(reshape)
meltedReadings <- melt(aggNAvgReadings,  id.vars=c("SubjectId","ActivityId"))

#**STEP 25: create and populate a new timeScaleType column as raw or fourier 
meltedReadings$timeScaleType <- "NA" #create the new column
i=1
for (i in 1:length(meltedReadings$timeScaleType)){
  if(grepl("^[t]",meltedReadings$variable[i])) { meltedReadings$timeScaleType[i] <- "raw" }
  else if(grepl("^[f]",meltedReadings$variable[i])) { meltedReadings$timeScaleType[i] <- "fourier" }
}

#**STEP 26: populate measurementType column  as x-axis, y-axis, z-axis, or magnitude
meltedReadings$measurementType <- "NA" #create the new column
i=1
for (i in 1:length(meltedReadings$variable)){
  if(grepl("[.X]$",meltedReadings$variable[i])) { meltedReadings$measurementType[i] <- "x axis" }
  else if(grepl("[.Y]$",meltedReadings$variable[i])) { meltedReadings$measurementType[i] <- "y axis" }
  else if(grepl("[.Z]$",meltedReadings$variable[i])) { meltedReadings$measurementType[i] <- "z axis" }
  else if(grepl("[Mag]",meltedReadings$variable[i])) { meltedReadings$measurementType[i] <- "magnitude" }
}

#**STEP 27: populate sensor column as gyro or accelerometer
meltedReadings$sensorType <- "NA" #create the new column
i=1
for (i in 1:length(meltedReadings$variable)){
  if(grepl("Gyro",meltedReadings$variable[i])) { meltedReadings$sensorType[i] <- "gyro" }
  else if(grepl("Acc",meltedReadings$variable[i])) { meltedReadings$sensorType[i] <- "accelerometer" }
}

#**STEP 28: populate sensor subtype column as body or gravity
meltedReadings$sensorTypeSubMeasure <- "NA" #create the new column
i=1
for (i in 1:length(meltedReadings$variable)){
  if(grepl("Body",meltedReadings$variable[i])) { meltedReadings$sensorTypeSubMeasure[i] <- "body" }
  else if(grepl("Gravity",meltedReadings$variable[i])) { meltedReadings$sensorTypeSubMeasure[i] <- "gravity" }
}

#**STEP 29: populate movement type column as normal or jerk
meltedReadings$movementType <- "NA" #create the new column
i=1
for (i in 1:length(meltedReadings$variable)){
  if(grepl("Jerk",meltedReadings$variable[i])) { meltedReadings$movementType[i] <- "jerk" }
  else if(!grepl("Jerk",meltedReadings$variable[i])) { meltedReadings$movementType[i] <- "normal" }
}

#**STEP 30: populate valueType column as mean or std.dev 
meltedReadings$valueType <- "NA" #create the new column
i=1
for (i in 1:length(meltedReadings$variable)){
  if(grepl("[Mm]ean",meltedReadings$variable[i])) { meltedReadings$valueType[i] <- "mean" }
  else if(grepl("[Ss]td",meltedReadings$variable[i])) { meltedReadings$valueType[i] <- "std.dev" }
}

#**STEP 31: Reorder columns
meltedReadings <- meltedReadings[,c("SubjectId","ActivityId",
                                    "timeScaleType","measurementType",
                                    "sensorType","sensorTypeSubMeasure",
                                    "movementType","valueType",
                                    "value","variable")]

#**STEP 32: Remove the variable column with original variable name 
meltedReadings <- subset(meltedReadings, select=c("SubjectId","ActivityId",
                                  "timeScaleType", "measurementType",
                                  "sensorType","sensorTypeSubMeasure",
                                  "movementType","valueType",
                                  "value"))

#--------------------------CREATE A TIDY DATASET ----------------#
#**STEP 33: Cast meltedReadings to move mean and std.dev into separate columns
tidydata  <- cast(meltedReadings, ...~valueType)

#**STEP 34: export tidy data set to txt file in working directory
write.table(tidydata, file = "project01_tidy.txt", sep=",", quote=FALSE,row.names = FALSE)

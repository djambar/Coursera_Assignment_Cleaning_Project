
#############################################################                                                               #
#          Getting and Cleaning Data Course Project         #
#                 R Script - Assignment                     #
#                    by Djambar                             #
#############################################################


  rm(list = ls())

# Q1. Download the zip file in the data folder of the project ------------------------------------------

  setwd("~/R/Assignment_Cleaning_Project")  ## project directory

  if(!file.exists("data")){
    dir.create("data")
  } 

  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileurl,destfile="./data/ucihardataset.zip",mode = "wb")

  if (!file.exists("UCI HAR Dataset")) { 
  unzip("./data/ucihardataset.zip", exdir="data") 
   }

# Q2. Merge the training and the test sets to create one data set. ------------------------------------
 
  ## Read the training and test data sets
  trainlabels <- read.table("../data/UCI HAR Dataset/train/y_train.txt")
  trainingset <- read.table("../data/UCI HAR Dataset/train/X_train.txt")
  traininsubject <- read.table("../data/UCI HAR Dataset/train/subject_train.txt")

  testlabels <- read.table("../data/UCI HAR Dataset/test/y_test.txt") ; str(testlabels)
  testset <- read.table("../data/UCI HAR Dataset/test/X_test.txt") ; str(testset)
  testsubject <- read.table("../data/UCI HAR Dataset/test/subject_test.txt") ; str(testsubject)

  ## Combine horizontally the subjet, activity and feature sets
  csubjects <-  rbind(traininsubject, testsubject)
  clabels <- rbind(trainlabels, testlabels)
  csets <- rbind(trainingset,  testset)
  
  names(csubjects) <- c("subject")
  names(clabels) <-  c("activity")
  features <- read.table("../data/UCI HAR Dataset/features.txt", header = FALSE)
  names(csets)  <- features$V2   

  ## Combine vertically the training and test data sets
  
  conso <-  cbind(csubjects, csets, clabels)
  library(dplyr)
  conso <-  tbl_df(conso)
  conso
  
# Q2. Extract only the measurements on the mean and standard deviation for each measurement.
  rm(measures)
  basicstats <- grep(".*mean.*|.*std.*", as.character(features[,2]))
  featureselected <-  as.character(features[basicstats, 2])
  test <- identical(featureselected, unique(featureselected)) # verify no dupplicates
  measures <-  subset(conso, select = c("subject", "activity", featureselected))
  measures
 

# Q3. Use descriptive activity names to name the activities in the dataset ----------------------------

  ActivityLabels <- read.table("../data/UCI HAR Dataset/activity_labels.txt", header = FALSE) 
  as.factor(measures$activity)
  measures$activity <- factor(measures$activity, levels = ActivityLabels[,1], labels = ActivityLabels[,2])
 

# Q4. Appropriately label the data set with descriptive variable name --------------------------------
# Measures of multiple signals are taken both in the time and frequency domains
# prefix 't' denotes time  ; f denotes frequency.
# for the clarity of the labelling, we will recode t as time and f as frequency
# we also remove unnecessary characters to remain with comprensible short names
# the short names indicated in the feture-info.txtare taken as a template.
  
  # names(measures) <- gsub("^t", "time_", names(measures))
  # names(measures) <- gsub("^f", "", names(measures))
  names(measures) <- gsub("[-()]", "", names(measures))
  names(measures) <- gsub("BodyBody", "body", names(measures)) # to remove duplicate nouns
  names(measures) <- tolower(names(measures)) # as adviced by Hadley Wickham and Google's R Style Guide
  names(measures) <- gsub("mean", ".mean.", names(measures))
  names(measures) <- gsub("std", ".std.", names(measures))
  names(measures) <- gsub("mean.$", "mean", names(measures))
  names(measures) <- gsub("std.$", "std", names(measures))

  names(measures) <- gsub("freqx$", "freq.x", names(measures))
  names(measures) <- gsub("freqy$", "freq.y", names(measures))
  names(measures) <- gsub("freqz$", "freq.z", names(measures))
 
 
  names(measures) <- gsub("tbody", "tbody.", names(measures))
  names(measures) <- gsub("fbody", "fbody.", names(measures))
  names(measures) <- gsub("tgravity", "tgravity.", names(measures))
  


# Q5. average of each variable for each activity and each subject.------------------------------

# recall that tidy data means that :
# i) each variable forms a column,
# ii) each observation formas a row, 
# iii) each table stores data about one kind of observation.
# see vignette http://127.0.0.1:10330/help/library/tidyr/doc/tidy-data.html from Hadley Wickham
# condition i) is violated in the data frame _measures_ because we have for instance more than one row for 
# subject 1.
  
# With package dplyr, we summarize the results by subject and activity (so subject and activity can be used as a key)
  hardata <- measures %>% group_by(activity, subject) %>% summarise_each(funs(mean)) 
  glimpse(hardata)
  dim(hardata)
  View(hardata)
  
# Finally we write the measures of the human activity data in a file
  write.table(hardata, "../data/hardata.txt", row.names = FALSE, quote = FALSE)
  save(hardata, file = "../data/hardata.Rda")
# You can read the data by typing : tidy <- read.table("../data/hardata.txt", header = TRUE) ; View( tidy )
# or just load the Rdata by typing load("../data/hardata.Rda")

  prompt(hardata) # quick way to create a  template of the documentation of a dataset.  


  
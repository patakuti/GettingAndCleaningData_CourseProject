## This file is R script file for
##   * calculates mean and standard deviation  
##   * creates a tidy data set with the average of each variable for each
##     activity and each subject  
## of acceleration and gravity data.
## 
## This script needs data files in the directory named UCI HAR Dataset.
## The data files can be get from
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
##
## This script output these files.
##   * result_mean.txt
##        mean for each measurement.
##   * result_sd.txt
##        standard deviation for each measurement.
##   * tidy_dataset.txt
##        the average of each variable for each activity and each subject.

nrows <- -1
datadir <- "UCI HAR Dataset/"

features <- read.table("UCI HAR Dataset/features.txt", header=FALSE, colClasses="character")
features <- features[[2]]

activityLabelsDf <- read.table("UCI HAR Dataset/activity_labels.txt", header=FALSE, colClasses="character")
activityLabels = list()
for (i in 1:nrow(activityLabelsDf)) {
    activityLabels[[activityLabelsDf[i,1]]] = activityLabelsDf[i,2]
}

allDf <- data.frame()
allActivity <- c()
allSubject <- c()

## 1. Merges the training and the test sets to create one data set.
for (type in c("train", "test")) {
        dfFile    <- paste(c(datadir, type, "/X_", type, ".txt"), collapse="")
        subjFile  <- paste(c(datadir, type, "/subject_", type, ".txt"), collapse="")
        actvtFile <- paste(c(datadir, type, "/y_", type, ".txt"), collapse="")
        df       <- read.table(dfFile   , header=F, nrows=nrows, colC="numeric")
        activity <- read.table(actvtFile,header=F,nrows=nrows,colC="character")
        subject  <- read.table(subjFile ,header=F,nrows=nrows,colC="character")
        allDf       <- rbind(allDf, df)
        allActivity <- rbind(allActivity, activity)
        allSubject  <- rbind(allSubject, subject)
}

allDf <- cbind(allDf, allActivity, allSubject)

## 4. Appropriately labels the data set with descriptive variable names. 
colnames(allDf) <- c(features, "activity", "subject");
n <- length(features)

## 2. Extracts only the measurements on the mean and standard deviation
##    for each measurement.
write.table(apply(allDf[1:n], 2, mean), "result_mean.txt")
write.table(apply(allDf[1:n], 2, sd)  , "result_sd.txt")

## 3. Uses descriptive activity names to name the activities in the data
##    set
allDf$activity <- sapply(allDf$activity, function(x,labels) {return(labels[[x]])}, activityLabels)

## 5. Creates a second, independent tidy data set with the average of
##    each variable for each activity and each subject.
activitySubject <- apply(allDf[-(1:n)], 1, paste, collapse="_")
activitySubjectDf <- apply(allDf[1:n], 2, function(x,as) {return(tapply(x,as,mean))}, activitySubject)
rownames <- rownames(activitySubjectDf)
activity <- sub("(.*)_[0-9]*", "\\1", rownames)
subject  <- sub(".*_([0-9]*)", "\\1", rownames)
activitySubjectDf <- cbind(activitySubjectDf, activity, subject)
write.table(activitySubjectDf, "tidy_dataset.txt")

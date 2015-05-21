## alternative_analysis.R - 15 May 2015 - Ed Rogers
#
# This version of the run_analysis code uses
# mutate_each() instead of summarise_each().
# The result is the original data.frame has
# columns appended with the averages for each
# subject and activity. Because there are multiple
# rows for each subject and activity combination,
# values will be repeated many times in the new,
# appended columns.

suppressPackageStartupMessages(library(dplyr,quietly = TRUE))

# Read in the names of each of the columns for our data.frame first
featureNames <- c("Subject",
                  "Activity",
                  read.table(file = "UCI HAR Dataset/features.txt",
                             header = FALSE,
                             stringsAsFactors = FALSE)[,"V2"]
                  )

# Read in the names of the activities to be used as factors
activities <- read.table(file = "UCI HAR Dataset/activity_labels.txt",
                         header = FALSE,
                         stringsAsFactors = FALSE)[,"V2"]

# Read in the test and train data from the 6 relevant files
trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
trainSubj <- read.table("UCI HAR Dataset/train/subject_train.txt",colClasses = "factor")
trainActv <- read.table("UCI HAR Dataset/train/y_train.txt",colClasses = "factor")
testData  <- read.table("UCI HAR Dataset/test/X_test.txt")
testSubj  <- read.table("UCI HAR Dataset/test/subject_test.txt",colClasses = "factor")
testActv  <- read.table("UCI HAR Dataset/test/y_test.txt",colClasses = "factor")

# rbind the 6 train and test tables into 3 data.frames:
combinedData <- rbind(trainData,testData)
combinedSubj <- rbind(trainSubj,testSubj)
combinedActv <- rbind(trainActv,testActv)

# cbind the three data.frames into one
dataHAR <- cbind(combinedSubj,combinedActv,combinedData)

# Name the columns
colnames(dataHAR) <- featureNames

# Rename the Activity factors
levels(dataHAR$Activity) <- activities

# Now that we have the data.frame we want, 
# we can clean up the environment a bit.
rm(trainData,trainSubj,trainActv,
   testData,testSubj,testActv,
   combinedData,combinedSubj,combinedActv,
   featureNames,activities)

# Extract "only the measurements on the mean 
# and standard deviation for each measurement."
# See Codebook for details.
namesWithMean             <- grep("mean",  names(dataHAR),value=TRUE,ignore.case = TRUE)
namesWithMeanButNotAngles <- grep("^angle",namesWithMean, value=TRUE,invert=TRUE)
namesWithStd              <- grep("std",   names(dataHAR),value=TRUE,ignore.case = TRUE)
dataHAR <- dataHAR[,c("Subject","Activity",namesWithMeanButNotAngles,namesWithStd)]


# Now, we can bind new columns to the original 
# data.frame with the averages for each feature, 
# subject, and activity. This would widen the original 
# 81 (= 2 factors + 79 features) column data.frame to 
# 160 columns (= 2 factors + 79 features + 79 "mean features").
# In this scheme, any two rows that share the same
# Subject and Activity, the "mean features" will be
# identical.
# To accomplish this, the mutate_each() function is
# used.
#
# Note: mutate_each() would overwrite columns if only
# one function was given. As of dplyr 0.4.1, an 
# extra function must be given to coerce mutate_each()
# to create new columns. The columns associated
# with the extra function are then immediately 
# removed.
# More info on this is available here:
# https://github.com/hadley/dplyr/issues/712

dataHARWithMeans <- dataHAR %>% group_by(Subject,Activity) %>% mutate_each(funs(mean(.),duplicate=mean(.)))
nonDuplicateCols <- grep("duplicate",names(dataHARWithMeans),value=TRUE,invert=TRUE)
dataHARWithMeans <- dataHARWithMeans[,nonDuplicateCols]

# # The commented code below demonstrates 10 rows of data for
# # 2 features and the 2 factors:
# featuresOfInterest <- c("Subject","Activity","tBodyAcc-mean()-X","tBodyAcc-mean()-X_mean")
# dataHARWithMeans[1:10,featuresOfInterest]

write.csv(dataHARWithMeans,file = "TidyAveragesMutateHAR.csv",row.names=FALSE)

# # Note: write.csv replaces special characters in feature names
# # with "."  For example "tBodyAcc-mean()-X" becomes
# # "tBodyAcc.mean...X"
# #
# # To read in this file later:
# 
# myColClasses <- c(rep("factor",times=2),rep("numeric",times=158))
# fromFileData <- read.csv("TidyAveragesMutateHAR.csv",colClasses=myColClasses)
# 
# # Demonstrate what's in the CSV by printing
# # a subset of the loaded data.frame
# featuresOfInterest <- c("Subject","Activity","tBodyAcc.mean...X","tBodyAcc.mean...X_mean")
# fromFileData[1:10,featuresOfInterest]
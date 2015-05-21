## run_analysis.R - 15 May 2015 - Ed Rogers
#
# For more information, see the included README
# and Codebook

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

# After grouping, the summarise_each() function will
# condense each feature to its average for each Subject
# and Activity. Thus, only 180 values (= 30 Subjects * 
# 6 Activities) are needed for each of the 79 features.
# The resulting data.frame is 81 columns (= 2 factors +
# 79 features) by 180 rows.
#
# Note: As of dplyr 0.4.1, "summarize_each()" (with an 
# American spelling) does not exist. Only summarise_each()

dataHARMeansOnly <- dataHAR %>% group_by(Subject,Activity) %>% summarise_each(funs(mean))

## The commented code below demonstrates 10 rows of data for
## 2 features and the 2 factors:
# featuresOfInterest <- c("Subject","Activity","tBodyAcc-mean()-X","tBodyAcc-std()-X")
# dataHARMeansOnly[1:10,featuresOfInterest]


# For this activity, we will only save the smaller
# 180x81 data.frame.

write.csv(dataHARMeansOnly,file = "TidyAveragesHAR.csv",row.names=FALSE)

# # Note: write.csv replaces special characters in feature names
# # with "."  For example "tBodyAcc-mean()-X" becomes
# # "tBodyAcc.mean...X"
# #
# # To read in this file later:
# 
# myColClasses <- c(rep("factor",times=2),rep("numeric",times=79))
# fromFileData <- read.csv("TidyAveragesHAR.csv",colClasses=myColClasses)
# 
# # Demonstrate what's in the CSV by printing
# # a subset of the loaded data.frame
# featuresOfInterest <- c("Subject","Activity","tBodyAcc.mean...X","tBodyAcc.std...X")
# fromFileData[1:10,featuresOfInterest]
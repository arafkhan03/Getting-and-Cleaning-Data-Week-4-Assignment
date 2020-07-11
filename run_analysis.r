################# Check whether the files exits and downloads them

filename <- "getdata_projectfiles_UCI HAR Dataset.zip"

# Check if zip exists
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Check if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# load the files
getwd <- "./UCI HAR Dataset/"
features <- read.table("./UCI HAR Dataset/features.txt", 
                       col.names = c("featureno","feature"))
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", 
                         col.names = c("activityno", "activity"))

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", 
                           col.names = "subjectid")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt", 
                     col.names = features$feature)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", 
                     col.names = "activityno")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", 
                            col.names = "subjectid")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt", 
                      col.names = features$feature)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", 
                      col.names = "activityno")




################# 01 Merging All Data
merg_test <- cbind(subject_test, y_test, x_test)
merg_train <- cbind(subject_train, y_train, x_train)

final <- rbind(merg_test, merg_train)




################# 02 Mean & SD for All
library(plyr);library(dplyr)

f <- function(x){
  list(mean(x),sd(x))
}
meansd <- select(final, tBodyAcc.mean...X:angle.Z.gravityMean.)
meansd <- as.data.frame(sapply(meansd, f), row.names = c("Mean", "Standard Deviation"))




################# 03 Descriptive Activity Names to Name the Activities in the Data Set
activitynames <- left_join(final, activities)
activitynames <- activitynames %>% select(subjectid, activityno, activity, everything())




################# 04 Appropriately Labels the Data Set with Descriptive Variable Names
colnames(activitynames) <- gsub("^[t]", "Time ", colnames(activitynames))
colnames(activitynames) <- gsub("tBody", "Time Body ", colnames(activitynames))
colnames(activitynames) <- gsub("Acc", " Acceleration ", colnames(activitynames))
colnames(activitynames) <- gsub("[Gg]yro", " Gyroscope", colnames(activitynames))
colnames(activitynames) <- gsub("[Mm]ag", " Magnitude ", colnames(activitynames))
colnames(activitynames) <- gsub("^f", "Frequency ", colnames(activitynames))
colnames(activitynames) <- gsub("\\.", " ", colnames(activitynames))
colnames(activitynames) <- gsub("\\.s+", "", colnames(activitynames))
colnames(activitynames) <- gsub(' +', ' ', colnames(activitynames)) 
colnames(activitynames) <- gsub('BodyBody', 'Body', colnames(activitynames)) 

library(stringr)
colnames(activitynames) <- str_trim(colnames(activitynames))
colnames(activitynames) <- tolower(colnames(activitynames))


  

################# 05 Mean & SD for Each Activity and Each Subject
tidy <- aggregate(activitynames[, 4:564], list("Activity" = activitynames$activity, 
                                             "Subject Id" = activitynames$subjectid), mean)
colnames(tidy)


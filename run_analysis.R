#Load dplyr
library(dplyr)

# Checking if folder already exists in wd. If not, downloads and unzip
if (!file.exists("UCI HAR Dataset")) { 
        download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', destfile = 'Course3Assignment.zip', method="curl")
        unzip('Course3Assignment.zip') 
}

### ITEM 1: Reads and assign names to test and train data and merges into one dataset

## Saves features names to pass as colnames
features <- read.table('./UCI HAR Dataset/features.txt', col.names = c('featureid', 'featurename'))

## Reads main test table
x_test <- read.table('./UCI HAR Dataset/test/X_test.txt', col.names = features$featurename)
## Reads activity test table and change to proper name
y_test <- read.table('./UCI HAR Dataset/test/y_test.txt')
colnames(y_test)[1] <- 'activity'
## Reads subject test table and change to proper name
subject_test <- read.table('./UCI HAR Dataset/test/subject_test.txt')
colnames(subject_test)[1] <- 'subject'
## Merge test tables to a single data frame using cbind
test_complete <- cbind(x_test, y_test, subject_test)

## Reads main train table
x_train <- read.table('./UCI HAR Dataset/train/X_train.txt', col.names = features$featurename)
## Reads activity train table and change to proper name
y_train <- read.table('./UCI HAR Dataset/train/y_train.txt')
colnames(y_train)[1] <- 'activity'
## Reads subject train table and change to proper name
subject_train <- read.table('./UCI HAR Dataset/train/subject_train.txt')
colnames(subject_train)[1] <- 'subject'
## Merge test tables to a single data frame using cbind
train_complete <- cbind(x_train, y_train, subject_train)

## rbind test and train data to a new data frame
completedf <- rbind(test_complete,train_complete)
### END OF ITEM 1

###ITEM 2 - "Extracts only the measurements on the mean and standard deviation for each measurement."

## Colect rows #'s in features that have the words 'mean' OR 'stc'
id_mean_std <- grep('mean|std', features$featurename)

## Subset to extract only columns with mean and std variables
tidy_dataset <- cbind(completedf[,id_mean_std], completedf[,562:563])
### END OF ITEM 2

###ITEM 3 - Uses descriptive activity names to name the activities in the data set

## Activities labels to 'activities'
activities <- read.table('./UCI HAR Dataset/activity_labels.txt', col.names = c('actid', 'actname'))
## Replaces activity codes with activity description
tidy_dataset$activity <- activities[tidy_dataset$activity, 2]
### END OF ITEM 3

###ITEM 4 - Appropriately labels the data set with descriptive variable names.
names(tidy_dataset)<-gsub("Gyro", "Gyroscope", names(tidy_dataset))
names(tidy_dataset)<-gsub("Acc", "Accelerometer", names(tidy_dataset))
names(tidy_dataset)<-gsub("BodyBody", "Body", names(tidy_dataset))
names(tidy_dataset)<-gsub("Mag", "Magnitude", names(tidy_dataset))
### END OF ITEM 4

###ITEM 5 - From the data set in step 4, creates a second, independent tidy data set
### with the average of each variable for each activity and each subject.
summarydata <- tidy_dataset %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))
write.table(summarydata, "summarydata.txt", row.name=FALSE)
### END OF ITEM 5
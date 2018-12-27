#Instructions to the project assignment:
##1 Merges the training and the test sets to create one data set.
##2 Extracts only the measurements on the mean and standard deviation for each measurement.
##3 Uses descriptive activity names to name the activities in the data set
##4 Appropriately labels the data set with descriptive variable names.
##5 From the data set in step 4, creates a second, independent tidy data set with the average 
##of each variable for each activity and each subject.

#Download Human Activity Recognition Using Smartphones Data Set 
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"
if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile)
}
path <- "C:/Users/gulna/Desktop/MachineL_DataScience/Data_Science3_Getting_and_CleaningData/UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}
#read files:
tr_subjects <- read.table(file.path(path, "train", "subject_train.txt"))
tr_values <- read.table(file.path(path, "train", "X_train.txt"))
tr_activity <- read.table(file.path(path, "train", "y_train.txt"))
ts_subjects <- read.table(file.path(path, "test", "subject_test.txt"))
ts_values <- read.table(file.path(path, "test", "X_test.txt"))
ts_activity <- read.table(file.path(path, "test", "y_test.txt"))
# read features and activity labels:
features <- read.table(file.path(path, "features.txt"), as.is = TRUE)
activities <- read.table(file.path(path, "activity_labels.txt"))
colnames(activities)
colnames(activities) <- c("Id", "Label")
# 1 Merge train and test datasets:
tr <- cbind(tr_subjects, tr_values, tr_activity)
ts <- cbind(ts_subjects, ts_values, ts_activity)
human_activity <- rbind(tr, ts)
# assign column names
colnames(human_activity)
colnames(human_activity) <- c("subject", features[, 2], "activity")
# 2 Extract only the measurements on the mean and standard deviation
newcolumns <- grepl("subject|activity|mean|std", colnames(human_activity))
human_activity <- human_activity[, newcolumns]
colnames(human_activity)
# 3 Use descriptive activity names to name the activities in the data
# replace activity values with named factor levels
human_activity$activity <- factor(human_activity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])
# 4 Appropriately label the data set with descriptive variable names
human_activityCols <- colnames(human_activity)
# Remove special characters and rename column names
human_activityCols <- gsub("[\\(\\)-]", "", human_activityCols)
human_activityCols <- gsub("^f", "frequencyDomain", human_activityCols)
human_activityCols <- gsub("^t", "timeDomain", human_activityCols)
human_activityCols <- gsub("Acc", "Accelerometer", human_activityCols)
human_activityCols <- gsub("Mag", "Magnitude", human_activityCols)
colnames(human_activity) <- human_activityCols
colnames(human_activity)
# 5 Create a second, independent tidy set with the average of each variable 
# group by subject and activity and summarise using mean
human_activityMeans <- human_activity %>% 
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
# output to file "tidy.txt"
write.table(human_activityMeans, "tidy.txt", row.names = FALSE)


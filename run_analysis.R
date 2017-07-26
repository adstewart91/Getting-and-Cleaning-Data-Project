## Andrew D. Stewart -- Final Project for Getting and Cleaning Data Course
## July 2017 -- adstewart91@hotmail.com


## Here are the data for the project:
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


## Script Assignment: You should create one R script called run_analysis.R 
## that does the following.

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with 
##    the average of each variable for each activity and each subject.

library(data.table)
library(dplyr)

## Function reads a dataframe from a provided file_name and returns a dplyr dataframe tbl
create_data_tbl <- function (file_name) {

        data <- read.table(file_name)
        data_tble <- data.table(data)
        data_tble <- tbl_df(data_tble)
}

## Create directory names:
test_directory <- c("UCI HAR Dataset/test/")    ## Directory location for Test Data
train_directory <- c("UCI HAR Dataset/train/")  ## Directory location for Train Data
data_directory <- c("UCI HAR Dataset/")         ## Base Directory location data

## Read Test Data, Training Data, and Labels (Total of 8 .txt Files):
x_test_data <- create_data_tbl(file.path(test_directory,"X_test.txt"))
y_test_data <- create_data_tbl(file.path(test_directory,"y_test.txt"))
subject_test_data <- create_data_tbl(file.path(test_directory,"subject_test.txt"))

x_train_data <- create_data_tbl(file.path(train_directory,"X_train.txt"))
y_train_data <- create_data_tbl(file.path(train_directory,"y_train.txt"))
subject_train_data <- create_data_tbl(file.path(train_directory,"subject_train.txt"))

activity_labels <- create_data_tbl(file.path(data_directory,"activity_labels.txt"))
data_labels <- create_data_tbl(file.path(data_directory,"features.txt"))

## Combine Test and Training Data by Rows
subjects_combined <- bind_rows(subject_test_data, subject_train_data)
y_data_combined <- bind_rows(y_test_data, y_train_data)
x_data_combined <- bind_rows(x_test_data, x_train_data)

## Update Data with descriptive column names
activity_labels <- rename(activity_labels, "acty_num" = "V1", "acty_name" = "V2")
subjects_combined <- rename(subjects_combined, "subject_num" = "V1")
y_data_combined <- rename(y_data_combined, "acty_num" = "V1")
x_data_combined <- rename_all(x_data_combined, funs(data_labels$V2))

## Bring Activity Numbers together with Activity Labels:
activity_combined <- merge(y_data_combined, activity_labels, key = acty_num, sort = FALSE)

## Column Bind All Data Into One Large Combined Dataset:
combined_dataset <- cbind(subjects_combined, activity_combined, x_data_combined)
str(combined_dataset)

## Total 17 measurements: (15) of tBodyAcc-XYZ and tBodyGyro-XYZ, (1) tBodyAccMag, & (1) tGravityAccMag
## 17 measurements over: (1) mean and (2) standard deviation = 34 columns to extract:

## First, Logical Vectors of columns with measurements named: "tBody" AND with: (mean or std): 
measure_means_vector <- grepl("^(tBody)", data_labels$V2) & grepl("mean",data_labels$V2)
measure_std_vector <- grepl("^(tBody)", data_labels$V2) & grepl("std",data_labels$V2)

## Second, Logical Vector columns with measurement named: "tGravity" AND with: (mean or std):
measure_gravity_vector_mean <- grepl("^(tGravityAccMag)", data_labels$V2) & grepl("mean",data_labels$V2)
measure_gravity_vector_std <- grepl("^(tGravityAccMag)", data_labels$V2) & grepl("std",data_labels$V2)

## Combine both logical vectors of columns and assemble measurment names to be extracted:
final_vector <- (measure_means_vector | measure_std_vector | measure_gravity_vector_mean | 
                         measure_gravity_vector_std) 
        ## OR combines mean and std columns together

final_datanames <- as.character(data_labels$V2[final_vector]) 
        ## character vector of measurement names

## Extract the subject number column and measurement columns:
final_extract <- tbl_df(combined_dataset[,c("subject_num", final_datanames)])

## Create Tidy Dataset grouped by subject number:
subject_group <- group_by(final_extract, subject_num) ## group by subject number
independent_tidydataset <- summarize_all(subject_group, funs(mean)) ## summarize with mean

## Tidy-Up column names of the results with read-able column names:
tidy_colnames <- c("subject number", "body acceleration mean-X", "body acceleration mean-Y",      
"body acceleration mean-Z", "body acceleration std dev-X", "body acceleration std dev-Y",      
"body acceleration std dev-Z", "body acceleration jerk mean-X", "body acceleration jerk mean-Y",
"body acceleration jerk mean-Z", "body acceleration jerk std dev-X", "body acceleration jerk std dev-Y", 
"body acceleration jerk std dev-Z", "body gyro mean-X", "body gyro mean-Y",    
"body gyro mean-Z", "body gyro std dev-X", "body gyro std dev-Y",      
"body gyro std dev-Z", "body gyro jerk mean-X", "body gyro jerk mean-Y", 
"body gyro jerk mean-Z", "body gyro jerk std dev-X", "body gyro jerk std dev-Y", 
"body gyro jerk std dev-Z", "body acceleration magnitude mean",
"body acceleration magnitude std dev", "gravity acceleration magnitude mean", 
"gravity acceleration magnitude std dev", "body accleration jerk magnitude mean", 
"body accleration jerk magnitude std dev", "body gyro magnitude mean", 
"body gyro magnitude std dev", "body gyro jerk magnitude mean",
"body gyro jerk magnitude std dev")

## Apply new tidy column names to the Tidy Dataset:
names(independent_tidydataset) <- tidy_colnames

print.table(independent_tidydataset)




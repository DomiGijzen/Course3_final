run_analysis <- function(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                         directory = "~/R/3.assignment"
                         ) {
## Install and load libraries
        install.packages("dplyr")
        library("dplyr")
        
        
## Prepare WD and get files
        ## Set working dir and download accelerometer data      
        setwd(directory)
        ## file is only downloaded and unzipped when it doesn't exist
        if (!file.exists("accel_data.zip")) {
                download.file(url, "accel_data.zip")
                unzip("accel_data.zip")
        }
## Read, bind, merge and name data
        ## Read activity and feature labels
        activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("activity", "activity_name")) 
        features <- read.table("UCI HAR Dataset/features.txt", col.names = c("feature", "feature_name"))
        ## Read activity lines
        y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "activity")
        y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "activity")
        ## Merge obseration activities with labels
        activity_test <- merge(y_test, activity_labels)
        activity_train <- merge(y_train, activity_labels)
        ## Read subject lines
        subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
        subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
        ## Read actual test and train set as variables including column names from feature lables
        X_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature_name)
        X_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature_name)
        ## Bind the subjects, activities and obsservations
        testset <- cbind(subject_test, activity_test, X_test)
        trainset <- cbind(subject_train, activity_train, X_train)
        ##Bind test and train set to total set
        totalset <- rbind(trainset, testset)
## Extract mean and sdev per observation
        extractset <- totalset[c(1:3, grep("mean|std", names(totalset)))]
## Calculate average per subject and activity (combination)
        grouped_set <- group_by(extractset, subject, activity, activity_name)
        summarised_set <- summarise_each(grouped_set, funs(mean))
        colnames(summarised_set)[4:82] <- paste("Mean_of", colnames(summarised_set[,c(4:82)]), sep="_")
## Write the table for upload
        write.table(summarised_set, file="upload_table.txt",row.name=FALSE)
        }